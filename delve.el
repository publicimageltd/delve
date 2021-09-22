;;; delve.el --- Delve into the depths of your org roam zettelkasten       -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021

;; Author:  <joerg@joergvolbers.de>
;; Version: 0.9
;; Package-Requires: ((emacs "26.1") (org-roam "2.1") (lister "0.9.1"))
;;
;; Keywords: hypermedia, org-roam
;; URL: https://github.com/publicimageltd/delve

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Delve into the depths of your zettelkasten.

;;; Code:

;;; TODO Find word for "inserting something as a sublist below"
;;; TODO Add function to insert a node from an org roam buffer
;;; TODO Add function to insert backlinks below point
;;; TODO Add function to insert fromlinks below point
;;; TODO ? Add query to insert tagged nodes, with "limit" value (>100)

;; * Dependencies

(require 'org-roam)
(require 'seq)
(require 'lister)
(require 'lister-mode)
(require 'button)
(require 'delve-query)
(require 'delve-pp)

;; * Silence Byte Compiler

(declare-function all-the-icons-faicon "all-the-icons" (string) t)

;;; * Global Variables

(defvar delve-version "0.9"
  "Current version of delve.")

(defvar delve--no-icons nil
  "If bound, do not use any icons when creating output.")

;;; * Data Types

;; Each list item has to be derived from this unspecific structure:
(cl-defstruct (delve--item (:constructor delve--item-create))
  "A generic single delve list item.")

(cl-defstruct (delve--zettel
            (:include delve--item)
            (:constructor delve--zettel-create (node)))
  "A Zettel item storing an org roam node."
  node)

;; Some shortcuts to the node element of a zettel:
(defmacro zettel--accessor-fn (name slot-name)
  "Define an accessor function for the node in a Zettel item.
SLOT-NAME must be the name of a slot of an org-roam-node.  Give
  the function the name NAME."
  `(defun ,name (z)
     ,(format "Access the slot %s of the node object stored in a Zettel item." slot-name)
     (cl-struct-slot-value 'org-roam-node ,slot-name (delve--zettel-node z))))

(zettel--accessor-fn delve--zettel-title   'title)
(zettel--accessor-fn delve--zettel-id      'id)
(zettel--accessor-fn delve--zettel-file    'file)
(zettel--accessor-fn delve--zettel-tags    'tags)
(zettel--accessor-fn delve--zettel-level   'level)
(zettel--accessor-fn delve--zettel-aliases 'aliases)
(zettel--accessor-fn delve--zettel-mtime   'file-mtime)
(zettel--accessor-fn delve--zettel-atime   'file-atime)

(cl-defstruct (delve--pile
            (:include delve--item)
            (:constructor delve--pile-create))
  "A pile (list) of Zettels."
  name zettels)

;; TODO Somehow differentiate zettel queries and queries yielding
;; other results, e.g. browsable tag lists.
(cl-defstruct (delve--query
            (:include delve--item)
            (:constructor delve--query-create))
  "An SQL query returning zettel objects."
  name query)

;; * Faces

(defface delve-tags-face
  '((t (:inherit org-tag)))
  "Face for displaying roam tags in a Delve list."
  :group 'delve)

(defface delve-title-face
  '((t (:inherit org-document-title)))
  "Face for displaying org roam page titles in a Delve list."
  :group 'delve)

(defface delve-pile-name-face
  '((t (:inherit org-document-title)))
  "Face for displaying the name of a Zettel pile."
  :group 'delve)

(defface delve-subtype-face
  '((t (:inherit font-lock-constant-face)))
  "Face for displaying the subtype of a Delve item."
  :group 'delve)

(defface delve-mtime-face
  '((t (:inherit org-document-info-keyword)))
  "Face for displaying the mtime of a Delve item."
  :group 'delve)

(defface delve-atime-face
  '((t (:inherit org-document-info-keyword)))
  "Face for displaying the atime of a Delve item."
  :group 'delve)

(defface delve-ctime-face
  '((t (:inherit org-document-info-keyword)))
  "Face for displaying the ctime of a Delve item."
  :group 'delve)

(defface delve-nbacklinks-face
  '((t (:weight bold)))
  "Face for displaying the number of backlinks to a Delve zettel."
  :group 'delve)

(defface delve-ntolinks-face
  '((t (:weight bold)))
  "Face for displaying the number of tolinks to a Delve zettel."
  :group 'delve)

(defface delve-pile-face
  '((t (:inherit org-level-1)))
  "Face for displaying the title of a Delve pile."
  :group 'delve)

(defface delve-query-face
  '((t (:inherit org-level-2)))
  "Face for displaying the title of a Delve query."
  :group 'delve)


;;; * The Lister Mapper

;; Printing anything

(defun delve--type-as-string (delve-item)
  "Return a string or icon representing the type of DELVE-ITEM.
If the global variable `delve--no-icons' is bound, always only
return strings."
  (let* ((representation
          (pcase (type-of delve-item)
            (`delve--query  (list "QUERY" "search"))
            (`delve--pile   (list "PILE"  "list-ul"))
            (`delve--zettel
             (if (eq 0 (delve--zettel-level delve-item))
                 (list "FILE" "file-text-o")
               (list "NODE" "dot-circle-o")))
            (_              (list "TYPE?" "question"))))
         (type-as-string (cl-first representation))
         (icon-name      (cl-second representation)))
    (if (and (featurep 'all-the-icons)
             (not delve--no-icons))
        (concat (all-the-icons-faicon icon-name) " ")
      (delve-pp--set-width type-as-string 6))))

;; Printing Zettel

(defun delve--node-title (node)
  "Return the title of NODE."
  (or (org-roam-node-title node)
      (org-roam-node-file node)
      "No file or title"))

(defun delve--get-button (label &rest properties)
  "Make a button LABEL in a temporary buffer and return it as a string.
Pass PROPERTIES to `insert-text-button', which creates the
button."
  (declare (indent 1))
  (with-temp-buffer
    (apply #'insert-text-button label properties)
    (buffer-string)))

(defun delve--tag-button (tag)
  "Return TAG as a button object."
  (delve--get-button tag
    'action (lambda (_) (message "Do something with %s" tag))))

(defun delve--string-join (strings &optional separator prefix)
  "Join all non-nil strings in STRINGS using SEPARATOR.
Optionally add string PREFIX to each non-nil item."
  (let ((strings (lister--flatten strings)))
    (when prefix
      (setq strings (mapcar (apply-partially #'concat prefix) strings)))
    (string-join strings separator)))

;; TODO Change that to buttons which open all links with that tag in a
;; new buffer?
(defun delve--tags-as-string (node)
  "Return the NODE's tags as a list of strings."
  (if-let ((tags (org-roam-node-tags node)))
      (delve--string-join tags ", " "#")
    "No tags."))

(defun delve--zettel-strings (zettel)
  "Return a list of strings representing ZETTEL."
  (let ((node (delve--zettel-node zettel)))
    (list
     (delve-pp-fields node '((org-roam-node-title   (:set-face delve-title-face))))
     (delve-pp-fields node '((delve--tags-as-string (:add-face delve-tags-face)))))))

;; Printing a pile item

(defun delve--pile-size (pile)
  "Return information of the size of PILE."
  (format "(%d)" (length (delve--pile-zettels pile))))

(defun delve--pile-strings (pile)
  "Return a list of strings representing PILE."
  (list
   (delve-pp-fields pile '((delve--pile-size     (:set-face delve-pile-face))
                           (delve--pile-name     (:set-face delve-pile-face))))))

;; The actual mapper

(defun delve-mapper (item)
  "Transform ITEM into a list of printable strings."
  (let* ((typestring  (delve--type-as-string item))
         (datastrings (cl-typecase item
                        (delve--zettel (delve--zettel-strings item))
                        (delve--pile   (delve--pile-strings item))
                        (t (list "no printer available for that item type")))))
    ;; hanging indent:
    (let* ((pad        (make-string (length typestring) ? ))
           (first-line (concat typestring (car datastrings)))
           (rest-lines (mapcar (apply-partially #'concat pad)
                               (cdr datastrings))))
      (apply #'list first-line rest-lines))))

;;; * Keys

;;; Generic stuff

(defun delve--push-to-global-mark-ring ()
  "Push current point on the global mark ring."
  (add-to-history 'global-mark-ring (copy-marker (point-marker)) global-mark-ring-max t))

(defun delve--current-item (&optional type no-error)
  "Get the item bound to the current Lister node.
If the item is not of type TYPE, throw an error.  If NO-ERROR is
non-nil, no not throw an error and return nil instead.  Skip
any typechecking if TYPE is nil."
  (unless lister-local-ewoc
    (error "Command must be called in a lister buffer"))
  (let ((item (lister-get-data-at lister-local-ewoc :point)))
    (if (or (not type)
            (eq (type-of item) type))
        item
      (unless no-error
        (error "The item at point is not of the right type for that command")))))

;;; Insert node(s)

(defun delve-key-insert-node ()
  "Interactively add NODE to current buffer's Delve list."
  (let ((node (org-roam-node-read)))
    (lister-insert-at lister-local-ewoc :point
                      (delve--zettel-create node))))

;;; Visit thing

(defun delve-key-visit-zettel (z)
  "Find the zettel Z in a (new) buffer."
  (interactive (list (delve--current-item 'delve--zettel)))
  (delve--push-to-global-mark-ring)
  (with-current-buffer (org-roam-node-visit (delve--zettel-node z))
    (org-show-entry)))

(defun delve--new-buffer (name &optional initial-list)
  "Create a new delve buffer NAME with INITIAL-LIST.
Return the buffer object."
  (let ((buf (generate-new-buffer name)))
    (with-current-buffer buf
      (delve-mode)
      (lister-setup buf #'delve-mapper name)
      (lister-mode)
      (when initial-list
        (lister-set-list lister-local-ewoc initial-list)))
    buf))

(defun delve-key-visit-pile (pile)
  "Open PILE at point in a new Delve buffer."
  (let* ((name (format "DELVE Pile: %s" (delve--pile-name pile)))
         (zettels (delve--pile-zettels pile)))
    (unless zettels
      (error "Pile is empty"))
    (let ((buf (get-buffer name)))
      (if buf
          ;; TODO add "new" items to existing buffer
          (message "Leaving existing buffer unchanged; no items added")
        (setq buf (delve--new-buffer name zettels)))
      (switch-to-buffer buf))))

(defun delve-visit-marked ()
  "Visit all marked items in a new buffer."
  (let (acc)
    (lister-walk-marked-nodes lister-local-ewoc
                              (lambda (_ewoc node)
                                (push (lister-node-get-data node) acc)))
    (let ((buf (delve--new-buffer
                (format-time-string "DELVE Items collected at %X")
                (nreverse acc))))
      (switch-to-buffer buf))))

(defun delve-key-visit (item)
  "Visit the ITEM at point."
  (interactive (list (delve--current-item)))
  (if (lister-items-marked-p lister-local-ewoc)
      (delve-visit-marked)
    (cl-typecase item
      (delve--zettel  (delve-key-visit-zettel item))
      (delve--pile    (delve-key-visit-pile   item))
      (t              (error "No visit action defined for this item")))))

;;; Pile Zettels

(defun delve--stuff-into-pile (ewoc pile)
  "In EWOC, stuff all marked nodes in PILE.
Return the PILE object."
  (cl-labels ((push-on-pile (ewoc node)
                            (let ((item (lister-node-get-data node)))
                              (when (eq (type-of item) 'delve--zettel)
                                (push item (delve--pile-zettels pile))
                                (lister-delete-at ewoc node)))))
    (lister-walk-marked-nodes ewoc #'push-on-pile))
    ;; uniqify the pile zettels:
    (setf (delve--pile-zettels pile)
          (seq-uniq (delve--pile-zettels pile)))
    pile)

(defun delve--move-into-pile-at (ewoc pos)
  "In EWOC, move all marked nodes into the pile at POS.
Throw an error if there is no pile at POS, or if PILE is marked."
  (let ((pile (lister-get-data-at ewoc pos)))
    (unless (eq 'delve--pile (type-of pile))
      (error "Item is not a pile"))
    (when (lister-marked-at-p ewoc pos)
      (error "Cannot move pile in itself"))
    (delve--stuff-into-pile ewoc pile)
    (lister-refresh-at ewoc pos)))

(defun delve-key-pile ()
  "Collect all marked Zettel items in a new pile and insert it.
If point is on a pile item, add to this pile instead.  Remove any
duplicates in the final pile.  Skip non-zettel items when
collecting."
  (interactive)
  (let* ((ewoc    lister-local-ewoc)
         (current (lister-get-data-at ewoc :point)))
    (unless (lister-items-marked-p ewoc)
      (error "No items marked"))
    ;; insert empty pile at point if there is none:
    (unless (eq 'delve--pile (type-of current))
      (let ((name (read-string "Name for the new pile: ")))
        (setq current (delve--pile-create :name name))
        (lister-insert-at ewoc :point current)))
    ;; now stuff it:
    (delve--move-into-pile-at ewoc :point)
    ;; TODO Once "lister-walk-marked" returns a count, use this to give
    ;; feedback how many items have been moved
    (lister-refresh-at ewoc :point)))

(defun delve-key-insert-pile (pile)
  "Insert PILE as a sublist below point."
  (interactive (list (delve--current-item 'delve--pile)))
  (let ((zettels (delve--pile-zettels pile)))
    (unless zettels (error "Pile is empty"))
    (lister-insert-sublist-below lister-local-ewoc :point zettels)))

;;; Delete Items

(defun delve-key-delete ()
  "Delete all marked items or the single ITEM at point.
If the single item to be deleted has a sublist, also decrease the
indentation of these items."
  (interactive)
  (let* ((ewoc         lister-local-ewoc)
         (sublist-beg  (and (lister-sublist-below-p ewoc :point)
                            (lister-get-node-at ewoc :next)))
         (marked-n     (lister-count-marked-items ewoc))
         (top-deleted? nil))
    ;;
    (if (> marked-n 0)
        ;; delete the marked items:
        (when (y-or-n-p (format "Delete %d marked items? "
                                marked-n))
          (lister-delete-marked-list ewoc))
      ;; else delete the top item at point and maybe its sublist:
      (lister-mark-unmark-at ewoc :point t)
      (if (setq top-deleted? (y-or-n-p "Delete this item? "))
          (lister-delete-at ewoc :point)
        (lister-mark-unmark-at ewoc :point nil))
      ;; now maybe delete the sublist:
      (when sublist-beg
        (lister-mark-unmark-sublist-at ewoc sublist-beg t)
        (if (y-or-n-p "Delete the sublist? ")
            (lister-delete-sublist-at ewoc sublist-beg)
          ;; realign remaining sublist since top has been deleted:
          (when top-deleted?
            (lister-walk-marked-nodes ewoc #'lister-move-item-left))
          (lister-mark-unmark-sublist-at ewoc sublist-beg nil))))))

;;; Multiple action keys

(defun delve-key-plus ()
  "Pile marked items or insert a new one."
  (interactive)
  (if (lister-items-marked-p lister-local-ewoc)
      (delve-key-pile)
    (delve-key-insert-node)))

(defun delve-key-ret (item)
  "Do something with the ITEM at point."
  (interactive (list (delve--current-item)))
  (cl-typecase item
    (delve--zettel  (delve-key-visit-zettel item))
    (delve--pile    (delve-key-insert-pile   item))
    (t              (error "No action defined for this item"))))

;;; * Delve Major Mode

(defvar delve-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<delete>")   #'delve-key-delete)
    (define-key map (kbd "<RET>")      #'delve-key-ret)
    (define-key map (kbd "v")          #'delve-key-visit)
    (define-key map (kbd "+")          #'delve-key-plus)
    map)
  "Key map for `delve-mode'.")

(define-derived-mode delve-mode
  fundamental-mode "Delve"
  "Major mode for browsing your org roam zettelkasten."
  (lister-setup	(current-buffer) #'delve-mapper
               (concat "DELVE Version " delve-version))
  (lister-mode))

(defun delve-test ()
  "BLABLA."
  (interactive)
  (let ((buf (generate-new-buffer "DELVE")))
    (switch-to-buffer buf)
    (with-current-buffer buf
      (delve-mode)
      (let* ((ewoc lister-local-ewoc)
             (allnodes   (delve-query-node-list))
             (nodes      (seq-take allnodes 10))
             (zettel     (mapcar #'delve--zettel-create nodes))
             (pilenodes  (seq-subseq allnodes 10 15))
             (pilezettel (mapcar #'delve--zettel-create pilenodes)))
        (lister-set-list ewoc zettel)
        (lister-insert-at ewoc :first (delve--pile-create :name "Ein Haufen Zettel!"
                                                          :zettels pilezettel))))))

(provide 'delve)
;;; delve.el ends here
