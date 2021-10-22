;;; delve.el --- Delve into the depths of your org roam zettelkasten       -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021

;; Author:  <joerg@joergvolbers.de>
;; Version: 0.9

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

;; Terminology:
;;
;; - A "collection" is a list of Zettels, which can be
;; stored in a file.  A buffer thus 'visits' a collection (or creates
;; one without associated file.)

;;; Code:

;;; TODO Exclude dashboard buffer

;; * Dependencies

(require 'org-roam)
(require 'seq)
(require 'lister)
(require 'lister-mode)
(require 'button)
(require 'delve-data-types)
(require 'delve-query)
(require 'delve-pp)
(require 'delve-store)
(require 'delve-edit)

;; * Silence Byte Compiler

(declare-function all-the-icons-faicon "all-the-icons" (string) t)
(declare-function consult-completing-read-multiple "consult")
(defvar org-roam-node-read--cached-display-format)

;;; * Global Variables

(defvar delve-version "0.9"
  "Current version of delve.")

(defvar delve--no-icons nil
  "If bound, do not use any icons when creating output.")

(defvar delve-dashboard-name "Dashboard"
  "Name of the dashboard buffer.")

(defvar delve--select-history nil
  "History for `delve--select-collection-buffer'.")

(defvar delve--last-selected-buffer nil
  "Last buffer selected with `delve--select-collection-buffer'.")

(defcustom delve-store-directory (concat (file-name-directory user-emacs-directory)
                                         "delve-store")
  "Path to a default directory for storing Delve buffers in."
  :group 'delve
  :type  'directory)

;; * Buffer Local Variables

(defvar-local delve-local-storage-file nil
  "Associated local storage file.")

(defun delve-get-storage-file (buf)
  "Get the buffer local storage file for BUF."
  (buffer-local-value 'delve-local-storage-file buf))

(defvar-local delve-local-header-info "DELVE"
  "First line of the local Lister header.")

;; * Faces

(defface delve-header-face
  '((t (:inherit org-document-title)))
  "Face for displaying the header of a Delve list."
  :group 'delve)

(defface delve-note-face
  '((t (:inherit font-lock-comment-face)))
  "Face for displaying note items in a Delve list."
  :group 'delve)

(defface delve-info-face
  '((t (:inherit warning)))
  "Face for displaying info items in a Delve list."
  :group 'delve)

(defface delve-tags-face
  '((t (:inherit org-tag)))
  "Face for displaying roam tags in a Delve list."
  :group 'delve)

(defface delve-title-face
  '((t (:inherit org-roam-title)))
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

;; Type icon

(defun delve--type-as-string (delve-item)
  "Return a string or icon representing the type of DELVE-ITEM.
If the global variable `delve--no-icons' is bound, always only
return strings."
  (pcase-let ((`(,s ,icon-name)
              (pcase (type-of delve-item)
                ('delve--query   (list "QUERY" "search"))
                ('delve--pile    (list "PILE"  "list-ul"))
                ('delve--info    (list "INFO"  "info"))
                ('delve--note    (list "NOTE"  "pencil"))
                ('delve--zettel
                 (if (eq 0 (delve--zettel-level delve-item))
                     (list "FILE" "file-text-o")
                   (list "NODE" "dot-circle-o")))
                (_              (list "TYPE?" "question")))))
    (if (and (featurep 'all-the-icons)
             (not delve--no-icons))
        (concat (all-the-icons-faicon icon-name) " ")
      (delve-pp--set-width s 6))))

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

(defun delve--insert-by-id (id)
  "In current Delve buffer, insert Zettel by ID after point."
  (if-let ((node (delve-query-node-by-id id)))
      (lister-insert-at lister-local-ewoc :point
                        (delve--zettel-create node)
                        nil t)
    (user-error "No node with this ID found")))

(defun delve--buttonize-link (link)
  "In an org mode buffer, replace element LINK with a button.
LINK has to be a LINK element as returned by
`org-element-parse-buffer'."
  (when (equal (org-element-property :type link) "id")
    (let* ((beg (org-element-property :begin link))
           (end (org-element-property :end link))
           (id  (org-element-property :path link))
           (blanks (org-element-property :post-blank link))
           (label (buffer-substring-no-properties
                   (org-element-property :contents-begin link)
                   (org-element-property :contents-end link))))
      (delete-region beg (- end blanks))
      (goto-char beg)
      (insert-text-button
       label
       'follow-link t
       'action (lambda (_)
                 (org-link-open link))
       'keymap (let ((map (make-sparse-keymap)))
                 (set-keymap-parent map button-map)
                 (define-key map "+"
                   (lambda ()
                     (interactive)
                     (delve--insert-by-id id)))
                 map)))))

(defun delve--prepare-preview (s)
  "Prepare preview string S for insertion.
Return the prepared string."
  (with-temp-buffer
    (insert s)
    (let ((org-inhibit-startup nil))
      (org-mode)
      (org-font-lock-ensure)
      (let ((tree (org-element-parse-buffer)))
        (org-element-map tree 'link
          #'delve--buttonize-link))
      (buffer-string))))

(defun delve--zettel-strings (zettel)
  "Return a list of strings representing ZETTEL."
  (let ((node (delve--zettel-node zettel)))
    (list
     ;; Display node:
     (delve-pp-fields node '((org-roam-node-title   (:add-face delve-title-face))))
     (delve-pp-fields node '((delve--tags-as-string (:add-face delve-tags-face))))
     ;; additional Zettel slots:
     (when-let ((preview (delve--zettel-preview zettel)))
       (split-string (delve--prepare-preview preview) "\n")))))

;; Printing a pile item

(defun delve--pile-size (pile)
  "Return information of the size of PILE."
  (format "(%d)" (length (delve--pile-zettels pile))))

(defun delve--pile-strings (pile)
  "Return a list of strings representing PILE."
  (list
   (delve-pp-fields pile '((delve--pile-size     (:set-face delve-pile-face))
                           (delve--pile-name     (:set-face delve-pile-face))))))

;; Printing Queries

(defun delve--query-strings (query)
  "Return a list of strings representing QUERY."
  (list (delve--query-info query)))

;; Printing Notes

(defun delve--note-s-to-list (string)
  "Format STRING as a paragraph and return it as a list of strings."
  (split-string
   (with-temp-buffer
     (insert "  " string)
     (goto-char (point-min))
     (fill-paragraph)
     (buffer-string))
    "\n"))

(defun delve--note-strings (note)
  "Return a list of strings representing NOTE."
  (delve--note-s-to-list
     (delve-pp--add-face (delve--note-text note) 'delve-note-face)))

(defun delve--info-strings (info)
  "Return a list of strings representing INFO."
  (delve--note-s-to-list
     (delve-pp--add-face (delve--note-text info) 'delve-info-face)))

;; The actual mapper

(defun delve-mapper (item)
  "Transform ITEM into a list of printable strings."
  (let* ((typestring  (delve--type-as-string item))
         (datastrings (cl-typecase item
                        (delve--zettel  (delve--zettel-strings item))
                        (delve--pile    (delve--pile-strings item))
                        (delve--info    (delve--info-strings item))
                        (delve--note    (delve--note-strings item))
                        (delve--query   (delve--query-strings item))
                        (t (list "no printer available for that item type")))))
    ;; hanging indent:
    (let* ((datastrings (lister--flatten datastrings))
           (pad         (make-string (length typestring) ? ))
           (first-line  (concat typestring (car datastrings)))
           (rest-lines  (mapcar (apply-partially #'concat pad)
                                (cdr datastrings))))
      (apply #'list first-line rest-lines))))

;; * Dynamic Header

(defun delve--db-info ()
  "Return strings with some basic infos."
  (let* ((nnodes  (caar (delve-query [:select (funcall count *) :from nodes])))
         (n0nodes (caar (delve-query [:select (funcall count *) :from nodes :where (= level 0)])))
         (tags    (caar (delve-query [:select :distinct tag :from tags :order :by asc]))))
    (list
     (propertize (format "Current db has %d nodes in %d files. %d tags are in use."
                         nnodes n0nodes (length tags))
                 'face 'font-lock-comment-face))))

(defun delve--header-function ()
  "Generate a Lister header item from local buffer vars."
  (lister--flatten
   (list (propertize delve-local-header-info 'face 'delve-header-face)
         (delve--db-info)
         (when delve-local-storage-file
           (propertize delve-local-storage-file 'face 'font-lock-string-face)))))

;; * Buffer and buffer-as-storage handling

(defun delve--storage-files ()
  "Return all storage file names.
Only return the file name relative to `delve-store-directory'."
  (directory-files delve-store-directory nil (rx string-start (not "."))))

(defun delve--create-buffer-name (name)
  "Create a name for a Delve buffer using NAME."
  (concat "DELVE " name))

(defun delve--new-buffer (name &optional initial-list)
  "Create a new delve buffer NAME with INITIAL-LIST.
Return the buffer object."
  (let* ((name (delve--create-buffer-name name))
         (buf (generate-new-buffer name)))
    (with-current-buffer buf
      (delve-mode)
      (setf delve-local-header-info name)
      (lister-setup buf #'delve-mapper #'delve--header-function)
      (lister-mode)
      (when initial-list
        (lister-set-list lister-local-ewoc initial-list)))
    buf))

(defun delve--create-tag-query (tags)
  "Create a item object searching for nodes matching TAGS."
  (let* ((tags (if (listp tags) tags (list tags))))
    (delve--query-create :info (format "Query for nodes matching %s"
                                       (delve--string-join tags " and " "#"))
                         :fn (lambda ()
                               (delve-query-nodes-by-tags tags)))))

(defun delve--new-dashboard ()
  "Create a new Delve dashboard buffer."
  (with-temp-message "Setting up dashboard..."
    (let* ((tags (delve-query-tags))
           (stores nil))
      (cl-dolist (tag (nreverse tags))
        (push (delve--create-tag-query tag) stores))
      (delve--new-buffer delve-dashboard-name
                         (append stores)))))

(defun delve--dashboard-p (&optional buf)
  "Check if BUF is the Delve dashboard."
  (string= (delve--create-buffer-name delve-dashboard-name)
           (buffer-name buf)))

(defun delve--dashboard-buf ()
  "Return the dashboard buffer, if existing."
  (seq-find #'delve--dashboard-p (buffer-list)))

(defun delve--buffer-p (&optional buf)
  "Check if BUF is a Delve buffer."
  (and (eq (buffer-local-value 'major-mode (or buf (current-buffer)))
           'delve-mode)))

;; TODO Seems not to be needed, remove it
(defun delve--buffer-and-not-dashboard-p (&optional buf)
  "Check if BUF is Delve buffer and not the dashboard."
  (and (delve--buffer-p buf)
       (not (string= (buffer-name buf) delve-dashboard-name))))

(defun delve-buffer-list ()
  "Return a list of all Delve buffers."
  (seq-filter #'delve--buffer-p (buffer-list)))

(defun delve-unopened-storages ()
  "Return all Delve storage files which are not visited yet.
Only return the file names relative to `delve-store-directory'"
  ;; FIXME This won't work currently as desired if the local file path
  ;; is outside of the delve store directory.
  (thread-last (delve-buffer-list)
    (mapcar          #'delve-get-storage-file)
    (seq-filter      #'identity)
    (mapcar          #'file-name-nondirectory)
    (seq-difference  (delve--storage-files))))

(defun delve--prepare-candidates (cand key-fn suffix)
  "Return list CAND as an alist with a string key.
Use KEY-FN to create the string key.  It will have SUFFIX added
to the end, in parentheses."
  (seq-group-by (lambda (elt)
                  (format "%s (%s)" (funcall key-fn elt) suffix))
                cand))

(defun delve--select-collection-buffer (prompt)
  "Select Delve buffer, collection, or create a new buffer.
Use PROMPT as a prompt to prompt the user to choose promptly."
  (let* ((buffer-suffix     "Switch to buffer")
         (buffer-alist      (delve--prepare-candidates  (delve-buffer-list)
                                                        #'buffer-name
                                                        buffer-suffix))
         (collection-suffix "Read into new buffer")
         (collection-alist  (delve--prepare-candidates (delve-unopened-storages)
                                                       #'identity
                                                       collection-suffix))
         (dashboard-suffix  "Create")
         (dashboard-alist   (delve--prepare-candidates '("New Dashboard")
                                                       #'identity
                                                       dashboard-suffix))
         (alist             (append dashboard-alist buffer-alist collection-alist))
         (new-name          (completing-read prompt alist
                                             nil nil nil
                                             'delve--select-history)))
    (setq delve--last-selected-buffer
          (if-let ((result (car (alist-get new-name alist nil nil #'string=))))
              (pcase new-name
                ;; We could also extract the string and then compare,
                ;; but I had always wanted to use the rx matcher!
                ((rx "(" (literal buffer-suffix) ")" string-end)
                 result)
                ((rx "(" (literal collection-suffix) ")" string-end)
                 (delve--read-storage-file result))
                ((rx "(" (literal dashboard-suffix) ")" string-end)
                 (delve--new-dashboard))
                (_                   (error "Something went wrong")))
            (delve--new-buffer new-name)))))

(defun delve--add-to-buffer (l)
  "Add L to a Delve buffer and return that buffer object.
Let the user select or create a buffer."
  (let ((buf (delve--select-collection-buffer " Add to buffer or collection: ")))
    (lister-add-list (with-current-buffer buf lister-local-ewoc)
                     l)
    buf))

(defun delve-kill-all-delve-buffers ()
  "Kill all Delve buffers."
  (interactive)
  (seq-do #'kill-buffer (delve-buffer-list)))

;;; * Remote Editing - Background Utilites

(defun delve--sync-zettel (zettels)
  "Force sync of all ZETTELS with the org roam db.
First update the db, then reload the ZETTELS."
  (let* ((filelist (mapcar #'delve--zettel-file zettels)))
    (cl-dolist (file (seq-uniq filelist #'string=))
      (org-roam-db-update-file file))
    (cl-dolist (z zettels)
      (setf (delve--zettel-node z)
            (delve-query-node-by-id (delve--zettel-id z))))))

;; TODO Test sync delve--zettel <-> DB
(defun delve--sync-marked (ewoc)
  "Force sync all marked list items of EWOC."
  (let ((nodes (lister-collect-nodes ewoc nil nil
                                     #'lister-node-marked-p)))
    (delve--sync-zettel (mapcar #'lister-node-get-data nodes))
    (cl-dolist (n nodes)
      (setf (lister--item-marked (ewoc-data n)) nil))
    (ewoc-invalidate ewoc nodes)))

;;; * Key handling / Commands

;;; Generic key related stuff

(defun delve--push-to-global-mark-ring ()
  "Push current point on the global mark ring."
  (add-to-history 'global-mark-ring (copy-marker (point-marker)) global-mark-ring-max t))

(defun delve--type-p (item &rest types)
  "Check if ITEM is one of TYPES."
  (memq (type-of item) types))

(defun delve--current-item (&optional types ewoc pos)
  "Get the item bound to the current Lister node.
TYPES is a type symbol or a list of type symbols.  If the item is
not of type TYPES, throw an error.  Use the position at point in
EWOC or POS, if supplied.  Skip any typechecking if TYPES is nil."
  (let ((ewoc (or ewoc lister-local-ewoc)))
    (unless ewoc
      (error "Command must be called in a Delve buffer"))
    (let ((item (lister-get-data-at ewoc (or pos :point))))
      (if (or (not types)
              (apply #'delve--type-p item (if (listp types) types (list types))))
          item
        (error "The item at point is not of the right types for that command")))))

(defun delve--insert-or-open-zettels (zettels &optional prefix as-sublist)
  "Insert ZETTELS in the current Delve buffer, at point.
If AS-SUBLIST is non-nil, insert as a sublist below point.  If
called with PREFIX, insert ZETTEL in a new Delve buffer instead."
  (if prefix
      (switch-to-buffer (delve--add-to-buffer zettels))
    ;; TODO Warn when list is too big
    (if as-sublist
        (lister-insert-sublist-below lister-local-ewoc
                                     :point
                                     zettels)
      (lister-insert-list-at lister-local-ewoc
                             :point
                             zettels
                             nil
                             (lister-eolp)))
    (message "Inserted %d Zettels" (length zettels))))

;;; * Key commands working with the "item at point"

;; TODO Write a transient for more comfy editing
;; TODO Test function interactively
;; TODO Handle syncing
(defun delve--key--add-tags (zettel)
  "Add the tags of the ZETTEL at point."
  (interactive (list (delve--current-item 'delve--zettel)))
  (delve-edit--add-tags zettel))

;; TODO Test function interactively
;; TODO Handle syncing
(defun delve--key--remove-tags (zettel)
  "Remove the tags of the ZETTEL at point."
  (interactive (list (delve--current-item 'delve--zettel)))
  (delve-edit--remove-tags zettel))

(defun delve--key--open-zettel (zettel)
  "Open the ZETTEL at point."
  (interactive (list (delve--current-item 'delve--zettel)))
  (delve--push-to-global-mark-ring)
  (with-current-buffer (org-roam-node-visit (delve--zettel-node zettel))
    (org-show-entry)))

(defun delve--key--backlinks (zettel &optional prefix)
  "Insert backlinks of current ZETTEL.
With PREFIX, open link list in a new buffer, else insert it as a
sublist below point."
  (interactive (list (delve--current-item 'delve--zettel) current-prefix-arg))
  (if-let ((nodes (delve-query-backlinks-by-id (delve--zettel-id zettel))))
      (delve--insert-or-open-zettels
       (mapcar #'delve--zettel-create nodes)
       prefix
       :as-sublist)
    (message "No backlinks to this zettel node")))

(defun delve--key--fromlinks (zettel &optional prefix)
  "Insert fromlinks of current ZETTEL.
With PREFIX, open link list in a new buffer, else insert it as a
sublist below point."
  (interactive (list (delve--current-item 'delve--zettel) current-prefix-arg))
  (if-let ((nodes (delve-query-fromlinks-by-id (delve--zettel-id zettel))))
      (delve--insert-or-open-zettels
       (mapcar #'delve--zettel-create nodes)
       prefix
       :as-sublist)
    (message "No tolinks to this zettel node")))

(defun delve--key--insert-query-or-pile (item &optional prefix)
  "Insert results from ITEM, either a query or a pile object.
With PREFIX, insert results in a new buffer, else insert it as a
sublist below point."
  (interactive (list (delve--current-item '(delve--query delve--pile))
                     current-prefix-arg))
  (let ((pile-node (lister-get-node-at lister-local-ewoc :point))
        zettels insertion-type)
    (cl-typecase item
      (delve--pile  (setq zettels (delve--pile-zettels item)
                          insertion-type nil))
      (delve--query (setq zettels (mapcar #'delve--zettel-create (funcall (delve--query-fn item)))
                          insertion-type :as-sublist)))
    (if (null zettels)
        (message "No matching zettels found")
      (delve--insert-or-open-zettels zettels prefix insertion-type)
      (unless prefix
        (if (delve--type-p item 'delve--pile)
            (lister-delete-at lister-local-ewoc pile-node))))))

(defun delve--get-preview-contents (zettel)
  "Get the raw preview contents for ZETTEL."
  (org-roam-with-temp-buffer (delve--zettel-file zettel)
    (org-roam-preview-get-contents
     (delve--zettel-file zettel)
     (if (eq (delve--zettel-level zettel) 0)
         (if (org-goto-first-child)
             (1- (point))
           (point-max))
       (delve--zettel-point zettel)))))

(defun delve--key--toggle-preview (zettel &optional prefix)
  "Toggle the display of the preview of ZETTEL.
With PREFIX, open the ZETTEL in a buffer."
  (interactive (list (delve--current-item 'delve--zettel)
                     current-prefix-arg))
  (if prefix
      (delve--key--open-zettel zettel)
    (let ((preview (and (not (delve--zettel-preview zettel))
                        (or (delve--get-preview-contents zettel)
                            "No preview available"))))
      (setf (delve--zettel-preview zettel) preview)
      (lister-refresh-at lister-local-ewoc :point))))
  
;; * Open the org roam buffer

(defun delve--key--roam (zettel)
  "Open the org roam buffer for ZETTEL."
  (interactive (list (delve--current-item 'delve--zettel)))
  (org-roam-buffer-display-dedicated (delve--zettel-node zettel)))

;;; * Key commands not bound to a specific item at point

(defun delve--key--insert-tagged (tags &optional prefix)
  "Insert zettel matching TAGS.
With PREFIX, open search results in a new buffer."
  (interactive (list (completing-read-multiple " Insert zettels matching tag(s): "
                                               (delve-query-tags))
                     current-prefix-arg))
  (let* ((matching-string (delve--string-join tags " and " "#"))
         (zettels         (mapcar #'delve--zettel-create (delve-query-nodes-by-tags tags))))
    (if zettels
        (delve--insert-or-open-zettels zettels
                                       (format "Nodes matching %s" matching-string)
                                       prefix)
      (message "No zettels found matching %s" matching-string))))

(defun delve--key--collect-in-new-buffer (ewoc)
  "In Delve EWOC, collect all marked items in a new buffer.
Mark items in the region.  Switch to the new buffer."
  (interactive (list lister-local-ewoc))
  ;; Mark items in the region:
  (when (use-region-p)
    (lister-mode--mark-unmark-region ewoc
                                     (region-beginning)
                                     (region-end)
                                     t))
  (unless (lister-items-marked-p ewoc)
    (user-error "No items marked"))
  ;; Collect:
  (let (acc)
    (lister-walk-marked-nodes ewoc
                              (lambda (_ node)
                                (push (lister-node-get-data node) acc)))
    (switch-to-buffer
     (delve--add-to-buffer (nreverse acc)))))

;; Insert node(s)

(defun delve--select-multiple-nodes (node-fn)
  "Let the user select multiple nodes from NODE-FN."
  (setq org-roam-node-read--cached-display-format nil)
  ;; Standard completing-read-multiple can't work with node
  ;; candidates, so check if alternatives are available:
  (cl-letf (((symbol-function 'completing-read-multiple)
             (cond
              ((featurep 'consult) #'consult-completing-read-multiple)
              (t #'completing-read))))
    ;; ...collect and select:
    (let* ((node-alist     (with-temp-message "Collecting nodes..."
                             (mapcar #'org-roam-node-read--to-candidate (funcall node-fn))))
           (node-selected  (if node-alist
                               (completing-read-multiple "Choose: " node-alist)
                             (user-error "No nodes to choose from"))))
      (mapcar (lambda (cand)
                (alist-get cand node-alist nil nil #'string=))
              (if (listp node-selected) node-selected (list node-selected))))))

(defun delve--key--insert-node (&optional limit-to-tags)
  "Interactively add node(s) to current buffer's Delve list.
With prefix LIMIT-TO-TAGS, let the user first limit the selection
candidate list to only those nodes matching specific tags."
  (interactive "P")
  (let* ((node-fn  (if limit-to-tags
                       (apply-partially #'delve-query-nodes-by-tags
                                        (completing-read-multiple " Limit to nodes matching tags: "
                                                         (delve-query-tags)))
                     #'delve-query-node-list))
         (nodes    (delve--select-multiple-nodes node-fn)))
    ;;
    (lister-insert-list-at lister-local-ewoc :point
                           (mapcar #'delve--zettel-create nodes)
                           nil (lister-eolp))))

;; Collect items into a pile

(defun delve--move-into-pile-at (ewoc pos)
  "In EWOC, move all marked nodes into the pile at POS.
Delete all copied nodes.  Throw an error if there is no pile at
POS, or if PILE is marked."
  (let ((pile (lister-get-data-at ewoc pos)))
    (unless (delve--type-p pile 'delve--pile)
      (error "Item at position is not a pile"))
    ;; push:
    (lister-walk-marked-nodes ewoc
                              (lambda (ewoc node)
                                (let ((item (lister-node-get-data node)))
                                  (when (delve--type-p item 'delve--zettel)
                                    (push item (delve--pile-zettels pile))
                                    (lister-delete-at ewoc node)))))
    ;; uniqify the pile zettels:
    (setf (delve--pile-zettels pile)
          (seq-uniq (delve--pile-zettels pile)))
    (lister-refresh-at ewoc pos)))

(defun delve--key--collect-into-pile (ewoc)
  "In EWOC, collect all marked Zettel items in a new pile and insert it.
If region is active, mark all items in the region.  If point is
on a pile item, add marked items to this pile instead.  Remove
any duplicates in the final pile.  Skip non-zettel items when
collecting."
  (interactive (list lister-local-ewoc))
  ;; Mark items in the region:
  (when (use-region-p)
    (lister-mode--mark-unmark-region ewoc
                                     (region-beginning)
                                     (region-end)
                                     t))
  (unless (lister-items-marked-p ewoc)
    (user-error "No items marked"))
  ;; Quit if there are piles marked:
  (lister-walk-marked-nodes ewoc
                            (lambda (_ node)
                              (when (delve--type-p (lister-node-get-data node)
                                                   'delve--pile)
                                (user-error "Cannot move pile in itself"))))
  ;; If there is no pile at point, create one:
  (unless (delve--type-p (lister-get-data-at ewoc :point) 'delve--pile)
    (lister-insert-at ewoc :point
                      (delve--pile-create
                       :name (read-string "Name for the new pile: "))))
  ;; now stuff marked items into this pile at point:
  (delve--move-into-pile-at ewoc :point)
  ;; FIXME Once "lister-walk-marked" returns a count, use this to give
  ;; feedback how many items have been moved
  (lister-refresh-at ewoc :point))


;;; * Delete Items

(defun delve--key--delete ()
  "Delete all marked items or the single iten at point.
If a region is active, first mark all the items in the region.
Delete all marked items, if there are any.  Only delete the item
at point if there are no marked items.  If any of the items has a
sublist, also decrease the indentation of these subitems."
  (interactive)
  (let ((ewoc lister-local-ewoc))
    ;; Mark items in the region:
    (when (use-region-p)
      (lister-mode--mark-unmark-region ewoc
                                       (region-beginning)
                                       (region-end)
                                       t))
    ;; If there are no marked items, mark the item at point:
    (unless (lister-items-marked-p ewoc)
      (lister-mark-unmark-at ewoc :point t))
    ;; Ask the user:
    (if (not (y-or-n-p (format "Delete %s item(s)? " (lister-count-marked-items ewoc))))
        (user-error "Canceled")
      ;; Decrease any sublists:
      (lister-walk-marked-nodes ewoc
                                (lambda (ewoc node)
                                  (lister-with-sublist-below ewoc node beg end
                                    (lister-walk-nodes ewoc #'lister-move-item-left beg end))))
      ;; Actually delete the items:
      (lister-delete-marked-list ewoc))))


;; * Storing and reading buffer lists in a file

(defun delve--ask-storage-file-name (&optional existing-only)
  "Ask for a file name for a Delve store.
Limit selection to only existing files if EXISTING-ONLY is
non-nil.  Offer completion of files in the directory
`delve-store-directory'."
  (let* ((default-dir (concat (file-name-as-directory delve-store-directory)))
         (file-name (read-file-name "File name for a Delve store: " default-dir
                                    nil existing-only)))
    (cond
     ((file-directory-p file-name)
      (user-error "File must not be a directory"))
     ((and (not existing-only) (file-exists-p file-name))
      (if (y-or-n-p "File exists, overwrite it? ")
          file-name
        (user-error "Canceled")))
     (t file-name))))

;;; TODO Write test
(defun delve--do-save-buffer (buf file-name)
  "Store the Delve list of BUF in FILE-NAME."
  ;; store list:
  (let ((l (lister-map (buffer-local-value 'lister-local-ewoc buf)
                       #'delve-store--tokenize-object)))
    (unless (file-exists-p file-name)
      (make-empty-file file-name t))
    (delve-store--write file-name l)
    ;; refresh header:
    (with-current-buffer buf
      (setq-local delve-local-storage-file file-name)
      (lister-refresh-header-footer lister-local-ewoc))))

;;; TODO Write test
(defun delve--read-storage-file (file-name)
  "Return a new Delve buffer read from FILE-NAME."
  (interactive (list (delve--ask-storage-file-name :existing-only)))
  ;; locate file
  (unless (file-exists-p file-name)
    (let ((new-name (concat (file-name-as-directory delve-store-directory)
                            file-name)))
      (if (file-exists-p new-name)
          (setq file-name new-name)
        (error "File not found %s" file-name))))
  ;; read it:
  (let* ((l          (delve-store--read file-name))
         (delve-list (with-temp-message "Creating data objects..."
                       (delve-store--create-object-list l)))
         (buf-name   (format "DELVE Zettel imported from '%s'" (file-name-nondirectory file-name)))
         (buf        (delve--new-buffer buf-name delve-list)))
    (with-current-buffer buf
      (setq-local delve-local-storage-file file-name)
      (lister-refresh-header-footer lister-local-ewoc))
    buf))

(defun delve-save-buffer (buf)
  "Store BUF in its existing storage file or create a new one."
  (interactive (list (current-buffer)))
  (unless (eq 'delve-mode (with-current-buffer buf major-mode))
    (error "Buffer must be in Delve mode"))
  (let ((name  (or (buffer-local-value 'delve-local-storage-file buf)
                   (delve--ask-storage-file-name))))
    (delve--do-save-buffer buf name))
  (with-current-buffer buf
    (message "Collection stored in file %s" delve-local-storage-file)))

;;; * Delve Major Mode

;; * Delve Keymap
(defvar delve-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Buffer as a whole:
    (define-key map [remap save-buffer]              #'delve-save-buffer)
    ;; Any item:
    (define-key map (kbd "<delete>")                 #'delve--key--delete)
    ;; Insert node(s):
    (define-key map (kbd "n")                        #'delve--key--insert-node)
    ;; Work with marks:
    (define-key map (kbd "c")                        #'delve--key--collect-in-new-buffer)
    (define-key map (kbd "p")                        #'delve--key--collect-into-pile)
    ;; Work with the Zettel at point:
    (define-key map [remap org-roam-bufer-toggle]    #'delve--key--roam)
    (define-key map (kbd "o")                        #'delve--key--open-zettel)
    (define-key map (kbd "f")                        #'delve--key--fromlinks)
    (define-key map (kbd "b")                        #'delve--key--backlinks)
    (define-key map (kbd "<RET>")                    #'delve--key--toggle-preview)
    ;; Insert Queries or Piles:
    (define-key map (kbd "i")                        #'delve--key--insert-query-or-pile)
    (define-key map (kbd "t")                        #'delve--key--insert-tagged)
    ;; Remote Editing:
    (define-key map (kbd "+")                        #'delve--key--add-tags)
    (define-key map (kbd "-")                        #'delve--key--remove-tags)
    map)
  "Key map for `delve-mode'.")

(define-derived-mode delve-mode
  fundamental-mode "Delve"
  "Major mode for browsing your org roam zettelkasten."
  (lister-setup	(current-buffer) #'delve-mapper
                (concat "DELVE Version " delve-version))
  (add-to-invisibility-spec '(org-link ))
  (lister-mode))

;;; * Main Entry Point

(defun delve (&optional jump-to-last-buffer)
  "Visit a Delve collection.
With prefix argument JUMP-TO-LAST-BUFFER, directly switch to the
last selected buffer."
  (interactive "P")
  (let (buf)
    (when (and jump-to-last-buffer
               delve--last-selected-buffer
               (buffer-live-p delve--last-selected-buffer))
      (setq buf delve--last-selected-buffer))
    (switch-to-buffer
     (or buf (delve--select-collection-buffer "Visit collection: ")))))

;; (bind-key (kbd "<f12>") 'delve)

(provide 'delve)
;;; delve.el ends here

;; Local Variables:
;; eval: (add-to-list 'load-path (file-name-directory (buffer-file-name)))
;; End:
