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

;;; TODO Prettify zettel output
;;; TODO Make RET visit the zettel at point, with typecase for extensions
;;; TODO Create function which opens a "pile buffer" (pile as arg)
;;; TODO Introduce "pile" type (like a folder)
;;; TODO Allow user to wrap zettel into a pile

;; * Dependencies

(require 'org-roam)
(require 'lister)
(require 'lister-mode)
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

;;; * The Lister Mapper

;; Printing anything

(defun delve--type-as-string (delve-item)
  "Return a string or icon representing the type of DELVE-ITEM.
If the global variable `delve--no-icons' is bound, always only
return strings."
  (let* ((representation
          (pcase (type-of delve-item)
            (`delve--query  (list "QUERY" "search"))
            (`delve--pile   (list "PILE"  "list"))
            (`delve--zettel (list "NODE"  "sticky-note-o"))
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

(defun delve--string-join (strings &optional separator prefix)
  "Join all non-nil strings in STRINGS using SEPARATOR.
Optionally add string PREFIX to each non-nil item."
  (let ((strings (lister--flatten strings)))
    (when prefix
      (setq strings (mapcar (apply-partially #'concat prefix) strings)))
    (string-join strings separator)))

(defun delve--tags-as-string (tags)
  "Return TAGS as a list of strings."
  (delve--string-join tags ", " "#"))

(defun delve--zettel-strings (zettel)
  "Return a list of strings representing of ZETTEL."
  (let ((node (delve--zettel-node zettel)))
    ;; TODO add faces using delve-pp
    (list (org-roam-node-title node)
          (when-let ((tags (org-roam-node-tags node)))
            (delve--tags-as-string tags)))))

;; The actual mapper
(defun delve-mapper (item)
  "Transform ITEM into a list of printable strings."
  (let* ((typestring  (delve--type-as-string item))
         (datastrings (cl-typecase item
                        (delve--zettel (delve--zettel-strings item))
                        (t (list "no printer available for that item type")))))
    (let* ((pad        (make-string (length typestring) ? ))
           (first-line (concat typestring (car datastrings)))
           (rest-lines (mapcar (apply-partially #'concat pad)
                               (cdr datastrings))))
      (apply #'list first-line rest-lines))))

;;; * Delve Major Mode

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
             (nodes (seq-take (delve-query-node-list) 10))
             (zettel (mapcar #'delve--zettel-create nodes)))
        (lister-set-list ewoc zettel)))))

(provide 'delve)
;;; delve.el ends here
