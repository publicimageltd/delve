;;; delve-edit.el --- functions for remote editing org roam files  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  

;; Author:  <joerg@joergvolbers.de>
;; Keywords: convenience

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

;; This is a library for "delve".

;;; Code:

;; * Dependencies:

(require 'org-element)
(require 'delve-db)
(require 'org-roam)
(require 'cl-lib)

;; * Interactive Remote Editing

(defmacro delve-edit-in-file (file &rest body)
  "Execute BODY in a buffer with FILE, saving all changes.
If FILE is already visited, use that buffer; else load it in a
temporary buffer.
Do not recurse this macro."
  (declare (indent 1) (debug t))
  `(progn 
     (unless (org-roam--org-roam-file-p ,file)
       (error "File nor an org roam file"))
     (let* ((__loaded-p (get-file-buffer ,file))
	    (__buf      (or __loaded-p (find-file-noselect ,file))))
       (with-current-buffer __buf
	 (save-buffer)
	 ,@body
	 (save-buffer))
       (unless __loaded-p
	 (kill-buffer __buf)))))

;;;###autoload
(defun delve-edit-prompt-add-tag (zettel-file)
  "Interactively add a tag to ZETTEL-FILE."
  (interactive (list buffer-file-name))
  (delve-edit-in-file zettel-file
    (org-roam-tag-add)))

;;;###autoload
(defun delve-edit-prompt-remove-tag (zettel-file)
  "Interactively remove a tag from ZETTEL-FILE."
  (interactive (list buffer-file-name))
  (delve-edit-in-file zettel-file
    (org-roam-tag-delete)))

;;;###autoload
(defun delve-edit-prompt-add-alias (zettel-file)
  "Interactively add an alias for ZETTEL-FILE"
  (interactive (list buffer-file-name))
  (delve-edit-in-file zettel-file
    (org-roam-alias-add)))

;;;###autoload
(defun delve-edit-prompt-remove-alias (zettel-file)
  "Interactively remove an alias from ZETTEL-FILE"
  (interactive (list buffer-file-name))
  (delve-edit-in-file zettel-file
    (org-roam-alias-delete)))


(provide 'delve-edit)
;;; delve-edit.el ends here
