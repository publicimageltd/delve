;;; delve-edit.el --- Remote Commands for Editing Org Roam Nodes  -*- lexical-binding: t; -*-

;; Copyright (C) 2021

;; Author:  <joerg@joergvolbers.de>

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

;; Edit Org Roam files without visiting them.

(require 'org-roam)
(require 'delve-data-types)

;;; Code:

(defmacro delve-edit--with-zettel-node (zettel &rest body)
  "Execute BODY with point at ZETTEL's node."
  (declare (indent 1) (debug (sexp body)))
  (let ((file-var (make-symbol "--file--")))
    `(let ((,file-var (delve--zettel-file ,zettel)))
       (unless ,file-var
         (user-error "Zettel does not have corresponding file"))
       (org-roam-with-file ,file-var t
         (save-mark-and-excursion
           (goto-char (delve--zettel-point ,zettel))
           ,@body)))))

(defun delve-edit--add-tags (zettel &optional tags)
  "Add TAGS to the node in ZETTEL.
If TAGS is nil, ask the user."
  (delve-edit--with-zettel-node zettel
    (let ((org-use-tag-inheritance nil))
      (if tags
          (org-roam-tag-add tags))
      (call-interactively 'org-roam-tag-add))))

(defun delve-edit--remove-tags (zettel &optional tags)
  "Remove TAGS from the node in ZETTEL.
If TAGS is nil, ask the user."
    (delve-edit--with-zettel-node zettel
      (let ((org-use-tag-inheritance nil))
        (if tags
            (org-roam-tag-remove tags)
          (call-interactively 'org-roam-tag-remove)))))

(provide 'delve-edit)
;;; delve-edit.el ends here
