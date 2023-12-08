;;; delve-edit.el --- Remote Commands for Editing Org Roam Nodes  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023

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
(require 'delve-query)

;;; Code:

(defmacro delve-edit--with-file (file point &rest body)
  "Execute BODY with FILE as current buffer.
Open FILE, move to POINT and execute BODY while preserving mark
and point. Kill the buffer afterwards if it has not been visited
yet.

This is basically a wrapper around `org-roam-with-file', which
see."
  (declare (indent 2) (debug (sexp sexp body)))
  `(org-roam-with-file ,file t
     (save-mark-and-excursion
       (goto-char ,point)
       ,@body)))

(defmacro delve-edit--with-zettel-node (zettel &rest body)
  "Execute BODY with point at ZETTEL's node."
  (declare (indent 1) (debug (sexp body)))
  (let ((file-var (make-symbol "--file--"))
        (point-var (make-symbol "--point--")))
    `(let ((,file-var (delve--zettel-file ,zettel))
           (,point-var (delve--zettel-point ,zettel)))
       (unless ,file-var
         (error "Zettel does not have corresponding file"))
       (unless ,point-var
         (error "Zettel has no associated position"))
       (delve-edit--with-file ,file-var ,point-var
         ,@body))))

(defun delve-edit--prompt-add-tags (zettels)
  "Prompt user to add tags to ZETTELS.
ZETTELS must be a zettel object or a list of zettel objects."
  (let ((org-use-tag-inheritance nil)
        (zettels (-list zettels))
        (tags    (completing-read-multiple "Add tag(s): "
                                           (org-roam-tag-completions))))
    (cl-dolist (zettel zettels)
      (delve-edit--with-zettel-node zettel
        (org-roam-tag-add tags)))))

(defun delve-edit--prompt-remove-tags (zettels)
  "Prompt user to remove tags from ZETTELS.
ZETTELS must be a zettel object or a list of zettel objects."
  (let* ((org-use-tag-inheritance nil)
         (zettels (-list zettels))
         (tags (completing-read-multiple "Remove tag(s): "
                                         (delve-query-tags (-map #'delve--zettel-id zettels)))))
    (cl-dolist (zettel zettels)
      (delve-edit--with-zettel-node zettel
        (org-roam-tag-remove tags)))))

(provide 'delve-edit)
;;; delve-edit.el ends here
