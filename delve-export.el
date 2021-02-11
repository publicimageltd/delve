;;; delve-export.el --- export org roam pages        -*- lexical-binding: t; -*-

;; Copyright (C) 2020  

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

;; Functions to export org roam zettel using the standard org export
;; dispatcher. By calling `delve-export-file' in an org roam file, the
;; file is being copied in a separate directory and exported using the
;; org export function.

;;; Code:
(require 'org-roam)

;; * Variables

(defvar delve-export-function (lambda ()
				(let ((org-export-initial-scope 'buffer))
				  (ignore org-export-initial-scope)
				  (org-export-dispatch )))
  "In-buffer-function to call for exporting the org buffer.")

(defvar delve-export-directory (concat
				(expand-file-name user-emacs-directory)
				"roam-file-exports")
  "Directory for exporting org roam files. 
Each exported org roam file will be copied in a separate, freshly
created directory, in which the export is taking place.")

;; * Export

(defun delve-export-maybe-get-file ()
  "Return current buffer's file name if it is an org roam file."
  (and (derived-mode-p 'org-mode) ;; FIXME This excludes exporting markdown files
       (when-let* ((file-name (buffer-file-name)))
	 (locate-file file-name (list org-roam-directory)))))

(defun delve-export-create-file-name (base-dir filename) 
  "Return a file name for exporting FILENAME.
The file will be within a subdirectory of BASE-DIR, the directory
name being FILENAME without the file suffix. Create this
directory if it does not exist."
  (unless (stringp filename)
    (error "Argument FILENAME must be a string."))
  (let* ((new-file (concat (file-name-as-directory base-dir)
			   (file-name-as-directory (file-name-base filename))
			   (file-name-nondirectory filename))))
    (unless (file-directory-p (file-name-directory new-file))
      (make-directory (file-name-directory new-file) t))
    new-file))

(defun delve-export-put-warning ()
  "Add a warning header to current buffer."
  (setq header-line-format
	'(:propertize "Use this copied file for exporting only."
		      'face 'bold)))

;;;###autoload 
(defun delve-export-find-original-file ()
  "In an exported org file, find the original org roam file."
  (interactive)
  (when-let* ((file-name  (file-name-nondirectory (buffer-file-name)))
	      (export-dir (file-name-directory
			   (delve-export-create-file-name delve-export-directory file-name))))
    (unless (locate-file file-name (list export-dir))
      (user-error "This file is not in a delve export directory."))
    (if-let* ((original-file (locate-file file-name (list org-roam-directory))))
	(progn 
	  (find-file original-file)
	  (message "Now visiting the original org roam file."))
      (user-error "Could not find file %s in the org roam directory."
		  file-name))))

;;;###autoload 
(defun delve-export-file (base-dir file)
  "Copy FILE to its own directory in BASE-DIR and call `delve-export-file'."
  (interactive (list delve-export-directory
		     (or (delve-export-maybe-get-file)
			 (read-file-name "Select roam file to export: "
					 org-roam-directory))))
  ;; save the buffer since we are copying the file:
  (with-current-buffer (find-file-noselect file)
    (save-buffer))
  ;; copy the file and trigger export:
  (let* ((target-file (delve-export-create-file-name base-dir file)))
    (when-let* ((old-buf (find-buffer-visiting target-file)))
      (with-current-buffer old-buf
	(save-buffer)
	(kill-buffer)))
    (copy-file file target-file t)
    (find-file target-file)
    (goto-char (point-min))
    (delve-export-put-warning)
    (funcall delve-export-function)))


(provide 'delve-export)
;;; delve-export.el ends here
