;;; delve-lister-highlight.el --- add highlighting to a lister list  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021

;; Author:  <joerg@joergvolbers.de>
;; Keywords: hypermedia

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


;;; Code:

(require 'delve-lister)

;; * Highlight face

(defcustom delve-lister-highlight-face-or-property 'hl-line
  "Face or property list used for highlighting.
The value can be either the name of a face (a symbol) or a
property list with face attributes."
  :group 'delve-lister-highlight
  :type '(choice (face :tag "Name of a face")
		 (plist :tag "Face attributes")))

;; * Callbacks which do the highlighting

(defun delve-lister-highlight-item ()
  "Highlight the item at point.
If the item is marked, add the highlighting face to the
background, letting the mark stand out.  Else, add it to the
front, letting the highlighting stand out."
  (let* ((inhibit-read-only t)
	 (pos    (point))
	 (end    (delve-lister-end-of-lines (current-buffer) pos t)))
    (when (/= pos end)
      (delve-lister-add-face-property  pos end
				 delve-lister-highlight-face-or-property
				 (get-text-property pos 'mark)))))

(defun delve-lister-unhighlight-item ()
  "Remove the highlighting of the item at point."
  (let* ((inhibit-read-only t)
	 (pos    (point))
	 (end    (delve-lister-end-of-lines (current-buffer) pos t)))
    (when (and end
	       (/= pos end))
      (delve-lister-remove-face-property pos end
				   delve-lister-highlight-face-or-property))))

;; * Define the mode

(define-minor-mode delve-lister-highlight-mode
  "Toggle automatic highlighting of the lister item at point."
  :lighter ""
  :group 'delve-lister-highlight
  (unless (derived-mode-p 'delve-lister-mode)
    (user-error "This minor mode is to be used in a buffer with a lister major mode"))
  (if delve-lister-highlight-mode
      ;; enable:
      (let ((buf (current-buffer)))
	(delve-lister-add-enter-callback buf #'delve-lister-highlight-item)
	(delve-lister-add-leave-callback buf #'delve-lister-unhighlight-item)
	(delve-lister-sensor-enter buf))
    ;; disable:
    (progn
      (let ((buf (current-buffer)))
	(delve-lister-sensor-leave buf)
	(delve-lister-remove-enter-callback buf #'delve-lister-highlight-item)
	(delve-lister-remove-leave-callback buf #'delve-lister-unhighlight-item)))))

(provide 'delve-lister-highlight)
;;; delve-lister-highlight.el ends here
