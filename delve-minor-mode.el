;;; delve-minor-mode.el --- minor mode for optimized integration of delve and org roam  -*- lexical-binding: t; -*-

;; Copyright (C) 2021

;; This file is part of Delve.

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

;; Global minor mode to access some of delve's feature from
;; within org roam.

;;; Code:
(require 'delve)

;; TODO Add key to collect the complete file with all of its nodes

;; * Variables

(defvar delve--last-selected-buffer) ;; used in delve.el

;; * Minor Mode Functions

(defun delve-minor-mode--maybe-select (prompt &optional prefer-last-one)
  "Maybe ask user to select a Delve collection.
If PREFER-LAST-ONE is non-nil, choose the last selected buffer
instead, if there is one.  If selection takes place, use PROMPT."
  (or (and prefer-last-one delve--last-selected-buffer)
      (delve--select-collection-buffer prompt)))

(defun delve-minor-mode-collect (&optional use-last-buffer)
  "Add node at point to a Delve buffer.
If USE-LAST-BUFFER is non-nil, don't ask the user to select a
buffer and use the one selected the last time.  Recognize the
node by the ID property of the containing org entry.  If there is
no node, refer to the whole file.  If the file has no ID either,
throw an error."
  (interactive "P")
  (let* ((id (or (org-roam-id-at-point)
                 (user-error "No org roam node with ID found")))
         (target-buf (delve-minor-mode--maybe-select "Add node to buffer or collection: "
                                                     use-last-buffer)))
    (save-window-excursion
      (lister-add (buffer-local-value 'lister-local-ewoc target-buf)
                  (delve--zettel-create (delve-query-node-by-id id)))
      (message "Zettel added"))))

(defun delve-minor-mode-open-collection (&optional last-selection)
  "Open a Delve collection or create a new one.
If LAST-SELECTION is non-nil, switch to the last buffer selected
before."
  (interactive "P")
  (let ((buf (delve-minor-mode--maybe-select "Choose or create a Delve collection: "
                                             last-selection)))
    (switch-to-buffer buf)))

;; * Minor Mode(s)

(defvar delve-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c d b") #'delve-minor-mode-open-collection)
    (define-key map (kbd "C-c d +") #'delve-minor-mode-collect)
    map)
  "Local map for the delve minor mode.")

(define-minor-mode delve-minor-mode
  "Local minor mode to collect org roam node via Delve."
  :lighter " DelveMM"
  :group 'delve
  :keymap 'delve-minor-mode-map
  :require 'delve)

(defun delve--maybe-activate-minor-mode ()
  "Turn on delve minor mode if current buffer is in org roam."
  (interactive)
  (when (and (buffer-file-name)
	     (org-roam-file-p))
    (delve-minor-mode +1)))

(defun delve-minor-mode--mass-activate (&optional deactivate)
  "Activate or deactivate Delve minor mode in all org roam buffers.
Activate the mode unless DEACTIVATE is non-nil."
  (mapcar (lambda (buf)
            (with-current-buffer buf
              (when (and (buffer-file-name)
                         (org-roam-file-p))
                (delve-minor-mode (if deactivate -1 +1)))))
          (buffer-list)))

(define-minor-mode delve-global-minor-mode
  "Add some Delve functionality in org roam files."
  :lighter ""
  :global t
  :group 'delve
  :require 'delve
  ;; enable in future org mode buffers:
  (if delve-global-minor-mode
      (add-hook 'org-mode-hook #'delve--maybe-activate-minor-mode)
    (remove-hook 'org-mode-hook #'delve--maybe-activate-minor-mode))
  ;; enable/disable in current buffers, too:
  (delve-minor-mode--mass-activate (not delve-global-minor-mode)))

(provide 'delve-minor-mode)
;;; delve-minor-mode.el ends here
;; Local Variables:
;; eval: (add-to-list 'load-path (file-name-directory (buffer-file-name)))
;; End:
