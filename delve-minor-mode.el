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

;; Minor mode to access some of delve's feature from within org roam.
;; Bind this minor mode to org roam, i.e. by using hooks.

;;; Code:
(require 'delve)

;; TODO Rename "visit-node" to "collect"
;; TODO Ask user to add to existing Delve buffers, if they exist
;; TODO Add special key to collect the file with all of its nodes

;; * Variables

(defvar delve-minor-mode-prefix-key (kbd "C-c d")
  "Prefix for delve minor mode keys.")

(defvar delve-minor-mode--old-prefix-key-command nil
  "Backup value of `delve-minor-mode-prefix-key'.")

;; * Functions

(defun delve-minor-mode-maybe-activate ()
  "Turn on delve minor mode if current buffer is in org roam.
Add this to `find-file-hook'."
  (interactive)
  (when (and (buffer-file-name)
	     (org-roam-file-p))
    (delve-minor-mode +1)))

(defun delve-minor-mode-collect ()
  "Add node at point to a delve buffer.
Recognize the node by the ID property of the current org entry.
If there is no node, refer to the whole file.  If the file has no
ID either, throw an error."
  (interactive)
  ;; TODO Use org-roam-id-at-point instead!
  (let* ((id (or (org-id-get)
                 (save-excursion
                   (org-with-wide-buffer
                    (org-back-to-heading-or-point-min t)
                    (org-id-get)))
                 (user-error "No org roam ID found")))
         (title (when-let ((l (org-collect-keywords '("TITLE"))))
                  (car (alist-get "TITLE" l nil nil #'string=)))))
    (switch-to-buffer
     (delve--new-buffer (or title "Collected node")
                        (list
                         (delve--zettel-create
                          (delve-query-node-by-id id)))))))

;; * Map

(defvar delve-local-map
  (let ((map (make-sparse-keymap)))
    ;; TODO Replace with working functions
    ;; (define-key map (kbd "+") #'org-roam-tag-add)
    ;; (define-key map (kbd "-") #'org-roam-tag-delete)
    (define-key map (kbd "d") #'delve-minor-mode-collect)
    map)
  "Local prefix map for the delve minor mode.
Bind this map to a prefix key of your choice.")

(define-minor-mode delve-minor-mode
  "Easier access to some DELVE functionality."
  :lighter "delveminor"
  :group 'delve
  :require 'delve
  (if delve-minor-mode
      (progn
	(setq delve-minor-mode--old-prefix-key-command
              (local-key-binding delve-minor-mode-prefix-key))
	(local-set-key delve-minor-mode-prefix-key delve-local-map))
    (local-set-key delve-minor-mode-prefix-key delve-minor-mode--old-prefix-key-command)))

(provide 'delve-minor-mode)
;;; delve-minor-mode.el ends here
