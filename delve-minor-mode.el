;;; delve-minor-mode.el --- minor mode for optimized integration of delve and org roam  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  

;; Author:  <joerg@joergvolbers.de>
;; Keywords: 

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

(defun delve-minor-mode-set-toplist (zettel-file)
  "Open ZETTEL-FILE in a new delve buffer."
  (interactive (list (buffer-file-name)))
  (if (org-roam--org-file-p zettel-file)
      (delve zettel-file)
    (user-error "%s is not an org roam file" zettel-file)))

;; * Map

(defvar delve-local-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "+") #'delve-edit-prompt-add-tag)
    (define-key map (kbd "-") #'delve-edit-prompt-remove-tag)
    (define-key map (kbd "d") #'delve-minor-mode-set-toplist)
    map)
  "Local prefix map for the delve minor mode.
Bind this map to a prefix key of your choice.")

(defvar delve-minor-mode-prefix-key (kbd "C-c d")
  "When activating delve minor mode, bind `delve-local-map' to
this key.''")

(defvar delve-minor-mode-old-prefix-key-command nil
  "Backup value of the command associated with
  `delve-minor-mode-prefix-key' before changing it.")

(define-minor-mode delve-minor-mode
  "Easier access to some DELVE functionality."
  :lighter "delveminor"
  :group 'delve
  :require 'delve
  (if delve-minor-mode
      (progn
	(setq delve-minor-mode-old-prefix-key-command (local-key-binding delve-minor-mode-prefix-key))
	(local-set-key delve-minor-mode-prefix-key delve-local-map))
    (local-set-key delve-minor-mode-prefix-key delve-minor-mode-old-prefix-key-command)))

(provide 'delve-minor-mode)
;;; delve-minor-mode.el ends here
