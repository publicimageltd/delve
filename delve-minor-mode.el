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

;; Global minor mode to access some of Delve's feature from
;; within Org Roam.

;;; Code:
(require 'delve)

;; * Variables

(defvar delve-minor-mode-prefix-key (kbd "M-n")
  "Prefix key for Delve minor mode commands.
If you want to change this key in your .emacs file, set the
variable before loading this package.  With `use-package', use
the `:init' keyword.")

(defvar delve--last-selected-buffer) ;; used in delve.el

;; * Minor Mode Functions

(defun delve-minor-mode--maybe-select (prompt &optional prefer-last-one)
  "Maybe ask user to select a Delve collection.
If PREFER-LAST-ONE is non-nil, directlychoose the last selected
buffer, if there is one.  Else use PROMPT to ask the user."
  (or (and prefer-last-one delve--last-selected-buffer)
      (delve--select-collection-buffer prompt)))

(defun delve-minor-mode--add-to-collection (buf zettel)
  "Insert ZETTEL at point in Delve buffer BUF.
ZETTEL can be a list or a single zettel."
  (let ((ewoc (lister-get-ewoc buf)))
    (lister-insert-list-at ewoc :point
                           (if (listp zettel) zettel (list zettel))
                           nil
                           (lister-eolp buf))
    (lister-goto ewoc :next)))

(defun delve-minor-mode-collect (&optional use-last-buffer)
  "Add node at point to a Delve buffer.
Use the ID property of the containing org entry or refer to the
whole file.  If the file has no ID either, throw an error.  If
USE-LAST-BUFFER is non-nil, don't ask the user to select a buffer
and use the one selected the last time."
  (interactive "P")
  (let* ((id     (or (org-roam-id-at-point)
                     (user-error "No org roam node with ID found at point")))
         (zettel (delve--zettel-create (delve-query-node-by-id id)))
         (buf    (delve-minor-mode--maybe-select
                  "Add node to buffer or collection: "
                  use-last-buffer)))
    (delve-minor-mode--add-to-collection buf zettel)
    (message "Zettel added to '%s'" (buffer-name buf))))

(defun delve-minor-mode-collect-all (&optional use-last-buffer)
  "Add all headline nodes with an ID to a Delve buffer.
Ask user to select the buffer to add to.  If USE-LAST-BUFFER is
non-nil, use the previously selected buffer."
  (interactive "P")
  (let* ((tree  (org-element-parse-buffer))
         (ids   (or (org-element-map tree 'headline
                     (apply-partially #'org-element-property :ID))
                   (user-error "No headlines found")))
         (n     (length ids))
         (buf   (delve-minor-mode--maybe-select
                 (format "Add %d nodes to buffer or collection: " n)
                 use-last-buffer))
         (zettel (mapcar #'delve--zettel-create
                         (delve-query-nodes-by-id ids))))
    (delve-minor-mode--add-to-collection buf zettel)
    (message "%d zettel added to '%s'" n (buffer-name buf))))

;; TODO replace with delve--find-zettel-byid
(defun delve-minor-mode--find-id (id buf)
  "Find first ewoc node with ID in Delve buffer BUF."
  (cl-labels ((match-id (z)
                         (equal (delve--zettel-id z) id)))
    (lister-first-matching (lister-get-ewoc buf) :first
                           (lambda (delve-object)
                             (cl-typecase delve-object
                               (delve--zettel (match-id delve-object))
                               (delve--pile   (seq-find #'match-id (delve--pile-zettels delve-object))))))))

(defun delve-minor-mode--find-node ()
  "Find node at point in open Delve buffers.
Return a list with the ewoc node and the containing buffer."
  (let* ((id (or (org-roam-id-at-point)
                 (user-error "No org roam node with ID found at point")))
         (bufs (delve-buffer-list)))
    (cl-loop for buf in bufs
             if (delve--find-zettel-by-id id buf)
             return (list it buf)
             finally return nil)))

(defun delve-minor-mode-find-node ()
  "Jump to a Delve zettel referring to the current headline or file."
  (interactive)
  (pcase-let ((`(,ewoc-node ,buf) (delve-minor-mode--find-node)))
    (if (not ewoc-node)
        (user-error "Node not found in any open Delve buffer")
      (delve--push-to-global-mark-ring)
      (switch-to-buffer buf)
      (lister-goto (lister-get-ewoc buf) ewoc-node)
      (message "Mark pushed; pop back to go to the original buffer"))))

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
  (let ((prefix (define-prefix-command 'delve-mm-prefix-map nil "Delve")))
    ;; add space to each key description so that the key is also printed in
    ;; the echo area when pressing the prefix key
    (define-key prefix "b" '(" buffer list" . delve-minor-mode-open-collection))
    (define-key prefix "c" '(" collect"     . delve-minor-mode-collect))
    (define-key prefix "a" '(" collect all" . delve-minor-mode-collect-all))
    (define-key prefix "f" '(" find node"   . delve-minor-mode-find-node))
    ;;
    (let ((map (make-sparse-keymap)))
      (define-key map delve-minor-mode-prefix-key prefix)
      map))
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

;;;###autoload
(define-minor-mode delve-global-minor-mode
  "Add some Delve functionality in org roam files."
  :lighter ""
  :global t
  :group 'delve
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
