;; delve-minor-mode.el --- minor mode for optimized integration of delve and org roam  -*- lexical-binding: t; -*-

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

(defcustom delve-minor-mode-prefix-key (kbd "M-n")
  "Prefix key for Delve minor mode commands.
If you want to change this key in your .emacs file, set the
variable before loading this package.  With `use-package', use
the `:init' keyword."
  :group 'delve
  :type 'key-sequence)

(defcustom delve-minor-mode-new-collection-title #'delve-minor-mode--get-title
  "Function which returns a title when creating collections on the fly.
The function is called with no argument with the source buffer
for the collection selected and has to return a suggestion for
the collection title.  Note that the source buffer is most likely
an Org buffer visiting an Org Roam file, but in future
implementations, it might as well be another buffer, e.g. the Org
Roam buffer."
  :group 'delve
  :type 'function)

(defvar delve--last-selected-buffer) ;; used in delve.el

;;; * Minor Mode Functions

;; * Utilities

(defun delve-minor-mode--get-title ()
  "Return an informative collection title."
  (or (cond
       ((org-roam-buffer-p)
        (org-roam-db--file-title))
       ((eq major-mode 'org-roam-mode)
        (org-roam-node-title org-roam-buffer-current-node)))
      "Collection"))

(defun delve-minor-mode--auto-collection-buffer (prefix)
  "Return a new collection buffer with auto generated title.
Prepend PREFIX to the title created by
`delve-minor-mode-new-collection-title'."
  (let ((title (concat prefix
                       (when prefix " ")
                       (funcall delve-minor-mode-new-collection-title))))
    (delve--new-buffer (generate-new-buffer-name title))))

(defun delve-minor-mode--id-at-point ()
  "Get node in current buffer.
Works in Org Roam buffers as well as in Org files."
  (cond
   ((org-roam-file-p) (org-roam-id-at-point))
   ((eq major-mode 'org-roam-mode)
    (progn
      (magit-section-case
        (org-roam-node-section (org-roam-node-id (oref it node)))
        (org-roam-preview-section
         (save-excursion
           (magit-section-up)
           (delve-minor-mode--id-at-point))))))))

(defun delve-minor-mode--get-collection (scheme)
  "Return a Delve collection buffer according to the list SCHEME.
SCHEME must contain a keyword and optionally a corresponding
argument, e.g. `(:select \"Add node\")'.

If keyword is `:select', prompt the user for a collection.  If it
is `:last', use the last selected collection, if possible.  In
both cases, the second argument is the prompt to use for the user
interaction.

If the keyword is `:auto', return a new collection with an
automatically generated title using the additional argument as a
prefix string."
  (pcase scheme
    (`(:select ,prompt) (delve--select-collection-buffer prompt))
    (`(:last    ,prompt) (or (and delve--last-selected-buffer
                                  (buffer-live-p delve--last-selected-buffer))
                            (delve-minor-mode--get-collection `(:select ,prompt))))
    (`(:auto)           (delve-minor-mode--get-collection `(:auto "")))
    (`(:auto   ,prefix) (delve-minor-mode--auto-collection-buffer prefix))
    (_ (error "Unknown scheme %S" scheme))))

(defun delve-minor-mode--get-collection-by-prefix (num-prefix prompt auto-prefix)
  "Return a collection buffer according to numeric NUM-PREFIX.
With NUM-PREFIX set to nil or 1, add node to the last selected
collection.  With NUM-PREFIX 4, prompt for the collection.  With
NUM-PREFIX set to 16, create a new collection on the fly.

For interactive selection, use PROMPT; when creating buffers on
the fly, prefix them with AUTO-PREFIX."
  (delve-minor-mode--get-collection
   (cl-case (or num-prefix 1)
     (1      `(:last ,prompt))
     (4      `(:select ,prompt))
     (16     `(:auto ,auto-prefix))
     (t      (error "Cannot parse prefix argument %S" num-prefix)))))


;; * Collecting Nodes

;; First utilities for collecting stuff...

(defun delve-minor-mode--get-this-node ()
  "Return the current Org Roam node at point.
If there is no node at point, use the buffer file node instead.
Throw an error if there is no node at all."
  (let ((id    (or (delve-minor-mode--id-at-point)
                   (user-error "No Org Roam node with ID found at point"))))
    (delve-query-node-by-id id)))

(defun delve-minor-mode--get-all-nodes ()
  "Return all Org Roam nodes from current buffer.
Throw an error if there are no nodes."
  ;; TODO Add handling for Org Roam Mode buffers
  (unless (org-roam-file-p)
    (user-error "This command can only be used in Org Roam files"))
  (let* ((tree  (org-element-parse-buffer))
         (ids   (or (org-element-map tree 'headline
                     (apply-partially #'org-element-property :ID))
                    (user-error "No headlines with ID"))))
    (delve-query-nodes-by-id ids)))

(defun delve-minor-mode--get-backlinks ()
  "Return all backlinks to the node at point as Org Roam nodes.
Throw an error if there are no backlinks."
  (let ((id    (or (delve-minor-mode--id-at-point)
                   (user-error "No Org Roam node with ID found at point"))))
    (or (delve-query-backlinks-by-id id)
        (user-error "No backlinks found!"))))

;; ...now all interactive stuff:

(defun delve-minor-mode-collect (&optional prefix)
  "Add node at point to a Delve collection buffer.
Use the ID property of the containing org entry or the node of
the whole file document.  If the file has no ID either, throw an
error.

Without interactive PREFIX, add node to the last selected
collection.  With single PREFIX 4 , prompt for the collection.
With double PREFIX 16, create a new collection on the fly."
  (interactive "p")
  (let* ((prompt "Add node to buffer or collection: ")
         (node   (delve-minor-mode--get-this-node))
         (buf    (delve-minor-mode--get-collection-by-prefix prefix prompt "Collection from")))
    (delve-insert-nodes buf node)
    (message "Zettel added to '%s'" (buffer-name buf))))

(defun delve-minor-mode-collect-all (&optional prefix)
  "Collect all headline nodes with an ID into a Delve buffer.

Without interactive PREFIX, add node to the last selected
collection.  With single PREFIX 4 , prompt for the collection.
With double PREFIX 16, create a new collection on the fly."
  (interactive "p")
  (when (eq major-mode 'org-roam-mode)
    (user-error "This command cannot be used in Org Roam Mode buffers"))
  (let* ((nodes (delve-minor-mode--get-all-nodes))
         (n     (length nodes))
         (prompt (format "Add %d nodes to buffer or collection: " n))
         (buf   (delve-minor-mode--get-collection-by-prefix prefix prompt "Collection from")))
    (delve-insert-nodes buf nodes)
    (message "%d zettels added to '%s'" n (buffer-name buf))))

(defun delve-minor-mode-collect-backlinks (&optional prefix)
  "Collect all backlinks from current node into a Delve buffer.

Without interactive PREFIX, add nodes to the last selected
collection.  With single PREFIX 4 , prompt for the collection.
With double PREFIX 16, create a new collection on the fly."
  (interactive "p")
  (let* ((backlinks  (delve-minor-mode--get-backlinks))
         (n (length backlinks))
         (prompt (format "Add %d backlinks to buffer or collection: " n))
         (buf   (delve-minor-mode--get-collection-by-prefix prefix prompt "Backlinks from")))
    (delve-insert-nodes buf backlinks)
    (message "%d backlinks added to '%s'" n (buffer-name buf))))

;; * Inspecting Nodes

(defun delve-minor-mode-inspect-backlinks ()
  "Switch to a new Delve buffer with all backlinks from this node."
  (let* ((backlinks  (delve-minor-mode--get-backlinks))
         (buf   (delve-minor-mode--get-collection '(:auto "Backlinks from"))))
      (delve-insert-nodes buf backlinks)
      (setq delve--last-selected-buffer buf)
      (switch-to-buffer buf)))

(defun delve-minor-mode-inspect-this-node ()
  "Switch to a new Delve buffer with the node at point."
  (let* ((node (delve-minor-mode--get-this-node))
         (buf  (delve-minor-mode--get-collection '(:auto "Collected node"))))
    (delve-insert-nodes buf node)
    (setq delve--last-selected-buffer buf)
    (switch-to-buffer buf)))

(defun delve-minor-mode-inspect-all ()
  "Switch to a new Delve buffer with all nodes."
  (let* ((nodes (delve-minor-mode--get-all-nodes))
         (buf   (delve-minor-mode--get-collection '(:auto "Collected nodes"))))
    (delve-insert-nodes buf nodes)
    (setq delve--last-selected-buffer buf)
    (switch-to-buffer buf)))

;; * Finding nodes

(defun delve-minor-mode--find-node ()
  "Find node at point in open Delve buffers.
Return a list with the Ewoc list node and the containing buffer."
  (let* ((id  (or (org-roam-id-at-point)
                  (user-error "No Org Roam node found")))
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
      (message "Mark pushed; use `pop-global-mark' to jump back to the original buffer"))))

;; * Switch to a Delve collection buffer

(defun delve-minor-mode-visit-last-collection ()
  "Open the last used Delve collection."
  (interactive)
  (unless delve--last-selected-buffer
    (user-error "There is no last selected collection buffer to jump to"))
  (switch-to-buffer delve--last-selected-buffer))

;; * Edit Node at Point

(defun delve-minor-mode-tag-add ()
  "Interactively add tags to the node at point."
  (interactive)
  (let ((org-use-tag-inheritance nil))
    (call-interactively 'org-roam-tag-add)))

(defun delve-minor-mode-tag-remove ()
  "Interactively remove tags from the node at point."
  (interactive)
  (let ((org-use-tag-inheritance nil))
    (call-interactively 'org-roam-tag-remove)))

;; * Mode definitions

;; TODO Use transient instead
(defvar delve-minor-mode-map
  (let ((prefix (define-prefix-command 'delve-mm-prefix-map nil "Delve")))
    ;; add space to each key description so that the key is also printed in
    ;; the echo area when pressing the prefix key
    (define-key prefix "b" '(" collect backlinks"           . delve-minor-mode-collect-backlinks))
    (define-key prefix "l" '(" last used collection buffer" . delve-minor-mode-visit-last-collection))
    (define-key prefix "c" '(" collect this node"     . delve-minor-mode-collect))
    (define-key prefix "a" '(" collect all nodes" . delve-minor-mode-collect-all))
    (define-key prefix "f" '(" find this node"   . delve-minor-mode-find-node))
    (define-key prefix "+" '(" add tag"     . delve-minor-mode-tag-add))
    (define-key prefix "-" '(" remove tag"  . delve-minor-mode-tag-remove))
    (define-key prefix "." '(" set ID"      . org-id-get-create))
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
