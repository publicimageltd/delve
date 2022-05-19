;; delve-minor-mode.el --- minor mode for optimized integration of Delve and Org Roam  -*- lexical-binding: t; -*-

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
(require 'delve-transient)

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
    (`(:last    ,prompt) (if (and delve--last-selected-buffer
                                  (buffer-live-p delve--last-selected-buffer))
                             delve--last-selected-buffer
                           (delve-minor-mode--get-collection `(:select ,prompt))))
    (`(:auto)           (delve-minor-mode--get-collection `(:auto "")))
    (`(:auto   ,prefix) (delve-minor-mode--auto-collection-buffer prefix))
    (_ (error "Unknown scheme %S" scheme))))

(defun delve-minor-mode--get-collection-by-transient-args (args prompt auto-prefix)
  "Return a collection buffer using transient argument list ARGS.
ARGS must be a list with a string expressing the key-value pair
'--target=X', where X must be either `auto', `last' or `select'.
PROMPT is used when the user is asked to select a collection;
AUTO-PREFIX is prefixed when a buffer is created on the fly.  See
`delve-minor-mode--get-collection'."
  (delve-minor-mode--get-collection
   (pcase (plist-get (delve-transient--args-to-plist args) :target)
     (`"last"   `(:last ,prompt))
     (`"select" `(:select ,prompt))
     (`"auto"   `(:auto ,auto-prefix))
     (_         (error "Could not parse ARGS list %S" args)))))

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

;; We define each command as a transient suffix using the list
;; ARGS to determine the target buffer.  To provide these commands as
;; separate keys (in case someone wants to use them directly, without
;; calling the transient), simply wrap these transient suffixes in an
;; interactive function, e.g.
;;
;; (defun delve-minor-mode--key--collect (prefix)
;;   "Key wrapper for transient suffix `delve-minor-mode--do-collect'.
;; Use PREFIX to toggle the target buffer."
;;   (interactive "P")
;;   (delve-minor-mode--do-collect (pcase prefix
;;                                   (`nil "--target=last")
;;                                   (`(1) "--target=select")
;;                                   (`(4) "--target=auto"))))


;; NOTE Currently the target option 'select' is not used; if the UI
;; hardens, consider removing it (also in the core function
;; ...get-collection..)

(transient-define-suffix delve-minor-mode--do-collect (&optional args)
  "Add node at point to a Delve buffer specified by ARGS.
Specify the target buffer by using ARGS, a list with a string
expressing the key-value pair '--target=X', where X must be
either `auto', `last' or `select'.

Use the ID property of the containing org entry or the node of
the whole file document.  If the file has no ID either, throw an
error."
  (interactive (list (transient-args transient-current-command)))
  ;; default if not called from a transient
  (when (or (not args) (equal args '(nil)))
    (setq args '("--target=last")))
  ;; and action:
  (let* ((buf    (delve-minor-mode--get-collection-by-transient-args
                  args
                  "Add node to buffer or collection: "
                  "Collection from "))
         (node   (delve-minor-mode--get-this-node)))
    (delve-insert-nodes buf node)
    (message "Zettel added to '%s'" (buffer-name buf))))

(transient-define-suffix delve-minor-mode--do-collect-all (&optional args)
  "Collect all headline nodes with an ID into a Delve buffer.
Select target buffer using ARGS.  ARGS must be a list with a
string expressing the key-value pair '--target=X', where X must
be either `auto', `last' or `select'."
  (interactive (list (transient-args transient-current-command)))
  ;; default if not called from a transient
  (when (or (not args) (equal args '(nil)))
    (setq args '("--target=last")))
  ;; and action:
  (let* ((nodes (delve-minor-mode--get-all-nodes))
         (n     (length nodes))
         (prompt (format "Add %d nodes to buffer or collection: " n))
         (buf   (delve-minor-mode--get-collection-by-transient-args
                 args
                 prompt
                 "Collection from")))
    (delve-insert-nodes buf nodes)
    (message "%d zettels added to '%s'" n (buffer-name buf))))

(transient-define-suffix delve-minor-mode--do-collect-backlinks (&optional args)
  "Collect all backlinks from current node into a Delve buffer.
Select target buffer using ARGS.  ARGS must be a list with a
string expressing the key-value pair '--target=X', where X must
be either `auto', `last' or `select'."
  (interactive (list (transient-args transient-current-command)))
  ;; default if not called from a transient
  (when (or (not args) (equal args '(nil)))
    (setq args '("--target=last")))
  ;; and action:
  (let* ((backlinks  (delve-minor-mode--get-backlinks))
         (n (length backlinks))
         (prompt (format "Add %d backlinks to buffer or collection: " n))
         (buf   (delve-minor-mode--get-collection-by-transient-args
                 args
                 prompt
                 "Backlinks from")))
    (delve-insert-nodes buf backlinks)
    (message "%d backlinks added to '%s'" n (buffer-name buf))))

;; * Inspecting Nodes

(transient-define-suffix delve-minor-mode--do-inspect-backlinks ()
  "Switch to a new Delve buffer with all backlinks from this node."
  (interactive)
  (let* ((backlinks  (delve-minor-mode--get-backlinks))
         (buf   (delve-minor-mode--get-collection '(:auto "Backlinks from"))))
      (delve-insert-nodes buf backlinks)
      (setq delve--last-selected-buffer buf)
      (switch-to-buffer buf)))

(transient-define-suffix delve-minor-mode--do-inspect-this-node ()
  "Switch to a new Delve buffer with the node at point."
  (interactive)
  (let* ((node (delve-minor-mode--get-this-node))
         (buf  (delve-minor-mode--get-collection '(:auto "Collected node"))))
    (delve-insert-nodes buf node)
    (setq delve--last-selected-buffer buf)
    (switch-to-buffer buf)))

(transient-define-suffix delve-minor-mode--do-inspect-all ()
  "Switch to a new Delve buffer with all nodes."
  (interactive)
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

;; * Transients

(defclass delve-minor-mode--target-choice-class (delve-transient-switches)
  ((choices        :initform '("last" "auto"))
   (pretty-choices :initform '("current" "on the fly"))
   (allow-nil      :initform nil))
  "Transient infix option class for selecting the target collection.")

;; We derive a new class with its own display method:
(defclass delve-minor-mode--target-buffer-variable-class (transient-lisp-variable)
  ((reader         :initform (lambda (&rest _) (delve--select-collection-buffer "Select or create target collection: "))))
  "Transient infix class for specifying the target collection by value.")

(cl-defmethod transient-format-value ((obj delve-minor-mode--target-buffer-variable-class))
  "Display the value of OBJ as a  buffer variable."
  (let ((val (oref obj value)))
    (when val
      (propertize (buffer-name (oref obj value))
              'face 'transient-value))))

(transient-define-infix delve-minor-mode--target-infix ()
  "Target buffer."
  :class 'delve-minor-mode--target-buffer-variable-class
  :variable 'delve--last-selected-buffer)

(transient-define-prefix delve-minor-mode-collect-actions ()
  "Transient prefix for collecting Org Roam nodes using Delve."
  ["Target"
   ("c"  "Current target buffer" delve-minor-mode--target-infix)
   ("t"  "Collect into..." "--target=" :class delve-minor-mode--target-choice-class)]
  [["Collect"
    ("n" "Node at point"  delve-minor-mode--do-collect)
    ("a" "All nodes"      delve-minor-mode--do-collect-all)
    ("b" "Backlinks"      delve-minor-mode--do-collect-backlinks)]
   ["Find node in collection"
    ("f" "Find node at point"  delve-minor-mode-find-node)]]
   [("q" "Quit" transient-quit-one)])

(transient-define-prefix delve-minor-mode-inspect-actions ()
  "Transient prefix for inspecting Org Roam nodes using Delve."
  ["Inspect"
    ("n" "Node"      delve-minor-mode--do-inspect-this-node)
    ("a" "All nodes" delve-minor-mode--do-inspect-all)
    ("b" "Backlinks" delve-minor-mode--do-inspect-backlinks)
    ("q" "Quit" transient-quit-one)])

(transient-define-prefix delve-minor-mode-edit-actions ()
  "Transient prefix for editing Org Roam nodes using Delve."
   ["Edit"
    ("+" "Add tag"    delve-minor-mode-tag-add)
    ("-" "Remove tag" delve-minor-mode-tag-remove)
    ("." "Set ID"     org-id-get-create)
    ("q" "Quit" transient-quit-one)])

(transient-define-prefix delve-minor-mode-actions ()
  "Transient top prefix for all actions."
  [["Add node to a collection"
    ("c" "Collect or find node(s)"  delve-minor-mode-collect-actions)
    ("i" "Inspect node(s)"  delve-minor-mode-inspect-actions)]
   ["Edit node(s)"
    ("e" "Edit node at point"     delve-minor-mode-edit-actions)]
   ["           "  ;; fake column
    ""]
   ["Quit"
    ("q" "Quit" transient-quit-one)]])

;;; * Minor Mode Definition

(defvar delve-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map delve-minor-mode-prefix-key #'delve-minor-mode-actions)
    map)
  "Local map for the delve minor mode.")

(define-minor-mode delve-minor-mode
  "Local minor mode to collect org roam node via Delve."
  :lighter " DelveMM"
  :group 'delve
  :keymap 'delve-minor-mode-map
  :require 'delve)

(defun delve--maybe-activate-minor-mode ()
  "Turn on delve minor mode if current buffer is in Org Roam."
  (interactive)
  (when (and (buffer-file-name)
             (org-roam-file-p))
    (delve-minor-mode +1)))

(defun delve-minor-mode--mass-activate (&optional deactivate)
  "Activate or deactivate Delve minor mode in all Org Roam buffers.
Activate the mode unless DEACTIVATE is non-nil."
  (mapcar (lambda (buf)
            (with-current-buffer buf
              (when (and (buffer-file-name)
                         (org-roam-file-p))
                (delve-minor-mode (if deactivate -1 +1)))))
          (buffer-list)))

;;;###autoload
(define-minor-mode delve-global-minor-mode
  "Add some Delve functionality in Org Roam files."
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
