;;; delve-narrow.el --- add narrowing to delve buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  

;; Author:  <joerg@joergvolbers.de>
;; Keywords: delve

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

;; 

;;; Code:

(require 'delve)

;; TODO This is a stub and has to be reworked.
   
(defvar delve-narrow-buffer nil)
(defvar delve-narrow-list nil)
(defvar delve-narrow-pos nil)
(defvar delve-narrow-input nil)
   
(defun delve-narrow-interactive-minibuffer-setup ()
  "Set up minibuffer for in-buffer narrowing."
  (add-hook 'post-command-hook #'delve-narrow-update nil t))

(defun delve-narrow-update ()
  "Narrow in-buffer list.
Reinsert the list items matching DELVE-NARROW-INPUT.  The complete
list is stored in DELVE-NARROW-LIST.  Reinsert at buffer position
DELVE-NARROW-POS.  Mark the minibuffer prompt if regexp is invalid."
  (setq delve-narrow-input (minibuffer-contents-no-properties))
  (lister-remove-this-level delve-narrow-buffer delve-narrow-pos)
  (delve-narrow-propertize-minibuffer-prompt 'isearch-fail t)
  (lister-insert-sequence delve-narrow-buffer
			 delve-narrow-pos
			 (or
			  (delve-narrow-reduce-list
			   delve-narrow-input
			   delve-narrow-list)
			  delve-narrow-list)))

(defun delve-narrow-reduce-list (regexp l)
  "Return items in L matching REGEXP, preserving nesting.
If REGEXP is invalid, return nil."
  (catch 'reduce
    (reverse
     (seq-reduce (lambda (acc elt)
		   (if (listp elt)
		       (if-let ((new-list (delve-narrow-reduce-list regexp elt)))
			 (cons new-list acc)
			 acc)
		     (if (delve-narrow-match-p regexp elt)
			 (cons elt acc)
		       acc)))
		 l
		 nil))))
  
(defun delve-narrow-match-p (regexp item)
  "Check if object ITEM match REGEXP.
Also higlight the minibuffer prompt if regexp is invalid."
  (condition-case err
      (string-match-p regexp
		      (cl-case (type-of item)
			(delve-zettel  (delve-zettel-title item))
			(delve-tag     (delve-tag-tag item))
			(string        item) ;; for debugging
			(t             "")))
    (error
     (ignore err) ;; silence byte compiler
     (delve-narrow-propertize-minibuffer-prompt 'isearch-fail)
     (throw 'reduce nil))))

(defun delve-narrow-propertize-minibuffer-prompt (value &optional de-propertize)
  "Add face property VALUE to the minibuffer prompt, or optionally remove it.
If option DE-PROPERTIZE is set, remove the value from the
minibuffer string, else add it."
  (with-current-buffer (window-buffer (minibuffer-window))
    (let* ((inhibit-field-text-motion t)
	   (inhibit-read-only t)
	   (fn (if de-propertize 'lister-remove-face-property 'lister-add-face-property)))
      (funcall fn (line-beginning-position) (minibuffer-prompt-end) value))))

(defun delve-narrow-sublist ()
  "Interactively narrow the sublist at point."
  (interactive)
  (unless lister-local-marker-list
    (user-error "No list to narrow"))
  (when lister-highlight-mode
    (lister-unhighlight-item))
  (pcase-let ((`(,beg ,end _ ) (lister-sublist-boundaries (current-buffer) (point))))
    ;; TODO get-all-data ignores all levels; thus they are not
    ;; reinserted. So we should ideally call get-all-data-tree
    ;; and make narrow-p recursively (if ITEM is a list, ....)
    (let* ((sublist (lister-get-all-data-tree (current-buffer) beg end)))
      (setq delve-narrow-buffer (current-buffer))
      (setq delve-narrow-pos (lister-pos-as-integer beg))
      (setq delve-narrow-input nil)
      (setq delve-narrow-list sublist))
    (minibuffer-with-setup-hook
	#'delve-narrow-interactive-minibuffer-setup
      (read-from-minibuffer "Narrow sublist: "))
    (when lister-highlight-mode
      (lister-highlight-item))))

(provide 'delve-narrow)
;;; delve-narrow.el ends here
