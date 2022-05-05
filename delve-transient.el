;;; delve-transient.el --- transient keymaps for Delve  -*- lexical-binding: t; -*-

;; Copyright (C) 2021

;; Author:  <joerg@joergvolbers.de>
;;
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

;; Define transient keymaps.

;;; Code:

(require 'transient)
(require 'dash)

;; * Generic Utilities for working with Transients

(defun delve-transient--split-switch (s)
  "Return S `--val=key' as a list with value-key-pair."
  (let* ((re (rx (*? blank)
                 "--" (group-n 1 (+? (not "=")))
                 "="  (group-n 2 (* (not blank)))
                 (*? blank))))
    (when (string-match re s)
      (list (intern (concat ":" (match-string 1 s)))
            (match-string 2 s)))))

(defun delve-transient--args-to-plist (args)
  "Return transient ARGS with --val=key pairs as a property list."
  (-flatten (-map #'delve-transient--split-switch (-flatten args))))

;; * Define infix delve-transient-switches for switching between options.

(defclass delve-transient-switches (transient-option)
  ((choices        :initarg :choices)        ;; return value
   (pretty-choices :initarg :pretty-choices) ;; display value
   (allow-nil      :initarg :allow-nil :initform t) ;; allow unsetting?
   (always-read    :initform t)
   (reader         :initform #'delve-transient--toggle-reader))
  "Transient switch for mutually exclusive values.
The transient returns the value from `:choices', but presents to
the user the corresponding value (same index) from
`:pretty-choices'. Per default, the user can switch between these
values and then unset the whole list (which will return a nil
value).  Set `:allow-nil' to nil to prevent nil values.")

(defun delve-transient--initial-switch-value (obj)
  "Return the initial value for switch OBJ.
OBJ must be an instance of `delve-transient-switches'."
  (and (not (oref obj allow-nil)) (elt (oref obj choices) 0)))

(cl-defmethod transient-init-value ((obj delve-transient-switches))
  "Set initial value for switch OBJ."
  (oset obj value (delve-transient--initial-switch-value obj)))

(cl-defmethod transient-format-value ((obj delve-transient-switches))
  "Format the value list of OBJ."
  (let* ((value (oref obj value))
         (choices (oref obj choices))
         (n 0))
    (mapconcat (lambda (pretty-choice)
                 (cl-incf n)
                 (propertize pretty-choice
                             'face (if (equal value (elt choices (1- n)))
                                       'transient-value
                                     'transient-inactive-value)))
               (oref obj pretty-choices)
               "|")))

(defun delve-transient--toggle-reader (&rest _)
  "Shift value to the next value.
Move forward to the next element in slot `choices', wrapping
around if the end of the list is reached.  If slot `allow-nil' is
non-nil, return nil after the last choice and move to the first
choice after nil."
  (let* ((obj      (transient-suffix-object))
         (value    (oref obj value))
         (choices  (oref obj choices)))
    ;; move from nil -> first value
    (if (not value)
        (elt choices 0)
      ;; move from any value -> next value
      (let ((n (1+ (-elem-index value choices))))
        (if (< n (length choices))
            (elt choices n)
          ;; end of list? either unset or go back to first value
          (delve-transient--initial-switch-value obj))))))


(provide 'delve-transient)
;;; delve-transient.el ends here
