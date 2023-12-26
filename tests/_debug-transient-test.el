;;; debug-transient-test.el --- debug non-interactive testing of transients  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023

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

;; 

;;; Code:
(require 'transient)

(defvar return-value nil)

(defun test-transient--store-value ()
  (interactive)
  (push (transient-get-value) return-value))

(transient-define-argument test-transient--switches ()
  :description "Toggle"
  :class 'transient-switches
  :key "t"
  :argument-format "--%s"
  :argument-regexp "\\(--\\(grape\\|orange\\|cherry\\|lime\\)\\)"
  :choices '("grape" "orange" "cherry" "lime"))

(transient-define-prefix test-transient ()
  "Test"
  ["Groupname"
   (test-transient--switches)
   ("p" "store value" test-transient--store-value :transient t)
   ("q" "quit" transient-quit-all)])

(progn
  (setq return-value nil)
  (message "Calling transient")
  (funcall 'test-transient)
  (message "Inserting keys")
  (execute-kbd-macro (kbd "ttpq"))
  (message "Result:%S" return-value))

(provide 'debug-transient-test)
;;; debug-transient-test.el ends here
