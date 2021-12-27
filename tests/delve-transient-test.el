;;; delve-transient-test.el --- Test delve transient stuff  -*- lexical-binding: t; -*-

;; Copyright (C) 2021

;; Author:  <joerg@joergvolbers.de>
;; Keywords: internal

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

;; Test transients.

;;; Code:
;;(require 'with-simulated-input)
(require 'buttercup)
(require 'delve-transient)
(require 'cl-lib)

;; * Ease testing of transients

(cl-defun delve-transient-test--do-transient (transient-name keys &optional (meta-prefix "."))
  "Execute kbd string sequence KEYS with TRANSIENT-NAME.
Before executing KEYS, rebuild the transient so that its results
can be inspected after execution.  For this, insert two special
commands to the transient: META-PREFIX-p and
META-PREFIX-q.  META-PREFIX-p pushes the current value of the
transient on the return value, META-PREFIX-q quits the transient."
  (let* (return-value
         (meta-p (concat meta-prefix "p"))
         (meta-q (concat meta-prefix "q")))
    ;; Assert that meta prefix keys are not in use:
    (when (ignore-errors (transient-get-suffix transient-name (kbd meta-p)))
      (error "Failed interacting with transient %s: key %s already in use" transient-name meta-p))
    (when (ignore-errors (transient-get-suffix transient-name (kbd meta-q)))
      (error "Failed interacting with transient %s: key %s already in use" transient-name meta-q))
    ;; add suffix to the transient:
    (transient-append-suffix transient-name '(0 0)
      `(,meta-p "store value" ,(lambda () (interactive) (push (transient-get-value) return-value)) :transient t))
    (transient-append-suffix transient-name '(0 0)
      `(,meta-q "quit" ,#'transient-quit-all))
    ;; do it:
    (message "Calling the transient")
    (funcall transient-name)
    (message "Keyboard %s" keys)
    (execute-kbd-macro (kbd keys))
    (message "Done calling the transient")
    ;; remove suffixes
    (transient-remove-suffix transient-name (kbd meta-p))
    (transient-remove-suffix transient-name (kbd meta-q))
    ;;
    return-value))

(cl-defun delve-transient-test--transient-and-quit (transient-name keys &optional (meta-prefix "."))
  "Execute kbd KEYS on TRANSIENT-NAME and return its value."
  (delve-transient-test--do-transient transient-name (concat keys meta-prefix "p" meta-prefix "q") meta-prefix))

;; * The actual tests

(describe "delve-transient--split-switch"
  (it "returns wellformed key value pair as a plist"
    (expect (delve-transient--split-switch "--key=value")
            :to-equal '(:key "value")))
  (it "returns missing value as empty string"
    (expect (delve-transient--split-switch "--key1=")
            :to-equal '(:key1 "")))
  (it "returns malformed string without = as nil"
    (expect (delve-transient--split-switch "--test")
            :to-be nil)))

(describe "delve-transient--args-to-plist"
  (it "returns list of key-val-pairs as a plist"
    (expect (delve-transient--args-to-plist '("--k1=v1" "--k2=v2"))
            :to-equal '(:k1 "v1" :k2 "v2")))
  (it "returns nil when called with empty list"
    (expect (delve-transient--args-to-plist nil)
            :to-be nil)))

;;; * Test class for toggling transient (delve-transient-switches)

;; TODO Before each test, we redefine the layout of this specific
;; transient, so that it can be modified without altering the
;; transient itself:
(transient-define-prefix delve-transient-test-transient ()
  "Test"
  ["Groupname"
   ("t" "Toggle" "--toggle-key=" :class delve-transient-switches :choices ("A" "B") :pretty-choices ("prettyA" "prettyB"))
   ("q" "quit" transient-quit-one)])

(xdescribe "Infix class delve-transient-switches"
  
  (describe "return value"
    (it "is nil per default"
      (expect (delve-transient-test--transient-and-quit 'delve-transient-test-transient "")
              :to-be nil))
    (it "is first value of :choice when toggling once"
      (expect (delve-transient-test--transient-and-quit 'delve-transient-test-transient "t")
              :to-equal '(("--toggle-key="A""))))
    (it "is nil when toggling choice times"
      (expect (delve-transient-test--transient-and-quit 'delve-transient-test-transient "tt")
              :to-be nil))
    (it "is first value of :choice when toggling choice+1 times"
      (expect (delve-transient-test--transient-and-quit 'delve-transient-test-transient "ttt")
              :to-equal '(("--toggle-key="A""))))))

;; (delve-transient-test--transient-and-quit 'delve-transient-test-transient "t")
;; (delve-transient-test--do-transient 'delve-transient-test-transient "t.p.p.q")

(provide 'delve-transient-test)
;;; delve-transient-test.el ends here
