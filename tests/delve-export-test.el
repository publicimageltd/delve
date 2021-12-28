;;; delve-export-test.el --- Tests for the export module  -*- lexical-binding: t; -*-

;; Copyright (C) 2021

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

;; Tests for the module delve-export.

;;; Code:
(require 'buttercup)
(require 'cl-lib)
(require 'delve-export)

(cl-defstruct delve-export-test-struct
  "Test struct."
  slot1 slot2 slot3)

(describe "delve-export--struct-to-plist"
  :var ((struct (make-delve-export-test-struct :slot1 "slot1"
                                               :slot2 :key
                                               :slot3 "slot3")))
  (it "returns a plist with all slots"
    (expect (delve-export--struct-to-plist struct)
            :to-equal
            '(:slot1 "slot1" :slot2 :key :slot3 "slot3")))
  (it "optionally exludes slots"
    (expect (delve-export--struct-to-plist struct '(slot2))
            :to-equal
            '(:slot1 "slot1" :slot3 "slot3")))
  (it "ignores non-existing slots for exclusion"
    (expect (delve-export--struct-to-plist struct '(:slot123))
            :to-equal
            '(:slot1 "slot1" :slot2 :key :slot3 "slot3")))
  (it "returns nil when called with nil"
    (expect (delve-export--struct-to-plist nil)
            :to-be nil)))

(describe "delve-export--merge-plist"
  :var ((plist '(:eins "1" :zwei "2" :drei "3")))

  (it "throws an error if list to be merged is malformed"
    (expect (delve-export--merge-plist plist '(:single-key))
            :to-throw))
  (it "returns plist when called with (plist nil)"
    (expect (delve-export--merge-plist plist nil)
            :to-equal plist))
  (it "returns plist when called with (nil plist)"
    (expect (delve-export--merge-plist nil plist)
            :to-equal plist))
  (it "merges plist with different keys"
    (expect (delve-export--merge-plist plist '(:vier "4"))
            :to-have-same-items-as
            (append plist '(:vier "4"))))
  (it "overwrites values when merging identical keys"
    (expect (delve-export--merge-plist plist '(:eins "neu1"))
            :to-have-same-items-as
            '(:eins "neu1" :zwei "2" :drei "3"))))

(describe "delve-export--merge-plists"
  :var ((plist '(:eins "1" :zwei "2")))
  (it "returns plist when called with no further arguments"
    (expect (delve-export--merge-plists plist)
            :to-equal plist))
  (it "merges one property list with non-identical keys"
    (expect (delve-export--merge-plists plist '(:drei "3"))
            :to-have-same-items-as (append plist '(:drei "3"))))
  (it "merges two property lists with non-identical keys"
    (expect (delve-export--merge-plists plist '(:drei "3") '(:vier "4"))
            :to-have-same-items-as (append plist '(:drei "3" :vier "4"))))
  (it "overwrites values when merging identical keys"
    (expect (delve-export--merge-plists plist '(:eins "neu1") '(:zwei "neu2"))
            :to-equal '(:eins "neu1" :zwei "neu2"))))

(describe "delve-export--value-or-fn"
  (it "returns nil when called with nil value"
    (expect (delve-export--value-or-fn nil)
            :to-be nil))
  (it "returns a string when called with a string value"
    (expect (delve-export--value-or-fn "string")
            :to-equal "string"))
  (it "returns an integer when called with an integer value"
    (expect (delve-export--value-or-fn 1000)
            :to-equal 1000))
  (it "returns the function result when called with a function name"
    (expect (delve-export--value-or-fn #'identity "result")
            :to-equal "result"))
  (it "returns the function result when called with a lambda object"
    (expect (delve-export--value-or-fn (lambda (arg) arg) "result")
            :to-equal "result")))

(describe "delve-export--process-special-values"
  :var ((options `(:name "name" :value "value" :fn ,(lambda (o) (plist-get o :name)))))
  (it "returns nil when called with plist nil"
    (expect (delve-export--process-special-values nil)
            :to-be nil))
  (it "ignores wrong key"
    (expect (delve-export--process-special-values options :quatsch)
            :to-equal options))
  (it "leaves string value unchanged"
    (expect (plist-get (delve-export--process-special-values options :value) :value)
            :to-equal "value"))
  (it "replaces value with result of fn"
    (expect (plist-get (delve-export--process-special-values options :fn) :fn)
            :to-equal "name"))
  (it "accepts multiple keys"
    (let ((new-options (delve-export--process-special-values options :value :fn)))
      (expect (plist-get new-options :value) :to-equal "value")
      (expect (plist-get new-options :fn)    :to-equal "name"))))


(describe "delve-export--item-string"
  (it "ignores nil object"
    (expect (delve-export--item-string nil nil)
            :to-be nil))
  (it "calls printer associated with object type"
    (expect (delve-export--item-string "string"
                                       `(:printers ((string . ,(lambda (obj _) obj)))))
            :to-equal "string"))
  (it "passes options to printer"
    (expect (delve-export--item-string "string" `(:result "result"
                                                          :printers ((string . ,(lambda (_ opt) (plist-get opt :result))))))
            :to-equal "result"))
  (it "ignores object with no associated printer"
    (expect (delve-export--item-string "string" `(:printers ((integer . ,(lambda (_ __) "should not happen!")))))
            :to-be nil))
  (it "ignores printer returning nil value"
    (expect (delve-export--item-string "string" `(:printers ((string . ,(lambda (_ __) nil)))))
            :to-be nil))
  (it "ignores separator"
    (expect (delve-export--item-string "string" `(:separator "\n" :printers ((string . ,(lambda (obj _) obj)))))
            :to-equal "string")))

(describe "delve-export--insert"
  :var (buf)
  (before-each
    (setq buf (get-buffer-create "NEWBUF"))
    (switch-to-buffer buf))
  (after-each
    (kill-buffer buf))

  (it "does nothing when called with no objects and no backend"
    (delve-export--insert buf nil nil)
    (expect (buffer-string) :to-equal ""))
  (it "overrides backend value :header when passed as extra-option"
    (let ((backend (delve-export-backend-create :header "header")))
      (delve-export--insert buf backend nil '(:header "test"))
      (expect (buffer-string) :to-equal "test")))
  (it "overrides backend slots with backend slot 'options'"
    (let ((backend (delve-export-backend-create :header "header" :options '(:header "test"))))
      (delve-export--insert buf backend nil)
      (expect (buffer-string) :to-equal "test")))
  (it "finally overrides everything with value in extra options"
    (let ((backend (delve-export-backend-create :header "header" :options '(:header "test"))))
      (delve-export--insert buf backend nil '(:header "anothertest"))
      (expect (buffer-string) :to-equal "anothertest")))
  (it "throws when assertion function returns nil"
    (expect (delve-export--insert buf nil nil `(:assert ,#'ignore))
            :to-throw))
  (it "assertion is executed with buf set current"
    (delve-export--insert buf nil nil `(:assert ,(lambda () (insert "test") t)))
    (expect (buffer-string) :to-equal "test"))
  (it "inserts header string"
    (delve-export--insert buf nil nil `(:header "header"))
    (expect (buffer-string) :to-equal "header"))
  (it "inserts footer string"
    (delve-export--insert buf nil nil `(:footer "footer"))
    (expect (buffer-string) :to-equal "footer"))
  (it "inserts item"
    (delve-export--insert buf nil '("string") `(:printers ((string . ,(lambda (obj _) obj)))))
    (expect (buffer-string) :to-equal "string"))
  (it "inserts two items with one separator"
    (delve-export--insert buf nil '("string" "string") `(:printers ((string . ,(lambda (obj _) obj)))
                                                                   :separator "-"))
    (expect (buffer-string) :to-equal "string-string"))
  (it "inserts two items, one footer with two separators"
    (delve-export--insert buf nil '("string" "string") `(:footer "footer"
                                                                 :printers ((string . ,(lambda (obj _) obj)))
                                                                 :separator "-"))
    (expect (buffer-string) :to-equal "string-string-footer"))
  (it "encloses two items with header and footer, with 3 separators"
    (delve-export--insert buf nil '("a" "b") `(:footer "footer" :header "header"
                                                       :printers ((string . ,(lambda (obj _) obj)))
                                                       :separator "-"))
    (expect (buffer-string) :to-equal "header-a-b-footer"))
  (it "passes # of items to options value :n-total"
    (delve-export--insert buf nil '("a" "b" "c") `(:printers ((string . ,(lambda (_ opt) (format "%s" (plist-get opt :n-total)))))))
    (expect (buffer-string) :to-equal "333"))
  (it "uses header function result, if passed"
    (delve-export--insert buf nil nil `(:header ,(lambda (_) "header")))
    (expect (buffer-string) :to-equal "header"))
  (it "passes options if calling header fn"
    (delve-export--insert buf nil nil `(:value "value" :header ,(lambda (opt) (plist-get opt :value))))
    (expect (buffer-string) :to-equal "value"))
  (it "uses footer function result, if passed"
    (delve-export--insert buf nil nil `(:footer ,(lambda (_) "footer")))
    (expect (buffer-string) :to-equal "footer"))
  (it "passes options if calling footer fn"
    (delve-export--insert buf nil nil `(:value "value" :footer ,(lambda (opt) (plist-get opt :value))))
    (expect (buffer-string) :to-equal "value")))


(provide 'delve-export-test)
;;; delve-export-test.el ends here
