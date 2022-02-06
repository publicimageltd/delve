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
    (expect (delve-export--merge-plist  plist '(:vier "4"))
            :to-have-same-items-as
            (append plist '(:vier "4"))))
  (it "overwrites values when merging identical keys"
    (expect (delve-export--merge-plist plist '(:eins "neu1"))
            :to-have-same-items-as
            '(:eins "neu1" :zwei "2" :drei "3")))
  (it "uses fn in merge-alist to construct result"
    (expect (delve-export--merge-plist plist '(:eins "neu") '((:eins . concat)))
            :to-equal '(:eins "1neu" :zwei "2" :drei "3"))
    (expect (delve-export--merge-plist '(:eins ((:hans . "dampf")))
                                       '(:eins ((:grete . "mampf")))
                                       '((:eins . append)))
            :to-equal '(:eins ((:hans . "dampf") (:grete . "mampf")))))
  (it "ignores key in merge-alist which are not present in any plist"
    (expect (delve-export--merge-plist plist '(:eins "neu") '((:hans . dampf)))
            :to-equal '(:eins "neu" :zwei "2" :drei "3"))))

(describe "delve-export--merge-plists"
  :var ((plist '(:eins "1" :zwei "2")))
  (it "returns plist when called with no further arguments"
    (expect (delve-export--merge-plists nil plist)
            :to-equal plist))
  (it "merges one property list with non-identical keys"
    (expect (delve-export--merge-plists nil plist '(:drei "3"))
            :to-have-same-items-as (append plist '(:drei "3"))))
  (it "merges two property lists with non-identical keys"
    (expect (delve-export--merge-plists nil plist '(:drei "3") '(:vier "4"))
            :to-have-same-items-as (append plist '(:drei "3" :vier "4"))))
  (it "overwrites values when merging identical keys"
    (expect (delve-export--merge-plists nil plist '(:eins "neu1") '(:zwei "neu2"))
            :to-equal '(:eins "neu1" :zwei "neu2")))
  (it "merges using fn associated with a key"
    (expect (delve-export--merge-plists '((:eins . concat)) plist '(:eins "0"))
            :to-equal '(:eins "10" :zwei "2"))))

(describe "delve-export--merge-alist"
  :var (alist)
  (before-each
    (setq alist '((eins . "eins") (zwei . "zwei") (drei . "drei"))))
  (it "returns nil when called with nil"
    (expect (delve-export--merge-alist nil nil)
            :to-be nil))
  (it "returns alist unchanged when merging with nil"
    (expect (delve-export--merge-alist alist nil)
            :to-equal alist))
  (it "returns alist when called with (nil alist)"
    (expect (delve-export--merge-alist nil alist)
            :to-have-same-items-as alist))
  (it "replaces value when key is found in original list"
    (expect (delve-export--merge-alist alist '((zwei . "neuzwei")))
            :to-equal '((eins . "eins") (zwei . "neuzwei") (drei . "drei"))))
  (it "adds key-value-pair when key is not found in original list"
    (expect (delve-export--merge-alist alist '((vier . "vier")))
            :to-have-same-items-as (append alist '((vier . "vier"))))))

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


(describe "delve-export--get-backend-by-name"
  :var ((backends (list (delve-export-backend-create :name 'eins
                                                     :header "eins")
                        (delve-export-backend-create :name 'zwei
                                                     :header "zwei")
                        (delve-export-backend-create :name 'eins
                                                     :header "drei"))))
  (it "returns first backend matching the name"
    (expect (delve-export-backend-header (delve-export--get-backend-by-name 'eins backends))
            :to-equal "eins"))
  (it "returns nil when there are no backends to choose from"
    (expect (delve-export--get-backend-by-name 'eins nil)
            :to-be nil))
  (it "returns nil when there is no matching backend"
    (expect (delve-export--get-backend-by-name 'vier backends)
            :to-be nil)))

(describe "Finding parents"
  :var* ((backends (list (delve-export-backend-create :name 'eins
                                                      :separator "sep-1"
                                                      :header "eins"
                                                      :printers '((type1 . fn1-1)
                                                                  (type2 . fn2-1)))
                        (delve-export-backend-create :name 'zwei
                                                     :parent 'eins
                                                     :separator "sep-2"
                                                     :printers '((type2 . fn2-2)
                                                                 (type3 . fn3-2))
                                                     :header "zwei")
                        (delve-export-backend-create :name 'drei
                                                     :separator "sep-3"
                                                     :parent 'zwei
                                                     :printers '((type2 . fn2-3))
                                                     :header "drei")))
         (eins (nth 0 backends))
         (zwei (nth 1 backends))
         (drei (nth 2 backends)))
  (describe "delve-export--get-parent"
    (it "returns nil if there are no backends to choose from"
      (expect (delve-export--get-parent drei nil)
              :to-be nil))
    (it "returns nil if there is no instance given"
      (expect (delve-export--get-parent nil backends)
              :to-be nil))
    (it "returns nil if instance has no parent"
      (expect (delve-export--get-parent eins backends)
              :to-be nil))
    (it "returns parent if instance has one parent"
      (expect (delve-export--get-parent zwei backends)
              :to-equal eins)))

  (describe "delve-export--get-parent-backends"
    (it "returns nil if there are no backends to choose from"
      (expect (delve-export--get-parent-backends eins nil)
              :to-be nil))
    (it "returns nil if there is no instance given"
      (expect (delve-export--get-parent-backends nil backends)
              :to-be nil))
    (it "returns nil if instance has no parent"
      (expect (delve-export--get-parent-backends eins backends)
              :to-be nil))
    (it "returns list of parent if instance has one parent"
      (expect (delve-export--get-parent-backends zwei backends)
              :to-equal (list eins)))
    (it "returns list of two parents (from closer to more distanced) if instance has two parents"
      (expect (delve-export--get-parent-backends drei backends)
              :to-equal (list eins zwei))))

  (describe "delve-export--backend-as-plist"
    (it "returns nil when instance is nil"
      (expect (delve-export--backend-as-plist nil backends)
              :to-be nil))
    (it "returns backend as plist when there is no list of backends to choose from"
      (expect (delve-export--backend-as-plist eins nil)
              :to-have-same-items-as
              '(:name eins
                      :separator "sep-1"
                      :header "eins"
                      :footer nil
                      :printers ((type1 . fn1-1)
                                 (type2 . fn2-1))
                      :assert nil
                      :description nil
                      :parent nil)))
    (it "returns backend with no parents as plist"
      (expect (delve-export--backend-as-plist eins backends)
              :to-have-same-items-as
              '(:name eins
                      :separator "sep-1"
                      :header "eins"
                      :footer nil
                      :printers ((type1 . fn1-1)
                                 (type2 . fn2-1))
                      :assert nil
                      :description nil
                      :parent nil)))
    (it "returns backend with one parent as plist with inheritance"
      (expect (delve-export--backend-as-plist zwei backends)
              :to-have-same-items-as
              '(:name zwei
                      :separator "sep-2"
                      :header "zwei"
                      :footer nil
                      ;; FIXME This is too static; we should
                      ;; change for items only, assuming no order
                      :printers ((type3 . fn3-2)
                                 (type1 . fn1-1)
                                 (type2 . fn2-2))
                      :assert nil
                      :description nil
                      :parent eins)))
    (it "returns backend with two parents as plist with normal inheritance"
      (expect (delve-export--backend-as-plist drei backends)
              :to-have-same-items-as
              '(:name drei
                      :separator "sep-3"
                      :header "drei"
                      :footer nil
                      :printers ((type1 . fn1-1)
                                 (type2 . fn2-3)
                                 (type3 . fn3-2))
                      :assert nil
                      :description nil
                      :parent zwei)))))

(describe "delve-export--item-string"
  (it "ignores nil object"
    (expect (delve-export--item-string nil nil)
            :to-be nil))
  (it "calls printer associated with object type"
    (expect (delve-export--item-string "string"
                                       `(:printers ((string . ,(lambda (obj _) obj)))))
            :to-equal "string"))
  (it "passes internal option list to printer"
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
