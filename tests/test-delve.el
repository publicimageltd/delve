;;; test-delve.el --- Tests for delve main                -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021

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

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Tests for delve.el

;;; Code:

(require 'buttercup)
(require 'delve)
(require 'seq)

;; Utilities

;; TODO Replace them with the ones from ./test-delve-pp.el

(defmacro test-delve--test-with-temp-buffer (content &rest body)
  "Evaluate BODY in a temporary buffer with CONTENT."
  (declare (debug t)
           (indent 1))
  `(with-temp-buffer
     (insert ,content)
     ,@body))

(defun test-delve--face-covers-range-p (start end face)
  "Return true if every face from START to END has FACE."
  (let ((all-faces (mapcar (lambda (i) (get-text-property i 'face)) (number-sequence start end))))
    (seq-every-p (lambda (target-face)
                   (or (eq face target-face)
                       (when (consp target-face)
                         (seq-contains-p target-face face))))
                 all-faces)))

;; this is taken from
;; https://github.com/clojure-emacs/cider/blob/master/test/cider-font-lock-tests.el
(defun test-delve--face-exists-in-range-p (start end face)
  "Return true if FACE exists between START to END."
  (let ((all-faces (mapcar (lambda (i) (get-text-property i 'face)) (number-sequence start end))))
    ;; cl-some returns t now but will change to return a truthy value in the future
    (seq-some (lambda (target-face)
                (or (eq face target-face)
                    (when (consp target-face)
                      (seq-contains-p target-face face))))
              all-faces)))

;; (require 'delve-test-utils)
  ;; (before-all
  ;;   (delve-test-setup-db))
  ;; (after-all
  ;;   (delve-test-teardown-db))

(describe "Searching"
  (describe "page search operator"
    (it "passes the search parameters correctly to the main SQL zettel query"
      (spy-on 'delve-db-query-all-zettel)
      (let ((constraints     [:where titles:title :like "%Reference%"])
	    (args            nil)
	    (with-clause     [:with something :or :something :else])
	    (name            "Irrelevant name"))
	(delve-operate-search (delve-make-page-search :name name
						      :constraint constraints
						      :args args
						      :postprocess #'identity 
						      :with-clause with-clause))
	(expect 'delve-db-query-all-zettel
		:to-have-been-called-with
		#'delve-make-page ;; make-fn
		constraints
		args
		with-clause)))
    (it "can postprocess the query results"
      (let ((results '("ITEM1" "ITEM2")))
	(spy-on 'delve-db-query-all-zettel :and-return-value results)
	(cl-labels ((post-process-fn (items)
				     (mapcar (apply-partially #'concat "a-") items)))
	  (expect (delve-operate-search
		   (delve-make-page-search
		    :postprocess #'post-process-fn))
		  :to-equal
		  (post-process-fn results)))))))

(describe "Expansion"  
  (describe "delve-expand"
    (it "can be called with no operator"
      (expect (delve-expand "ITEM")
	      :to-be nil))
    (it "accepts operators returning lists"
      (expect (delve-expand "ITEM" #'list #'list)
	      :to-equal
	      '("ITEM" "ITEM")))
    (it "accepts operators returning single values"
      (expect (delve-expand "ITEM" #'identity #'identity)
	      :to-equal
	      '("ITEM" "ITEM")))
    (it "returns results in the order of the operator functions"
      (expect (delve-expand "ITEM"
			    (apply-partially #'concat "a-")
			    (apply-partially #'concat "b-"))
	      :to-equal
	      '("a-ITEM" "b-ITEM")))))

(describe "UI"
  (describe "the mapper"
    (it "recognizes zettel objects"
      (spy-on 'delve-represent-zettel)
      (delve-mapper (delve-make-zettel))
      (expect 'delve-represent-zettel :to-have-been-called))
    (it "recognizes tag objects"
      (spy-on 'delve-represent-tag)
      (delve-mapper (delve-make-tag))
      (expect 'delve-represent-tag :to-have-been-called))
    (it "recognizes page search objects"
      (spy-on 'delve-represent-search)
      (delve-mapper (delve-make-page-search))
      (expect 'delve-represent-search :to-have-been-called))
    (it "recognizes error objects"
      (spy-on 'delve-represent-error)
      (delve-mapper (delve-make-error))
      (expect 'delve-represent-error :to-have-been-called)))

  (xdescribe "representing a zettel object"
    (describe "delve-represent-tags"
      :var (s tags)
      (before-all
	(setq tags '("tag1" "tag2"))
	(setq s (delve-represent-tags (delve-make-zettel
				       :tags tags))))
      (it "uses delve-tags-face"
	(test-delve--test-with-temp-buffer s
	  (expect (test-delve--face-exists-in-range-p
		   (point-min) (point-max)
		   'delve-tags-face)
		  :to-be-truthy)))
      (it "returns nil when no tags are given"
	(expect (delve-represent-tags (delve-make-zettel))
		:to-be nil)))))
      

(provide 'delve-test)
;;; delve-test.el ends here
