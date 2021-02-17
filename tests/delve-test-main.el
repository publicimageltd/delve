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

(defun delve-test-fake-pages (&optional title)
  "Return a fake page object with no significant data."
  (delve-make-page  :title  (if title (format "%s" title) "Fake-Title")
		     :file  "dummyfile.org"
		     :tags '("tag1" "tag2")
		     :mtime (current-time)
		     :atime (current-time)
		     :backlinks 0
		     :tolinks 0))

;; TODO Replace this one with the ones from ./test-delve-pp.el
(defmacro delve-test--with-temp-buffer (content &rest body)
  "Evaluate BODY in a temporary buffer with CONTENT."
  (declare (debug t)
           (indent 1))
  `(with-temp-buffer
     (insert ,content)
     ,@body))

;; TODO Replace this one with the ones from ./test-delve-pp.el
(defun delve-test--face-covers-range-p (start end face)
  "Return true if every face from START to END has FACE."
  (let ((all-faces (mapcar (lambda (i) (get-text-property i 'face)) (number-sequence start end))))
    (seq-every-p (lambda (target-face)
                   (or (eq face target-face)
                       (when (consp target-face)
                         (seq-contains-p target-face face))))
                 all-faces)))

;; this is taken from
;; https://github.com/clojure-emacs/cider/blob/master/test/cider-font-lock-tests.el
(defun delve-test-face-exists-in-range-p (start end face)
  "Return true if FACE exists between START to END."
  (let ((all-faces (mapcar (lambda (i) (get-text-property i 'face)) (number-sequence start end))))
    ;; cl-some returns t now but will change to return a truthy value in the future
    (seq-some (lambda (target-face)
                (or (eq face target-face)
                    (when (consp target-face)
                      (seq-contains-p target-face face))))
              all-faces)))

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
	(delve-test--with-temp-buffer s
	  (expect (delve-test-face-exists-in-range-p
		   (point-min) (point-max)
		   'delve-tags-face)
		  :to-be-truthy)))
      (it "returns nil when no tags are given"
	(expect (delve-represent-tags (delve-make-zettel))
		:to-be nil)))))

;; * Collections

(describe "Collections"
  (describe "delve-view-collection"
    :var (some-items)
    (before-all
      (setq some-items (seq-map #'delve-test-fake-pages
				'("Zettel1" "Zettel2" "Zettel3"))))

    (it "returns a delve buffer if called with correct arguments"
      (let ((buf (delve-view-collection some-items (lambda () (list "test"))) "testbuffer")))
	(expect (type-of buf) :to-be 'buffer)
	(expect (with-current-buffer buf major-mode)
		:to-be 'delve-mode)
	(kill-buffer buf)))
    (it "uses the return value of the heading function for the list header"
      (let* ((my-heading '("My Heading"))
	     (heading-fn (lambda () my-heading))
	     (buf (delve-view-collection some-items heading-fn "testbuffer"))
	     (header (with-current-buffer buf
		       (or lister-local-header-marker
			   (buffer-substring-no-properties (+ lister-local-header-marker lister-local-left-margin)
							   (+ lister-local-header-marker
							      (1- (get-text-property lister-local-header-marker 'nchars)))))))))
      (expect (list header) :to-equal my-heading)
      (kill-buffer buf))
    (it "accepts a value of nil for the header fn"
      (let ((buf (delve-view-collection some-items nil "testbuffer")))
	(expect buf :not :to-be nil)
	(kill-buffer buf)))
    (it "inserts all items of the collections"
      (let* ((n 200)
	     (many-items (seq-map #'delve-test-fake-pages (number-sequence 1 n)))
	     (buf (delve-view-collection many-items nil "testbuffer")))
	(expect (length (lister-get-all-data buf)) :to-be n)
	(kill-buffer buf))))

    
(provide 'delve-test)
;;; delve-test.el ends here
