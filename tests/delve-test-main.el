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

;; * Utilities

(defmacro with-buf-bound-by  (sym buf-fn &rest body)
  (declare (indent 2))
  `(let* ((,sym ,buf-fn))
     (with-current-buffer ,sym
       ,@body)
     (kill-buffer ,sym)))

(defun delve-test--string-properties (s)
  "Return a list of all properties of S, with char indices.
The list is simlar to the output of `prin1', with the string
itself ommitted."
  (let* ((s-prin1 (format "%S" s)))
    (if (string-prefix-p "#(" s-prin1)
	(cdr (read (substring s-prin1 1)))
      nil)))

(defun delve-test--merged-props (s)
  "Return a list of all properties of S, disregarding indices."
  (seq-uniq (cl-remove-if-not #'listp (delve-test--string-properties s))))

(defun delve-test-fake-page (&optional title)
  "Return a fake page object with no significant data."
  (delve-make-page  :title  (if title (format "%s" title) "Fake-Title")
		     :file  "dummyfile.org"
		     :tags '("tag1" "tag2")
		     :mtime (current-time)
		     :atime (current-time)
		     :backlinks 0
		     :tolinks 0))

;; * The Specs

(describe "Expanding"

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
	      '("a-ITEM" "b-ITEM"))))
  
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
    
    (it "postprocesses the query results"
      (let ((results '("ITEM1" "ITEM2")))
	(spy-on 'delve-db-query-all-zettel :and-return-value results)
	(cl-labels ((post-process-fn (items)
				     (mapcar (apply-partially #'concat "a-") items)))
	  (expect (delve-operate-search
		   (delve-make-page-search
		    :postprocess #'post-process-fn))
		  :to-equal
		  (post-process-fn results)))))))

;; *  UI

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
      (expect 'delve-represent-error :to-have-been-called))))

;; * Collections

(describe "Collections"
  :var (some-items)
  (before-all
    (setq some-items (seq-map #'delve-test-fake-page
			      '("Zettel1" "Zettel2" "Zettel3"))))

  (describe "delve-new-collection-buffer"
    (it "returns a delve buffer if called with correct arguments"
      (with-buf-bound-by buf (delve-new-collection-buffer some-items (list "test") "testbuffer")
	(expect (type-of buf) :to-be 'buffer)
	(expect major-mode    :to-be 'delve-mode)))

    (it "sets the heading list as the list header"
      (spy-on 'lister-set-header)
      (let* ((my-heading '("My Heading")))
	(with-buf-bound-by buf (delve-new-collection-buffer some-items my-heading "testbuffer")
	  (expect 'lister-set-header :to-have-been-called-with buf my-heading))))

    (it "accepts a value of nil for the header"
      (spy-on 'lister-set-header)
      (with-buf-bound-by buf (delve-new-collection-buffer some-items nil "testbuffer")
	(expect 'lister-set-header :to-have-been-called-with buf nil)))

    (it "inserts all items of the collections"
      (let* ((n 200)
	     (many-items (seq-map #'delve-test-fake-page (number-sequence 1 n))))
	(with-buf-bound-by buf (delve-new-collection-buffer many-items nil "testbuffer")
	  (expect (length (lister-get-all-data buf)) :to-be n)))))

  (describe "delve-add-to-buffer"
    (it "adds an item to the end of an existing collection"
      (let* ((new-item (delve-test-fake-page "New Item")))
	(with-buf-bound-by buf (delve-new-collection-buffer some-items nil "collection")
	  (delve-add-to-buffer buf new-item)
	  (expect (lister-get-data buf :last)
		  :to-equal new-item))))
    (it "adds a list of items to an existing collection"
      (let* ((new-items (seq-map #'delve-test-fake-page (number-sequence 1 10))))
	(with-buf-bound-by buf (delve-new-collection-buffer some-items nil "collection")
	  (delve-add-to-buffer buf new-items)
	  (expect (lister-get-all-data buf)
		  :to-equal (append some-items new-items)))))
    (it "adds an item to an empty collection"
      (let* ((new-item (delve-test-fake-page "New Item")))
	(with-buf-bound-by buf (delve-new-collection-buffer nil nil "collection")
	  (delve-add-to-buffer buf new-item)
	  (expect (lister-get-all-data buf)
		  :to-equal (list new-item)))))
    (it "throws an error if target buffer does not exist"
      (let* ((new-item (delve-test-fake-page "New Item"))
	     (buf      (delve-new-collection-buffer nil nil "collection")))
	(kill-buffer buf)
	(expect  (delve-add-to-buffer buf new-item)
		 :to-throw)))))
		       

    
(provide 'delve-test)
;;; delve-test.el ends here
