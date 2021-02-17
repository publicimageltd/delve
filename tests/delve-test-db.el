;;; delve-test-db.el --- Test delve's database functions   -*- lexical-binding: t; -*-

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

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Tests the interaction with the database.

;;; Notes for writing tests:

;; - The database will be created from tests/note-files. Once a test
;;   is run, you can query the database in
;;   /tmp/note-files-last-test-run/org-roam.db directly to see what
;;   your lisp code should return.
;;
;; - Some executable org mode babel blocks in note-files/structure.org
;;   already provide some basic insights by calling sqlite3 directly
;;   with some queries.
;;
;; - To force the creation of the database without first running this
;;   test suite, manually set the variable "no-tests" to true in this
;;   file and then run the test (or execute this file). This will
;;   create the database in /tmp/note-files-last-test-run, as if a
;;   test had been run.

;;; Code:

(require 'delve-test-utils)
(require 'delve-db)

(defvar no-tests nil
  "Set this to t if you just want to create the database.")

;; * Optionally create the database w/o testing

(when no-tests
  (delve-test-setup-db)
  (delve-test-teardown-db)
  ;; you don't run this test interactively within emacs, do you?
  (kill-emacs))

;; * Tests

(describe "delve--flatten"
  (it "flattens a nested list"
    (expect (delve--flatten '(1 2 (3 4) 5))
	    :to-equal
	    '(1 2 3 4 5)))
  (it "removes nil values when flattening"
    (expect (delve--flatten '(1 2 (3 nil) nil))
	    :to-equal
	    '(1 2 3)))
  (it "returns nil when passed nil"
    (expect (delve--flatten nil)
	    :to-be nil)))

(describe "delve-db-rearrange"
  (it "rearranges using position index"
    (expect (delve-db-rearrange [1 0] '((a b) (a b)))
	    :to-equal
	    '((b a) (b a))))
  (it "discards non-indexed values when rearranging"
    (expect (delve-db-rearrange [0] '((a b c) (a b c)))
	    :to-equal
	    '((a) (a))))
  (it "rearranges and applies functions with arity 1"
    (expect (delve-db-rearrange [1 (0 1+)] '((1 0) (1 0)))
	    :to-equal
	    '((0 2) (0 2))))
  (it "rearranges and applies functions with anaphoric argument 'it'"
    (expect (delve-db-rearrange [1 (0 (1+ it))] '((1 0) (1 0)))
	    :to-equal
	    '((0 2) (0 2))))
  (it "adds keyword to the rearranged items:"
    (expect (delve-db-rearrange [:count 1] '((0 20) (1 87)))
	    :to-equal
	    '((:count 20) (:count 87))))
  (it "adds keywords to the rearranged items:"
    (expect (delve-db-rearrange [:count 1 :yes 0] '((0 20) (1 87)))
	    :to-equal
	    '((:count 20 :yes 0) (:count 87 :yes 1))))
  (it "it passes strings as-is when rearranging:"
    (expect (delve-db-rearrange [:count 1 :string \"hi\"] '((0 20) (1 87)))
	    :to-equal
	    '((:count 20 :string \"hi\") (:count 87 :string \"hi\")))))

(describe "Catching malformed queries"
  (before-all
    (delve-test-setup-db))
  (after-all
    (delve-test-teardown-db))

  (describe "delve-db-safe-query"
    :var ((query [:select * :from thistabledoesnotexist]))
      (it "throws no error if query is invalid"
	(expect (let ((inhibit-message t)
		      (debug-on-error nil))
		  (delve-db-safe-query query))
		:not :to-throw))
      (it "sets delve-db-there-were-errors after an error"
	(expect delve-db-there-were-errors :not :to-be nil))
      (it "has created an error buffer"
	(expect (get-buffer delve-db-error-buffer)
		:not :to-be nil))
      (it "inserts the malformed query in the error buffer"
	(expect (with-current-buffer (get-buffer delve-db-error-buffer)
		  (buffer-string))
		:to-match
		(concat ".*" (regexp-quote (format "%s" query)))))
      (it "returns results as a list of lists"
	(expect (delve-db-safe-query [:select "Hallo"])
		:to-equal
		'(("Hallo"))))))

(describe "Counting queries"
  (before-all
    (delve-test-setup-db))
  (after-all
    (delve-test-teardown-db))
    
  (describe "delve-db-count-tag"
    (it "returns 0 if no tag is found"
      (expect (delve-db-count-tag "diesertagexistiertnicht")
	      :to-be 0))
    (it "returns correct count for all tags"
      (expect 
       (seq-map #'delve-db-count-tag '("tag1" "tag2" "Bignote"))
       :to-equal
       '(2 2 1))))

  (describe "delve-db-count-backlinks"
    (it "counts backlinks for file reference.org"
      (expect
       (delve-db-count-backlinks (delve-test-get-file "reference.org"))
       :to-be 2))
    (it "counts backlinks for file with-meta.org"
      (expect
       (delve-db-count-backlinks (delve-test-get-file "with-meta.org"))
       :to-be 0))
    (it "counts backlinks for file without-meta.org"
      (expect
       (delve-db-count-backlinks (delve-test-get-file "without-meta.org"))
       :to-be 2))
    (it "counts backlinks for file reference2.org"
      (expect
       (delve-db-count-backlinks (delve-test-get-file "reference2.org"))
       :to-be 1)))

  (describe "delve-db-count-tolinks"
    (it "counts tolinks for file reference.org"
      (expect
       (delve-db-count-tolinks (delve-test-get-file "reference.org"))
       :to-be 1))
    (it "counts tolinks for file with-meta.org"
      (expect
       (delve-db-count-tolinks (delve-test-get-file "with-meta.org"))
       :to-be 3))
    (it "counts tolinks for file without-meta.org"
      (expect
       (delve-db-count-tolinks (delve-test-get-file "without-meta.org"))
       :to-be 0))
    (it "counts tolinks for file reference2.org"
      (expect
       (delve-db-count-tolinks (delve-test-get-file "reference2.org"))
       :to-be  1))))

(describe "Queries returning plain items"
  (before-all
    (delve-test-setup-db))
  (after-all
    (delve-test-teardown-db))

   (describe "delve-db-plain-roam-tags"
     (it "returns all #+ROAM_TAGS in a plain list"
       (expect (delve-db-plain-roam-tags)
	       :to-have-same-items-as
	       '("tag1" "tag2" "Bignote")))))

(describe "Queries returning delve objects"
  (before-all
    (delve-test-setup-db))
  (after-all
    (delve-test-teardown-db))

  (describe "delve-db-get-page-from-file"
    :var (result file-name)
    (before-all
      (let ((inhibit-message t))
	(setq file-name (delve-test-get-file "reference.org"))
	(setq result (delve-db-get-page-from-file file-name))))

    (it "returns a non-nil value"
      (expect result :not :to-be nil))
    (it "returns a zettel object"
      (expect (delve-zettel-p result)))
    (it "returns a page object"
      (expect (delve-page-p result)))
    (it "returns a page with the correct file name"
      (expect (delve-page-file result)
	      :to-match
	      file-name)))
  
  (describe "delve-db-query-roam-tags"
    (it "returns all #+ROAM_TAGS as delve objects"
      (let* ((result (delve-db-query-roam-tags)))
	(dolist (obj result)
	  (expect (delve-tag-p obj) :to-be-truthy))))
    (it "returns the correct count with each tag object"
      (let* ((result (delve-db-query-roam-tags)))       
	(expect (mapcar (lambda (it) (delve-tag-count it)) result)
		:to-have-same-items-as '(2 2 1)))))

  (describe "delve-db-query-pages-with-tag"
    (describe "for all pages tagged with 'tag1'"
      :var (result)
      (before-all
	(let ((inhibit-message t))
	  (setq result (delve-db-query-pages-with-tag "tag1"))))
      
      (it "returns a non-nil value"
	(expect result :not :to-be nil))
      (it "returns a list of pages"
	(dolist (page result)
	  (expect (delve-page-p page) :to-be-truthy)))
      (it "returns the correct pages"
	(expect (mapcar (lambda (it) (delve-page-file it))
			result)
		:to-equal
		(mapcar #'delve-test-get-file
			'("reference.org" "reference2.org"))))))
  
  (describe "delve-db-query-pages-matching-title"
    (describe "for all pages matching 'Reference'"
      :var (result)
      (before-all
	(let ((inhibit-message t))
	  (setq result (delve-db-query-pages-matching-title "Reference"))))

      (it "returns a non-nil value"
	(expect result :not :to-be nil))
      (it "returns a list of pages"
	(dolist (page result)
	  (expect (delve-page-p page) :to-be-truthy)))
      (it "returns the correctly titled pages"
	(dolist (page result)
	  (expect (delve-page-title page)
		  :to-match
		  "Reference")))))
  
  (describe "delve-db-query-backlinks"
    (describe "backlinks for 'reference.org'"
      :var (result file-name)
      (before-all
	(let ((inhibit-message t))
	  (setq file-name (delve-test-get-file "reference.org"))
	  (setq result (delve-db-query-backlinks
			(delve-db-get-page-from-file file-name)))))
      (it "has found the right file"
	(expect file-name :to-match "reference.org$"))
      (it "returns a non-nil value"
	(expect result :not :to-be nil))
      (it "returns exact 2 items"
	(expect (length result) :to-be 2))
      (it "returns zettel objects"
	(dolist (zettel result)
	  (expect (delve-zettel-p zettel)
		  :to-be-truthy)))))
  
  (describe "delve-db-query-tolinks"
    (describe "tolinks for 'with-meta.org'"
      :var (result file-name)
      (before-all
	(let ((inhibit-message t))
	  (setq file-name (delve-test-get-file "with-meta.org"))
	  (setq result (delve-db-query-tolinks
			(delve-db-get-page-from-file file-name)))))

      (it "returns a non-nil value"
	(expect result :not :to-be nil))
      (it "returns exact 3 items"
	(expect (length result) :to-be 3))
      (it "returns zettel objects"
	(dolist (zettel result)
	  (expect (delve-zettel-p zettel)
		  :to-be-truthy)))))

  (describe "delve-db-query-sort-by-mtime"
    :var (zettel-list)
    (before-all
      (setq zettel-list
	    (cl-loop for i from 1 to 5
		     do (sleep-for 0.1)
		     collect (delve-make-zettel  :title (format "%d" i)
						 :mtime (current-time)))))
    
    (it "sorts zettel by mtime, last one first"
      (expect (mapcar #'delve-zettel-title
		      (delve-db-query-sort-by-mtime zettel-list))
	      :to-equal
	      '("5" "4" "3" "2" "1"))))
      
  (describe "delve-db-query-last-10-modified"))

  
(provide 'delve-test-db)
;;; delve-test-db.el ends here
