;;; delve-test-db.el --- Test delve's database functions   -*- lexical-binding: t; -*-

;; Copyright (C) 2021  

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

;; To see what results to expect, see note-files/structure.org
;;
;; To create a database without running the test, set the variable
;; "no-tests". Then you can query the database located in
;; /tmp/note-files-last-test-run/org-roam.db
;;
;;; Code:

(require 'delve-test-utils)
(require 'delve-db)

(defvar no-tests nil
  "Set this to t if you just want to create the database.")

;; * Optionally create the database w/o teting

(when no-tests
  (delve-test-setup-db)
  (delve-test-teardown-db)
  (kill-emacs))

;; * Tests

(describe "Specific DB queries"
  (before-all
    (delve-test-setup-db))
  (after-all
    (delve-test-teardown-db))
  ;;
  (it "returns all #+ROAM_TAGS in a plain list"
    (expect (delve-db-plain-roam-tags)
	    :to-have-same-items-as
	    '("tag1" "tag2" "Bignote")))
  (it "returns all #+ROAM_TAGS as delve objects"
    (let* ((result (delve-db-query-roam-tags)))
      (dolist (obj result)
	(expect (delve-tag-p obj) :to-be-truthy))
      (expect (sort (mapcar (lambda (it) (delve-tag-count it)) result) #'>)
	      :to-equal
	      ;; 2 times "tag1"; 2 times "tag2"; 1 time "Bignote"
	      '(2 2 1))))
  (it "counts tags using delve-db-count-tag"
    (expect 
     (seq-map #'delve-db-count-tag '("tag1" "tag2" "Bignote"))
     :to-equal
     '(2 2 1)))
  (it "counts file backlinks using delve-db-count-backlinks (no IDs yet!)"
    (expect
     (delve-db-count-backlinks (delve-test-get-file "reference.org"))
     :to-be
     2)
    (expect
     (delve-db-count-backlinks (delve-test-get-file "reference2.org"))
     :to-be
     1)))

(provide 'delve-test-db)
;;; delve-test-db.el ends here
