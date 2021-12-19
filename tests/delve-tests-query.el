;;; delve-tests-query.el --- Test delve query functions  -*- lexical-binding: t; -*-

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

;; Tests the interaction with the database.

;;; Code:
(require 'buttercup)
(require 'delve-test-db-utils)
(require 'delve-query)
(require 'delve-store)

(describe "Test reading and writing"
  :var ((file-name (concat temporary-file-directory "test.el")))
  (before-each
    (when (file-exists-p file-name)
      (delete-file file-name)))

  (describe "delve-store--write"
    (it "creates a file"
      (let ((l '("A" "B" "C" ("D" "E") 1 2 3 4)))
        (delve-store--write file-name l)
        (expect (file-exists-p file-name)
                :to-be-truthy)))
    (it "overwrites an existing file"
      (let ((l '("A" "B")))
        (delve-store--write file-name l)
        (expect (delve-store--write file-name l)
                :not :to-throw)))
    (it "creates a file which opens in emacs lisp mode"
      (let ((l '("A" "B" "C")))
        (delve-store--write file-name l)
        (let ((buf (find-file-noselect file-name)))
          (expect (buffer-local-value 'major-mode buf)
                  :to-be 'emacs-lisp-mode))))
    (it "creates a file and returns the stored object"
      (let ((l '("A" "B" "C")))
        (expect (delve-store--write file-name l)
                :to-equal l))))

  (describe "delve-store--write/delve--store-read"
    (it "read and write simple lists"
      (let ((l '("A" "B" "C" ("D" "E") 1 2 3 4)))
        (delve-store--write file-name l)
        (expect (delve-store--read file-name)
                :to-equal l)))
    (it "read and write list with 10,000 elements"
      (let ((l (number-sequence 0 100000)))
        (delve-store--write file-name l)
        (expect (delve-store--read file-name)
                :to-equal l)))
    (it "read and write lisp strings with non-ascii chars"
      (let ((l `("ÄÜÄÖÄ" "\"jkjkjkj" ""
                 ,(char-to-string (char-from-name "GREEK CAPITAL LETTER SIGMA")))))
        (delve-store--write file-name l)
        (expect (delve-store--read file-name)
                :to-equal l)))
    (it "read and write nil"
      (delve-store--write file-name nil)
      (expect (delve-store--read file-name)
              :to-be nil)))

  (describe "delve-store--read"
    (it "throws an error if file not found"
      (expect (delve-store--read (concat file-name "_XX"))
              :to-throw))))

(describe "Parsing / Tokenizing Objects"
  :var ((file-name (concat temporary-file-directory "test.el")))
  (before-all
    (delve-test-setup-db))
  (after-all
    (delve-test-teardown-db))
  (before-each
    (when (file-exists-p file-name)
      (delete-file file-name)))

  (describe "delve-store--map-tokenized-tree"
    (it "maps over list elements"
      (let ((l '(("A") ("B") (("C") ("D")))))
        (expect (delve-store--map-tokenized-tree #'listp l)
                :to-equal '(t t (t t)))
        (expect (delve-store--map-tokenized-tree #'car l)
                :to-equal '("A" "B" ("C" "D"))))))

  (describe "delve-store--parse-tokenized-object"
    (it "returns info object if it could not parse its argument"
      (let ((res (delve-store--parse-tokenized-object nil '(delve--unknown-object :arg "dummy"))))
        (expect (type-of res) :to-be 'delve--info))))
  (describe "delve-store--tokenize-object"
    (it "throws an error if passed an unknown object"
      (cl-defstruct (delve--unknown-object (:include delve--item)) slot)
      (expect (delve-store--tokenize-object (make-delve--unknown-object :slot "value"))
              :to-throw)))

  (describe "delve-store--get-all-ids"
    (it "gets all ids from a tokenized zettel list"
      ;; We skip creating real nodes, since we only need to pass the
      ;; ids around:
      (cl-labels ((get-node (id)
                            (org-roam-node-create :id id)))
        (let* ((ids '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J"))
               (tokenized (thread-last ids
                            (mapcar #'get-node)
                            (mapcar #'delve--zettel-create)
                            (mapcar #'delve-store--tokenize-object))))
        (expect (delve-store--get-all-ids tokenized)
                :to-have-same-items-as ids))))
    (it "skips token without ids:"
      (let* ((ids '("A" "B"))
             (zs  (thread-last ids
                    (mapcar (apply-partially #'org-roam-node-create
                                             :id))
                    (mapcar #'delve--zettel-create)))
             (i    (delve--info-create :text "No ID my dear"))
             (n    (delve--note-create :text "No no"))
             (ts   (mapcar #'delve-store--tokenize-object
                           `(,@zs ,i ,n))))
        (expect (delve-store--get-all-ids ts)
                :to-equal (delve-store--get-all-ids
                           (mapcar #'delve-store--tokenize-object zs))))))

  (describe "delve-store--prefetch-ids"
    :var (ids hash)
    (before-all
      (setq ids (delve-test-collect-ids)
            hash (delve-store--prefetch-ids ids)))
    (it "creates a hash"
      (expect hash :not :to-be nil))
    (it "creates a hash with all ids as keys"
      (expect (hash-table-keys hash)
              :to-have-same-items-as ids))
    (it "stores the nodes in the hash"
      (expect (mapcar #'org-roam-node-id (hash-table-values hash))
              :to-have-same-items-as ids)))

  (describe "Tokenize, parse and check for identity"
    (it "'delve--zettel'"
      (let* ((id     "AA")
             (node   (org-roam-node-create :id id))
             (zettel (delve--zettel-create node))
             (token  (delve-store--tokenize-object zettel))
             (hash   (let ((h (make-hash-table :test #'equal)))
                       (puthash id node h)
                       h)))
        (expect (delve-store--parse-tokenized-object hash token)
                :to-equal zettel)))
    (it "'delve--pile'"
      (let* ((ids     '("AA" "BB"))
             (nodes   (mapcar (apply-partially #'org-roam-node-create :id) ids))
             (zettels (mapcar #'delve--zettel-create nodes))
             (pile    (delve--pile-create :name "A Pile"
                                          :zettels zettels))
             (token   (delve-store--tokenize-object pile))
             (hash    (let ((h (make-hash-table :test #'equal)))
                      (cl-dolist (node nodes)
                        (puthash (org-roam-node-id node) node h))
                      h)))
        (expect (delve-store--parse-tokenized-object hash token)
                :to-equal pile)))
    (it "'delve--note'"
      (let* ((note   (delve--note-create :text "Hallo!"))
             (token  (delve-store--tokenize-object note)))
        (expect (delve-store--parse-tokenized-object nil token)
                :to-equal note)))
    (it "'delve--info'"
      (let* ((info   (delve--info-create :text "Hallo!"))
             (token  (delve-store--tokenize-object info)))
        (expect (delve-store--parse-tokenized-object nil token)
                :to-equal info)))
    (it "'delve--heading'"
      (let* ((heading (delve--heading-create :text "HEADING"))
             (token   (delve-store--tokenize-object heading)))
        (expect (delve-store--parse-tokenized-object nil token)
                :to-equal heading)))
    (it "'delve--query'"
      (let* ((query   (delve--query-create))
             (token  (delve-store--tokenize-object query)))
        (expect (delve-store--parse-tokenized-object nil token)
                :to-equal query)))
    ))

(xdescribe "Test if DB is in sync"
  (before-all
    (delve-test-setup-db))
  (after-all
    (delve-test-teardown-db))

  (it "org roam db has all IDs"
    (let* ((db-ids (cl-loop for node-id in (org-roam-db-query [:select id :from nodes])
                            append node-id))
           (file-ids (delve-test-collect-ids)))
      (expect db-ids :to-have-same-items-as file-ids))))

(xdescribe "Test DB Queries"
  (before-all
    (delve-test-setup-db))
  (after-all
    (delve-test-teardown-db))

  (describe "delve-query-node-list"
    (it "returns all nodes"
      (let ((nodes (delve-query-node-list)))
        (expect (mapcar #'org-roam-node-id nodes)
                :to-have-same-items-as
                (delve-test-collect-ids)))))

  (describe "delve-query-nodes-by-tags"
    (it "returns all nodes with a single tag"
      (let ((nodes (delve-query-nodes-by-tags '("meat"))))
        (expect (mapcar #'org-roam-node-id nodes)
                :to-have-same-items-as
                '("41ce628a-9631-4f51-92a3-1476ac9d4a61"
                  "54713c90-13da-4ea9-ab97-b056e4f47535"
                  "5fa37556-7c0f-4e7f-ba94-00dfb3388731"
                  "e9d02eb6-22e1-4549-890d-6f1d8d4ec744"))))
    (it "returns all nodes matching two tags (boolean AND)"
      (let ((nodes (delve-query-nodes-by-tags '("meat" "vegetarian"))))
        (expect (mapcar #'org-roam-node-id nodes)
                :to-have-same-items-as
                '("e9d02eb6-22e1-4549-890d-6f1d8d4ec744"))))
    (it "returns nil if called with empty list"
      (let ((nodes (delve-query-nodes-by-tags nil)))
        (expect nodes :to-be nil))))

  (describe "delve-query-tags"
    (it "returns all tags"
      (let ((tags (delve-query-tags)))
        (expect tags :to-have-same-items-as
                (delve-test-collect-tags))))
    (it "returns all tags sorted"
      (let ((tags (delve-query-tags)))
        (expect tags :to-equal
                (sort (delve-test-collect-tags) #'string<)))))

  (describe "delve-query-nodes-by-id"
    (it "returns nodes"
      (let ((ids '("b77a4837-71d6-495e-98f1-b576464aacc1"
                   "92a06447-2400-4c33-948c-c76fecda5ad2")))
        (expect (mapcar #'org-roam-node-title
                 (delve-query-nodes-by-id ids))
                :to-have-same-items-as
                '("Big note sub-heading"
                  "Spinach")))))
  (describe "delve-query-node-by-id"
    (it "returns the node"
      (let ((id "b77a4837-71d6-495e-98f1-b576464aacc1"))
        (expect (org-roam-node-title
                 (delve-query-node-by-id id)))
                :to-equal
                "Big note sub-heading"))))





(provide 'delve-tests-query)
;;; delve-tests-query.el ends here
