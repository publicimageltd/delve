;;; delve-store-test.el --- Test delve store functions   -*- lexical-binding: t; -*-

;; Copyright (C) 2022

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

;; Tests storing and reading Lisp object, as well as parsing and
;; tokenizing Delve objects.

;;; Code:
(require 'buttercup)
(require 'delve-store)

(describe "Reading and writing"
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


(describe "ID handling"

  (describe "delve-store--get-ids-for-token-list"
    (it "gets all ids from a tokenized zettel list"
      ;; We skip creating real nodes, since we only need to pass the
      ;; ids around:
      (let* ((ids '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J"))
             (tokenized (thread-last ids
                                     (mapcar (apply-partially #'org-roam-node-create :id))
                                     (mapcar #'delve--zettel-create)
                                     (mapcar #'delve-store--tokenize-object))))
        (expect (delve-store--get-ids-for-token-list tokenized)
                :to-have-same-items-as ids))))

    (it "skips token without ids:"
      (let* ((ids '("A" "B"))
             (zs  (thread-last ids
                    (mapcar (apply-partially #'org-roam-node-create :id))
                    (mapcar #'delve--zettel-create)))
             (i    (delve--info-create :text "No ID my dear"))
             (n    (delve--note-create :text "No no"))
             (ts   (mapcar #'delve-store--tokenize-object
                           `(,@zs ,i ,n))))
        (expect (delve-store--get-ids-for-token-list ts)
                :to-equal (delve-store--get-ids-for-token-list
                           (mapcar #'delve-store--tokenize-object zs))))))

  (describe "delve-store--prefetch-ids"
    :var (ids hash)
    ;; "Spies may only be created in before-each or it blocks"
    (before-each
      (setq ids (number-sequence 10 100))
      (spy-on 'delve-query-nodes-by-id :and-return-value
              (--map (org-roam-node-create :id it) ids))
      (setq  hash (delve-store--prefetch-ids ids)))

    (it "creates a hash"
      (expect hash :not :to-be nil))
    (it "creates a hash with all ids as keys"
      (expect (hash-table-keys hash)  :to-have-same-items-as ids))
    (it "stores node objects as hash values"
      (expect (cl-every #'org-roam-node-p (hash-table-values hash)))))

(describe "Parsing"

  (describe "delve-store--map-tree"
    (it "maps over list elements"
      (let ((l '(("A") ("B") (("C") ("D")))))
        (expect (delve-store--map-tree #'listp l)
                :to-equal '(t t (t t)))
        (expect (delve-store--map-tree #'car l)
                :to-equal '("A" "B" ("C" "D"))))))

  (describe "delve-store--parse-element"
    (it "returns info object if it could not parse its argument"
      (let ((res (delve-store--parse-element nil '(delve--unknown-object :arg "dummy"))))
        (expect (type-of res) :to-be 'delve--info))))

  (describe "delve-store--tokenize-object"
    (it "throws an error if passed an unknown object"
      (cl-defstruct (delve--unknown-object (:include delve--item)) slot)
      (expect (delve-store--tokenize-object (make-delve--unknown-object :slot "value"))
              :to-throw)))

  (describe "specific objects"
    ;; we define fake nodes, tokenize them, and then check if parsing
    ;; the token returns on object equal to the original one
    :var (nodes zettels hash)
    ;; "Spies may only be created in before-each or it blocks"
    (before-each
      (let* ((ids '("AA" "BB" "CC" "DD")))
        (setq nodes (mapcar (apply-partially #'org-roam-node-create :id) ids)
              zettels (mapcar #'delve--zettel-create nodes))
        (spy-on 'delve-query-nodes-by-id :and-return-value
                (--map (org-roam-node-create :id it) ids))
        (setq hash (delve-store--prefetch-ids ids))))

    (it "'delve--zettel'"
      (let ((zettel (car zettels)))
        (expect (delve-store--parse-element hash (delve-store--tokenize-object zettel))
                :to-equal zettel)))

    (it "'delve--pile'"
      (let ((pile (delve--pile-create :name "A pile" :zettels zettels)))
        (expect (delve-store--parse-element hash (delve-store--tokenize-object pile))
                :to-equal pile)))

    (it "'delve--query'"
      (let* ((query (delve--query-create :info "info" :fn #'ignore)))
        (expect (delve-store--parse-element hash (delve-store--tokenize-object query))
                :to-equal query)))

    (it "'delve--heading'"
      (let* ((heading (delve--heading-create :text "HEADING")))
        (expect (delve-store--parse-element hash (delve-store--tokenize-object heading))
                :to-equal heading)))

    (it "'delve--info'"
      (let* ((info   (delve--info-create :text "Hallo!")))
        (expect (delve-store--parse-element hash (delve-store--tokenize-object info))
                :to-equal info)))

    (it "'delve--note'"
      (let* ((note   (delve--note-create :text "Hallo!")))
        (expect (delve-store--parse-element hash (delve-store--tokenize-object note))
                :to-equal note)))))

(provide 'delve-store-test)
;;; delve-store-test.el ends here
