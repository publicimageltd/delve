;;; delve-test.el --- Tests for delve.el             -*- lexical-binding: t; -*-

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

;; Test delve.el functions.
;; VERY incomplete.

;;; Code:
(require 'buttercup)
(require 'delve)
(require 'dash)
;; (require 'delve-test-db-utils)

;; * Utilities

(defun delve-test-all-file-extensions (files)
  "Return an aggregate list counting all extensions in FILES.
The result is an alist with the file extension (with period)
as its key and an integer count as value."
  (--map (cons (when (car it) (concat "." (car it))) (length (cdr it)))
         (-group-by #'file-name-extension files)))

;; * The Tests
(describe "Storage File Handling"

  (describe "delve--storage-file-name-p"
    (it "returns t if passed name with correct suffix"
      (expect (delve--storage-file-name-p (expand-file-name "~/A")) :to-be nil)
      (expect (delve--storage-file-name-p (concat (expand-file-name "~/A")
                                                  delve-storage-suffix))
              :to-be-truthy)))

  (describe "delve--storage-p"
    (it "checks if file part of (delve--storage-files)"
      (let ((files (mapcar #'expand-file-name '("~/A" "~/B"))))
        (spy-on 'delve--storage-files :and-return-value files)
        (expect (delve--storage-p "~/A") :to-be-truthy)
        (expect (delve--storage-p "~/C") :to-be nil))))

  (describe "delve--set-storage-dir"
    (before-each
     (setq delve--last-storage-dir nil))
    (it "uses first value of delve-storage-paths (list)"
      (let ((delve-storage-paths '("foo" "bar")))
        (expect (delve--set-storage-dir)
                :to-equal "foo/")))
    (it "uses single value of delve-storage-paths (string)"
      (let ((delve-storage-paths "foo"))
        (expect (delve--set-storage-dir)
                :to-equal "foo/")))
    (it "returns nil if there are no paths set"
      (let ((delve-storage-paths nil))
        (expect (delve--set-storage-dir)
                :to-be nil))))

  (describe "delve--fix-suffix"
    (it "adds extension if there is none"
      (expect (delve--fix-suffix "test" ".el")
              :to-equal "test.el"))
    (it "replaces existing extension"
      (expect (delve--fix-suffix "test.el" ".doc")
              :to-equal "test.doc")))


  ;; NOTE to future self: These tests are definetely overexplicit and
  ;; useless insofar they should cover more edge cases (files are
  ;; hell).  In truth, they are just a playground to improve on
  ;; writing tests.  But I did find SOME bugs at least.
  (describe "Finding files"
    :var (tempdir suffix-list)
    (before-all
      ;; Check results against this list
      ;; using delve-test-all-file-extensions
      (setq suffix-list '((".el" 4)
                          (".delve" 8)
                          (".doc" 2)
                           (nil 1)))
      (setq tempdir (file-name-as-directory
                     (make-temp-file "test" t)))
      (let ((temporary-file-directory tempdir))
        ;; this creates the files and returns the names as a list so
        ;; they can be stored in a var in case the tests seem to be
        ;; buggy and have to be tested themselves
        (cl-loop for (suffix n) in suffix-list
                 append (cl-loop repeat n
                                 collect (make-temp-file "test" nil suffix)))))
    (after-all
      (when (and tempdir (file-exists-p tempdir))
        (delete-directory tempdir t)))

    (describe "delve--all-files-in-paths"
      (it "returns the correct number of files per suffix"
        (let ((paths (-list tempdir)))
          (expect (length (delve--all-files-in-paths paths ".el")) :to-be 4)
          (expect (length (delve--all-files-in-paths paths ".doc")) :to-be 2)
          (expect (length (delve--all-files-in-paths paths ".delve")) :to-be 8)
          (expect (length (delve--all-files-in-paths paths nil))    :to-be 1)
          )))

    (describe "delve--storage-files"
      (it "asks for the creation of the path directory (single path)"
        (spy-on 'y-or-n-p :and-return-value t)
        (spy-on 'make-directory)
        (spy-on 'directory-files)
        (let ((delve-storage-paths "singledir"))
          (delve--storage-files)
          (expect 'y-or-n-p :to-have-been-called)
          (expect 'make-directory :to-have-been-called-with delve-storage-paths t)))
      (xit "returns the correct number of files (single path)"
        (let ((delve-storage-paths tempdir))
          (expect (length (delve--storage-files)) :to-be 8))))))

(describe "Deleting"
  :var (ewoc)
  (before-each
    (setq ewoc nil)
    (setq ewoc (lister-setup "*LISTER*" (lambda (x) (format "%S" x))))
    (with-current-buffer (ewoc-buffer ewoc)
      (setq-local lister-local-left-margin 0)))

  (after-each
    (kill-buffer (ewoc-buffer ewoc)))

  (describe "delve--ewoc-node-invalid-p"
    (it "returns t for a deleted node"
      (lister-insert-at ewoc :first '(:eins :zwei :drei))
      (let ((node (lister-get-node-at ewoc 0)))
        (lister-delete-at ewoc 0)
        (expect (delve--ewoc-node-invalid-p ewoc node) :to-be-truthy)))
    (it "returns nil for a normal node"
      (lister-insert-at ewoc :first '(:eins :zwei :drei))
      (expect (--map (delve--ewoc-node-invalid-p ewoc
                                                 (lister-get-node-at ewoc it))
                     '(0 1 2))
              :to-equal '(nil nil nil)))
    (it "returns t for a nil value"
      (expect (delve--ewoc-node-invalid-p ewoc nil) :to-be-truthy)))

  (describe "delve--delete-item"
    (it "deletes a single node"
      (lister-insert-at ewoc :first :test-data)
      (let ((node (lister-get-node-at ewoc :first)))
        (delve--delete-item ewoc node)
        (expect (lister-get-list ewoc) :to-be nil)))
    (it "reindents a subtree"
      (lister-insert-list-at ewoc :first '(:top-node (:indented-node :indented-node)))
      (let ((node (lister-get-node-at ewoc :first)))
        (delve--delete-item ewoc node)
        (expect (lister-get-list ewoc) :to-equal '(:indented-node :indented-node))
        (expect (lister-get-level-at ewoc 0) :to-be 0)
        (expect (lister-get-level-at ewoc 1) :to-be 0)))
    (it "reindents a nested subtree"
      (lister-insert-list-at ewoc :first '(:top-node (:second-top-node (:indented-node :indented-node))))
      (let ((node (lister-get-node-at ewoc 1)))
        (delve--delete-item ewoc node)
        (expect (lister-get-list ewoc) :to-equal '(:top-node (:indented-node :indented-node)))
        (expect (lister-get-level-at ewoc 0) :to-be 0)
        (expect (lister-get-level-at ewoc 1) :to-be 1)
        (expect (lister-get-level-at ewoc 2) :to-be 1)))
    (it "deletes a nested item"
      (lister-insert-list-at ewoc :first '(:top-node (:second-top-node) :node-2 :node-3))
      (let ((node (lister-get-node-at ewoc 1)))
        (delve--delete-item ewoc node)
        (expect (lister-get-list ewoc) :to-equal '(:top-node :node-2 :node-3))
        (expect (lister-get-level-at ewoc 0) :to-be 0))))

  (describe "delve--save-outline"
    (it "unhides all nodes within its body"
      (let ((nested-list '(:top-node (:level-1-parent (:hidden-item :hidden-item) :level-1-item))))
        (lister-insert-list-at ewoc :first nested-list)
        (lister-outline-hide-sublist-below ewoc 1) ;; :level-1-parent
        (delve--save-outline ewoc
          (expect (-map (-partial #'lister--outline-invisible-p ewoc)
                        '(0 1 2 3 4))
                  :to-equal '(nil nil nil nil nil)))))
    (it "restores previous state after exiting body"
      (let ((nested-list '(:top-node (:level-1-parent (:hidden-item :hidden-item) :level-1-item))))
        (lister-insert-list-at ewoc :first nested-list)
        (lister-outline-hide-sublist-below ewoc 1) ;; :level-1-parent
        (delve--save-outline ewoc)
        (expect (-map (-partial #'lister--outline-invisible-p ewoc)
                      '(0 1 2 3 4))
                :to-equal '(nil nil t t nil))))
    (it "ignores deleted nodes when restoring visibility state"
      (let ((nested-list '(:top-node (:level-1-parent (:hidden-item :hidden-item) :level-1-item))))
        (lister-insert-list-at ewoc :first nested-list)
        (lister-outline-hide-sublist-below ewoc 1) ;; :level-1-parent
        (delve--save-outline ewoc
          (lister-delete-at ewoc 2))
        (expect (-map (-partial #'lister--outline-invisible-p ewoc)
                      '(0 1 2 3))
                :to-equal '(nil nil t nil))))
    (it "returns the result of BODY"
      (lister-insert-list ewoc :first '(:item))
      (expect (delve--save-outline ewoc :a-value) :to-be :a-value))
    (it "can be called with an empty list"
      (expect (delve--save-outline ewoc) :not :to-throw))))




(provide 'delve-test)
;;; delve-test.el ends here
