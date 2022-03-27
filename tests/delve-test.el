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
     (setq delve--storage-dir nil))
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

(provide 'delve-test)
;;; delve-test.el ends here
