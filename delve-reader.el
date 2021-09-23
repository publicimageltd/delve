;;; delve-reader.el --- file i/o for the Delve tool  -*- lexical-binding: t; -*-

;; Copyright (C) 2021

;; Author:  <joerg@joergvolbers.de>
;; Keywords: files

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

;; Library for reading and writing list data in files.

;;; Code:
(require 'seq)
(require 'lister)
(require 'delve-data-types)
(require 'delve-query)

(defun delve-reader-write (filename lisp-object &optional header)
  "Write LISP-OBJECT to FILENAME.
Add string HEADER as a comment line to the top of the file.
Return LISP-OBJECT."
  (let* ((coding-system-for-write 'binary)
         (print-level nil)
         (print-length nil)
         (content (concat
                   (format ";;; file created: %s at %s h"
                           (format-time-string "%x")
                           (format-time-string "%H:%M"))
                   "\n\n"
                   (when header
                     (concat header "\n\n"))
                   (pp-to-string lisp-object))))
    (with-temp-file filename
      (set-buffer-multibyte nil)
      (encode-coding-string content 'utf-8 nil (current-buffer)))
  lisp-object))

(defun delve-reader-read (file-name)
  "Read FILE-NAME as Lisp expression and return it."
  (if (file-exists-p file-name)
      (with-temp-buffer
        (insert-file-contents-literally file-name)
        (condition-case err
            (car (read-from-string (decode-coding-region (point-min) (point-max) 'utf-8 t)))
          ((end-of-file scan-error invalid-read-syntax circular-list)
           (error "File '%s' could not be read, returned error '%s'"
                  file-name
                  (error-message-string err)))
          (error (error "Error reading data base: %s" (error-message-string err)))))
    (error "File not found: %s" file-name)))

(defun delve-reader-object-as-list (delve-object)
  "Represent DELVE-OBJECT as a special list item.
The return value is a list with the object type as its CAR and
additional information as its CDR.  The data in the CDR must
suffice to fully reconstruct the complete item."
  (append
   (list (type-of delve-object))
   (cl-typecase delve-object
     (delve--zettel (list :id (delve--zettel-id delve-object)))
     (delve--pile   (list :name    (delve--pile-name delve-object)
                          :zettels (seq-reduce (lambda (acc elt)
                                                 (cons (delve-reader-object-as-list elt) acc))
                                               (delve--pile-zettels delve-object)
                                               nil)))
     (t             nil))))

(defun delve-reader-create-object (l)
  "Create a Delve object using L.
Determine the type of the object with the CAR of L. Use the CDR
  as arguments to actually create the object.  Return the
  object."
  (let ((type (car l))
        (args (cdr l)))
    (pcase type
      ;; TODO Use destructuring, it's pcase!
      (`delve--zettel (delve--zettel-create
                       (car (delve-query-nodes-by-id
                             (list (plist-get args :id))))))
      (`delve--pile   (delve--pile-create
                       :name    (plist-get args :name)
                       :zettels (seq-reduce (lambda (acc elt)
                                              (cons (delve-reader-create-object elt) acc))
                                            (plist-get args :zettels)
                                            nil)))
      ;; TODO add a text object to inform the user of the malformed expression
      (_  nil))))

(defun delve-reader-create-object-list (l)
  "Recursively create a list of delve-objects out of L."
  (nreverse  (seq-reduce (lambda (acc elt)
                           (cons
                            (if (listp (car elt))
                                (delve-reader-create-object-list elt)
                              (delve-reader-create-object elt))
                            acc))
                         l nil)))

(defun delve-reader-buffer-as-list (buf)
  "Return contents of Delve buffer BUF as a writeable list."
  (let ((ewoc (with-current-buffer buf lister-local-ewoc)))
    (lister--get-nested ewoc nil nil 0 #'identity
                        (lambda (ewoc-data)
                          (delve-reader-object-as-list
                           (lister--item-data ewoc-data))))))

(defun delve-reader-file-name ()
  "Return a file name with full path for storing a Delve buffer."
  (concat (file-name-directory user-emacs-directory)
          "delve-buffer-store.el"))

(provide 'delve-reader)
;;; delve-reader.el ends here
