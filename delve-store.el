;;; delve-store.el --- file i/o for the Delve tool  -*- lexical-binding: t; -*-

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

(defun delve-store--write (filename lisp-object)
  "Write LISP-OBJECT to FILENAME.
Return LISP-OBJECT."
  (let* ((coding-system-for-write 'binary)
         (print-level nil)
         (print-length nil)
         (content (concat
                   (format ";;; file created: %s at %s h"
                           (format-time-string "%x")
                           (format-time-string "%H:%M"))
                   "\n\n"
                   (pp-to-string lisp-object)
                   "\n;; Local Variables:\n;; eval: (when (featurep 'flycheck) (flycheck-mode -1))\n;; End:\n")))
    (with-temp-file filename
      (set-buffer-multibyte nil)
      (encode-coding-string content 'utf-8 nil (current-buffer)))
  lisp-object))

(defun delve-store--read (file-name)
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

(defun delve-store--object-as-list (delve-object)
  "Represent DELVE-OBJECT as a special list item.
The return value is a list with the object type as its CAR and
additional information as its CDR.  The data in the CDR must
suffice to fully reconstruct the complete item."
  (append
   (list (type-of delve-object))
   (cl-typecase delve-object
     (delve--zettel (list :id (delve--zettel-id delve-object)))
     (delve--pile   (list :name    (delve--pile-name delve-object)
                          :zettels (mapcar #'delve-store--object-as-list
                                           (delve--pile-zettels delve-object))))
     (t             nil))))

(defun delve-store--create-object (l)
  "Create a Delve object using L.
Determine the type of the object with the CAR of L. Use the CDR
as arguments to actually create the object.  Return the object."
  (pcase l
    (`(delve--zettel :id ,id)
     (let ((node (delve-query-node-by-id id)))
       (if node
           (delve--zettel-create node)
         (delve--info-create :text (format "Could not create zettel with ID %s" id)))))
    (`(delve--pile   :name ,name
                     :zettels ,zettels)
     (delve--pile-create :name name
                         :zettels (mapcar #'delve-store--create-object zettels)))
      ;; TODO add a text object to inform the user of the malformed expression
    (_  nil)))

(defun delve-store--create-object-list (l)
  "Recursively create a list of delve-objects out of L."
  (cl-labels ((create (elt)
                      ;; TODO return informative text object if elt==nil
                      (if (listp (car elt))
                          (delve-store--create-object-list elt)
                        (delve-store--create-object elt))))
    (mapcar #'create l)))

(defun delve-store--buffer-as-list (buf)
  "Return contents of Delve buffer BUF as a writeable list."
  (let ((ewoc (with-current-buffer buf lister-local-ewoc)))
    (lister--get-nested ewoc nil nil 0 #'identity
                        (lambda (ewoc-data)
                          (delve-store--object-as-list
                           (lister--item-data ewoc-data))))))

(provide 'delve-store)
;;; delve-store.el ends here
