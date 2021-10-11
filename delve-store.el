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

(defun delve-store--write (file-name lisp-object)
  "Write LISP-OBJECT to FILE-NAME.
Return LISP-OBJECT."
  (let* ((coding-system-for-write 'binary)
         (print-level nil)
         (print-length nil)
         (content (concat
                   (format ";;; file created: %s at %s h -*- emacs-lisp -*-"
                           (format-time-string "%x")
                           (format-time-string "%H:%M"))
                   "\n\n"
                   (pp-to-string lisp-object)
                   "\n;; Local Variables:\n;; eval: (when (featurep 'flycheck) (flycheck-mode -1))\n;; End:\n")))
    (with-temp-file file-name
      (set-buffer-multibyte nil)
      (encode-coding-string content 'utf-8 nil (current-buffer)))
  lisp-object))

(defun delve-store--read (file-name)
  "Read FILE-NAME as Lisp expression and return it."
  ;; FIXME See the info entry on "file-exists-p" on why this check
  ;;       is actually not enough
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

;;; * Store a Delve list

(defun delve-store--tokenize-object (delve-object)
  "Represent DELVE-OBJECT as a special list item.
The return value is a list with the object type as its CAR and
additional information as its CDR.  The data in the CDR must
suffice to fully reconstruct the complete item."
  (append
   (list (type-of delve-object))
   (cl-etypecase delve-object
     (delve--zettel (list :id (delve--zettel-id delve-object)))
     (delve--pile   (list :name    (delve--pile-name delve-object)
                          :zettels (mapcar #'delve-store--tokenize-object
                                           (delve--pile-zettels delve-object))))
     (delve--note   (list :text  (delve--note-text delve-object)))
     (delve--info   (list :text  (delve--info-text delve-object))))))

(defun delve-store--parse-tokenized-object (id-hash elt)
  "Create a Delve object parsing tokenized object ELT.
Use ID-HASH to get the nodes by their ID."
  (pcase elt
    (`(delve--zettel :id ,id)
     (if-let ((node (gethash id id-hash)))
         (delve--zettel-create node)
       (delve--info-create :text (format "Could not create zettel with ID %s" id))))
    ;;
    (`(delve--pile :name ,name :zettels ,zettels)
     (delve--pile-create :name name
                         :zettels (mapcar
                                   (apply-partially
                                    #'delve-store--parse-tokenized-object id-hash)
                                   zettels)))
    ;;
    (`(delve--note :text ,text)
     (delve--note-create :text text))
    ;;
    (`(delve--info :text ,text)
     (delve--info-create :text text))
    ;;
    (_  (delve--info-create :text (format "Could not parse expression %s" elt)))))

;;; * Read a stored list

;; Useful for looping over the token list:

(defun delve-store--map-tokenized-tree (fn l)
  "Apply FN to each list element of tree L."
  (cond
   ((and (listp l) (listp (car l)))
    (mapcar (lambda (x) (delve-store--map-tokenized-tree fn x)) l))
   (t (funcall fn l))))

;; Prefetch all IDs:

(defun delve-store--parse-get-id (elt)
  "Return all IDs for tokenized ELT."
  (pcase elt
    (`(delve--zettel :id ,id)     id)
    (`(delve--pile :name ,_ :zettels ,zettels)
     (mapcar #'delve-store--parse-get-id zettels))
    (_ nil)))

(defun delve-store--get-all-ids (l)
  "Get all ids in the Delve storage list L."
  (lister--flatten
   (delve-store--map-tokenized-tree
    #'delve-store--parse-get-id
    l)))

(defun delve-store--prefetch-ids (l)
  "Return all nodes referred to in L as a hash table."
  (let* ((ids    (delve-store--get-all-ids l))
         (nodes  (delve-query-nodes-by-id ids))
         (table (make-hash-table :test 'equal)))
    (while ids
      (puthash (car ids) (car nodes) table)
      (setq ids (cdr ids)
            nodes (cdr nodes)))
    table))

;; Parse and create Delve objects:

;;; TODO Write tests
(defun delve-store--create-object-list (l)
  "Create Delve objects for stored list L."
  (let ((id-hash (delve-store--prefetch-ids l)))
    (delve-store--map-tokenized-tree (apply-partially #'delve-store--parse-tokenized-object
                                            id-hash)
                           l)))

;;; Test-DB: (delve-store--read "~/.emacs.d/delve-store/stufen.el")

(provide 'delve-store)
;;; delve-store.el ends here
