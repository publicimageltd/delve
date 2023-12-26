;;; delve-store.el --- file i/o for the Delve tool  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023

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

;; Library for reading and writing Delve data in files.

;;; Code:
(require 'seq)
(require 'dash)
(require 'lister)
(require 'delve-data-types)
(require 'delve-query)

;; * Generic read/write stuff

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

;;; * Delve Tokenizer

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
     (delve--query  (list :info  (delve--query-info delve-object)
                          :fn    (delve--query-fn   delve-object)))
     (delve--heading (list :text (delve--heading-text delve-object)))
     (delve--info   (list :text  (delve--info-text delve-object)))
     (delve--note   (list :text  (delve--note-text delve-object))))))

(defun delve-store--parse-element (id-hash elt)
  "Create a Delve object from token ELT.
Use ID-HASH to get the nodes by their ID."
  (pcase elt
    (`(delve--zettel :id ,id)
     (if-let ((node (gethash id id-hash)))
         (delve--zettel-create node)
       (delve--info-create :text (format "Could not create zettel with ID '%s'" id))))
    ;;
    (`(delve--pile :name ,name :zettels ,zettels)
     (delve--pile-create :name name
                         :zettels (mapcar
                                   (apply-partially
                                    #'delve-store--parse-element id-hash)
                                   zettels)))
    ;;
    (`(delve--query :info ,info :fn ,fn)
     (delve--query-create :info info :fn fn))
    ;;
    (`(delve--heading :text ,text)
     (delve--heading-create :text text))
    ;;
    (`(delve--note :text ,text)
     (delve--note-create :text text))
    ;;
    (`(delve--info :text ,text)
     (delve--info-create :text text))
    ;;
    (_  (delve--info-create :text (format "Could not parse expression %s" elt)))))

(defun delve-store--map-tree (fn l)
  "Apply FN to each list element of tree L.
Traverse L as a tree where each list can hold further list
elements as 'branches' of the containing list.  When traversing,
call FN when the node's value is a list which contains an element
which is not anymore a list."
  (-tree-map-nodes (lambda (l) (not (listp (car l))))  fn l))

(defun delve-store--get-ids-for-token (elt)
  "Return Org Roam IDs for tokenized Delve object ELT.
Return nil if ELT does not reference any Org Roam node."
  (pcase elt
    (`(delve--zettel :id ,id)     id)
    (`(delve--pile :name ,_ :zettels ,zettels)
     (-map #'delve-store--get-ids-for-token zettels))
    (_ nil)))

(defun delve-store--get-ids-for-token-list (l)
  "Get all ids in the Delve token list L."
  (->> l
    (delve-store--map-tree #'delve-store--get-ids-for-token)
    (flatten-tree)))

(defun delve-store--create-node-table (ids)
  "Create an hash table associating Org Roam nodes by IDS.
The Org Roam ID serves as the key, the node object is the
associated value."
  (let ((nodes  (delve-query-nodes-by-id ids))
        (table  (make-hash-table :test 'equal)))
    (while nodes
      (puthash (org-roam-node-id (car nodes)) (car nodes) table)
      (setq nodes (cdr nodes)))
    table))

(defun delve-store--parse-list (l)
  "Create Delve objects from token list L."
  (let* ((ids    (delve-store--get-ids-for-token-list l))
         (table  (delve-store--create-node-table ids)))
    (delve-store--map-tree
     (-partial #'delve-store--parse-element table)
     l)))

(provide 'delve-store)
;;; delve-store.el ends here
