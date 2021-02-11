;;; delve-edit.el --- functions for remote editing org roam files  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  

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

;;; Commentary:

;; This library provides interactive editing commands for "delve".

;;; Code:

;; * Dependencies:

(require 'org-element)
(require 'delve-db)
(require 'org-roam)
(require 'cl-lib)

;; * Global Variables

(defvar delve-roam-tag-history nil
  "History of selected tags for remote editing of org roam files.")

;; * Interactive Remote Editing Macro

(defmacro delve-edit-in-file (file &rest body)
  "Execute BODY in a buffer with FILE, saving all changes.
If FILE is already visited, use that buffer; else load it in a
temporary buffer.
Do not recurse this macro."
  (declare (indent 1) (debug t))
  (let* ((loaded-p-var (make-symbol "delve-edit-remote-file-loaded-p"))
	 (file-buf-var (make-symbol "delve-edit-file-buffer")))
    `(progn 
       (unless (org-roam--org-roam-file-p ,file)
	 (error "File nor an org roam file"))
       (let* ((,loaded-p-var (get-file-buffer ,file))
	      (,file-buf-var (or ,loaded-p-var
				 (find-file-noselect ,file))))
	 (with-current-buffer ,file-buf-var
	   (save-buffer)
	   ,@body
	   (save-buffer))
	 (unless ,loaded-p-var
	   (kill-buffer ,file-buf-var))))))

;; -----------------------------------------------------------
;;; * Remote editing of tags
;; -----------------------------------------------------------

;; Org roam offers its own functions to add or remove tags, but they
;; are not factored out (yet). So we build our own.

;; -- parse relevant informations from org buffers
    
(defun delve-edit-map-keyword (org-tree keyword fn)
  "Apply FN to each KEYWORD in ORG-TREE, collecting the results.
FN is called with the associated element property list as an
argument. ORG-TREE is the result of `org-element-parse-buffer'."
  (org-element-map org-tree 'keyword
    (lambda (key)
      (when (and
	     (eq (org-element-type (org-element-property :parent key))
		 'section)
	     (string= (org-element-property :key key)
		      keyword))
	(funcall fn key)))))

(defun delve-edit-get-tags (org-tree)
  "Get all roam tags from ORG-TREE.
ORG-TREE is the result of `org-element-parse-buffer'."
    (apply #'append 
	   (delve-edit-map-keyword org-tree "ROAM_TAGS"
				   (lambda (key)
 				     (split-string (org-element-property :value key))))))

(defun delve-edit-get-unused-tags (org-tree)
  "Return all tags known to the db, but not found in ORG-TREE."
  (let* ((buf-tags (delve-edit-get-tags org-tree))
	 (db-tags  (delve-db-plain-roam-tags)))
    (cl-set-difference db-tags buf-tags :test #'string=)))

(defun delve-edit-get-tag-regions (org-tree)
  "Return a list of all regions with roam tags keywords.
ORG-TREE is the result of `org-element-parse-buffer'. The
resulting list will be a list of pairs, each pointing to the
beginning and the end of the respective regions."
  (delve-edit-map-keyword org-tree "ROAM_TAGS"
			  (lambda (key)
			    (list (org-element-property :begin key)
				  (org-element-property :end key)))))


(defun delve-edit-get-new-keyword-position (org-tree)
  "Return position for the first '+ROAM_TAGS' keyword."
  (let* ((res (delve-edit-map-keyword org-tree "TITLE" 
				      (lambda (key)
					(org-element-property :end key)))))
    (car res)))

;;  -- add or remove tags in an org buffer:

(defun delve-edit-pos-to-marker (buf positions)
  "Convert POSITIONS to marker."
  (mapcar (lambda (pos)
	    (set-marker (make-marker) pos buf))
	  positions))

(defun delve-edit-set-tags (buf tags &optional org-tree)
  "Set TAGS in org roam buffer BUF.
TAGS is a list of strings. Duplicate items will be dropped. If
TAGS is nil, effectively remove any tags in the buffer.

ORG-TREE should be the result of `org-element-parse-buffer'.  If
ORG-TREE is nil, use the tree from calling this function on BUF."
  (with-current-buffer buf
    (let* ((tree     (or org-tree (org-element-parse-buffer)))
	   (regions  (delve-edit-get-tag-regions tree))
	   (new-pos  (or (delve-edit-get-new-keyword-position tree)
			 (point-min)))
	   (marker   (mapcar (apply-partially #'delve-edit-pos-to-marker buf)
			     regions)))
      (cl-dolist (region marker)
	(delete-region (cl-first region) (cl-second region)))
      (when tags
	(goto-char new-pos)
	(insert "#+ROAM_TAGS: "
		(string-join
		 (cl-remove-duplicates
		  (mapcar #'string-trim tags)
		  :test #'string=)
		 " ")
		"\n")))))

;; -- the workhorses for adding or removing tags:

(defun delve-edit-do-add-tag (buf tag &optional org-tree)
  "Add TAG as roam tag(s) to BUF.
TAG is a string or a list of strings.
ORG-TREE should be the result of `org-element-parse-buffer'. If
ORG-TREE is nil, use the tree from calling this function on BUF."
  (with-current-buffer buf
    (let* ((tree          (or org-tree (org-element-parse-buffer)))
	   (old-tags      (delve-edit-get-tags tree)))
      (delve-edit-set-tags buf
			   (append old-tags (if (listp tag) tag (list tag)))
			   tree))))

(defun delve-edit-do-remove-tag (buf tag &optional org-tree)
  "Remove TAG from org buffer BUF.
TAG is a string or a list of string.
ORG-TREE should be the result of `org-element-parse-buffer'. If
ORG-TREE is nil, use the tree from calling this function on BUF."
  (with-current-buffer buf
    (let* ((tree            (or org-tree (org-element-parse-buffer)))
	   (tags            (mapcar #'string-trim
				    (if (listp tag) tag (list tag))))
	   (existing-tags   (delve-edit-get-tags tree))
	   (new-tags        (cl-set-difference existing-tags tags
					       :test #'string=)))
      (delve-edit-set-tags buf new-tags tree))))

(defun delve-edit-add-tag (file new-tag)
  "Add NEW-TAG to org roam FILE.
NEW-TAG can be a string or a list of strings."
  (delve-edit-in-file file
    (delve-edit-do-add-tag (current-buffer) new-tag)))

(defun delve-edit-remove-tag (file new-tag)
  "Remove TAG to org roam FILE.
TAG can be a string or a list of strings."
  (delve-edit-in-file file
    (delve-edit-do-remove-tag (current-buffer) new-tag)))


;;; * Actual interactive functions

;; TODO Try to implement this with our own functions 
;;;###autoload
(defun delve-edit-prompt-add-alias (zettel-file)
  "Interactively add an alias for ZETTEL-FILE"
  (interactive (list buffer-file-name))
  (delve-edit-in-file zettel-file
    (org-roam-alias-add)))

;; TODO Try to implement this with our own functions 
;;;###autoload
(defun delve-edit-prompt-remove-alias (zettel-file)
  "Interactively remove an alias from ZETTEL-FILE"
  (interactive (list buffer-file-name))
  (delve-edit-in-file zettel-file
    (org-roam-alias-delete)))

(provide 'delve-edit)
;;; delve-edit.el ends here
