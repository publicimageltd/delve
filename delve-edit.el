;;; delve-edit.el --- functions for remote editing org roam files  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  

;; Author:  <joerg@joergvolbers.de>
;; Keywords: convenience

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

;; This is a library for "delve".

;;; Code:

;; * Dependencies:

(require 'org-element)
(require 'delve-db)
(require 'org-roam)

;; * Global Variables

(defvar delve-roam-tag-history nil
  "History of selected tags for remote editing of org roam files.")

;; * Remote Editing API

(defun delve-edit-tags-first-eol (org-tree)
  "Using ORG-TREE, determine end of first line with roam tags."
  (let (end)
    (org-element-map org-tree 'keyword
      (lambda (key)
	(when (and
	       (eq (org-element-type (org-element-property :parent key)) 'section)
	       (string= (org-element-property :key key) "ROAM_TAGS"))
	  (unless end
	    (setq end (org-element-property :end key))))))
    (when end
      (1- end))))

(defun delve-edit-get-tags (org-tree)
  "Get all ROAM_TAGS from ORG-TREE.
ORG-TREE is the result from `org-element-parse-buffer'."
  (let (tags)
    (org-element-map org-tree 'keyword
      (lambda (key)
	(when (and
	       (eq (org-element-type (org-element-property :parent key))
		   'section)
	       (string= (org-element-property :key key)
			"ROAM_TAGS"))
	  (setq tags (append tags
			     (split-string (org-element-property :value key)))))))
    tags))

(defun delve-edit-title-eol (org-tree)
  "Return the position after the TITLE keyword."
  (when-let* ((end
	       (org-element-map org-tree 'keyword
		 (lambda (key)
		   (when (string= (org-element-property :key key) "TITLE")
		     (org-element-property :end key))))))
    (1- (car end))))
    
(defun delve-edit-get-unused-tags (org-tree)
  "Return all tags known to the db, but not found in ORG-TREE."
  (let* ((buf-tags (delve-edit-get-tags org-tree))
	 (db-tags  (delve-db-plain-roam-tags)))
    (cl-set-difference db-tags buf-tags :test #'string=)))

(defun delve-edit-do-add-tag (buf tag &optional org-tree)
  "Add TAG as roam tag(s) to BUF.
TAG is a string or a list of strings.
ORG-TREE should be the result of `org-element-parse-buffer'. If
ORG-TREE is nil, use the tree from calling this function on BUF."
  (with-current-buffer buf
    (let* ((tree          (or org-tree (org-element-parse-buffer)))
	   ;; either add to existing keyword or add new keyword
	   (add-pos       (delve-edit-tags-first-eol tree))
	   (new-pos       (unless add-pos
			    (or (delve-edit-title-eol tree)
				(point-min)))))
      (goto-char (or new-pos add-pos))
      (when new-pos
	(insert "\n#+ROAM_TAGS:"))
      (insert " ") 
      (insert (string-join (mapcar #'string-trim 
				   (if (listp tag) tag (list tag)))
			   " ")))))

(defun delve-edit-do-remove-tag (buf org-tree tag)
  "Remove roam tags matching TAGS from BUF, using ORG-TREE."
  (with-current-buffer buf
    (let* ((existing-tags   (delve-edit-get-tags org-tree))
	   (tag-2b-removed  (car
			     (cl-member tag existing-tags
				      :test #'string=
				      :key (lambda (it)
					     (plist-get it :value))))))
      (unless tag-2b-removed
	(error "Tag not in use"))
      
      (delete-region (plist-get tag-2b-removed :begin)
		     (plist-get tag-2b-removed :end)))))

;; -----------------------------------------------------------
;; * Interactive Remote Editing

;;;###autoload
(defun delve-edit-prompt-add-tag (zettel-file)
  "Interactively add a tag to ZETTEL-FILE."
  (interactive (list buffer-file-name))
  (unless (org-roam--org-roam-file-p zettel-file)
    (error "File not an org roam file"))
  (let* ((loaded-p (get-file-buffer zettel-file))
	 (buf      (or loaded-p (find-file-noselect zettel-file))))
    (with-current-buffer buf
      (save-buffer)
      (let* ((org-tree (org-element-parse-buffer))
	     (new-tag  (completing-read "Select tag to add: "
					(delve-edit-get-unused-tags org-tree))))
	(delve-edit-do-add-tag buf new-tag org-tree)
	(save-buffer)))
    (unless loaded-p
      (kill-buffer buf))))

;; TODO Add action to remove tag
;;;###autoload
(defun delve-edit-prompt-remove-tag (zettel-file)
  "Interactively remove a tag from ZETTEL-FILE."
  (interactive (list buffer-file-name))
  (unless (org-roam--org-roam-file-p zettel-file)
    (error "File not an org roam file"))
  (let* ((loaded-p (get-file-buffer zettel-file))
	 (buf      (or loaded-p (find-file-noselect zettel-file))))
    (with-current-buffer buf
      (save-buffer)
      )))
  

(provide 'delve-edit)
;;; delve-edit.el ends here
