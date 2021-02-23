;;; delve-db.el --- library for accessing the org roam database   -*- lexical-binding: t; -*-

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

;; Provide basic functionality for accessing the org roam database.

;;; Code:

;; * Dependencies

(require 'seq)
(require 'org-roam-db)
(require 'delve-data-types)

;; * Silence Byte Compiler

(declare-function lister-replace "lister" (buf pos data &optional level) t)

;; * Global Variables

(defvar delve-db-error-buffer "*DELVE - Database Error*"
  "Buffer name when displaying SQL errors.")

(defvar delve-db-there-were-errors nil
  "Indicate if last query has caused an error.")


;; * Internal Variables
;; for constructing the queries

(defvar delve-db-sql--link-type-restriction
  '(or (= links:type "id")
       (= links:type "file"))
  "If added to an SQL WHERE Clause, restrict links to the types
'file' and 'id'.")


;; * Helper

(defun delve--flatten (l)
  "Flatten the list L, removing any null values.
This is a simple copy of dash's `-flatten' using `seq'."
  (if (and (listp l) (listp (cdr l)))
      (seq-mapcat #'delve--flatten l)
    (list l)))

;; * API for Safe Queries

;; Wrap all queries in a logging system.

(defun delve-db-log-error (err &rest strings)
  "Insert ERR and additional STRINGS in the error buffer."
  (declare (indent 1))
  ;; fill the error buffer
  (with-current-buffer (get-buffer-create delve-db-error-buffer)
    (special-mode)
    (let* ((inhibit-read-only t)
	   (date-string (format-time-string "%D %T  "))
	   ;; prevent logging if this is not the first error:
	   (message-log-max (if delve-db-there-were-errors nil  message-log-max)))
      (goto-char (point-max))
      (insert date-string
	      (format "Error message: %s\n" (error-message-string err)))
      (seq-doseq (s strings)
	(when (stringp s)
	  (insert date-string s "\n")))
      (insert "\n"))
    (unless delve-db-there-were-errors
      (message "There are errors. See buffer '%s' for more information."
	       delve-db-error-buffer)
      (setq delve-db-there-were-errors t)))
  ;; notify the user via delve:
  (when (derived-mode-p 'delve-mode)
    ;; TODO Move this out of this module; this should be handled in
    ;; delve main
    (lister-replace (current-buffer) :point
		    (delve-make-error :message "Useless message"
				      :buffer (get-buffer-create delve-db-error-buffer)))))

(defun delve-db-safe-query (sql &rest args)
  "Call org roam SQL query (optionally using ARGS) in a safe way.
Catch all errors and redirect the error messages to an error
buffer.  If an error occurs, inform the user with a message and
return nil."
  (condition-case-unless-debug err
      (apply #'org-roam-db-query sql args)
    (error (delve-db-log-error err
			       " Error occured when executing the query:"
			       (format " %s" sql)
			       (when args
				 (format " Arguments: %s" args)))
	   nil)))

;; * Specific Queries to the Data Base

;; One query to rule them all, listed here for debugging purposes.
;; Just paste it into the scratch buffer, uncomment it, and insert it
;; into your sql editor of choice
;;
;; SELECT titles.file, titles.title, tags.tags, files.meta,
;;        (SELECT COUNT() FROM links WHERE links.[source]=titles.file) AS tolinks,
;; 	   (SELECT COUNT() FROM links WHERE links.[dest] = titles.file) AS backlinks
;; FROM  titles
;; LEFT JOIN files USING (file)
;; LEFT JOIN tags USING (file)

;;
(defun delve-db-query-all-zettel (make-fn &optional constraints args with-clause)
  "Query the org roam DB for pages and return them as zettel objects.

MAKE-FN is a constructor function to create and populate the
zettel objects. Useful values are `delve-make-zettel',
`delve-make-backlink' and `delve-make-tolink'.

The result is created by using a query with quite some SQL magic
putting together all informations such as file names, links, etc.
This main query can be modified using the vectors WITH-CLAUSE,
CONSTRAINTS and ARGS. The final SQL query is constructed like
this:
 
 WITH-CLAUSE + main query + CONSTRAINTS

This final query is passed to `org-roam-db-query', and from there
to `emacsql'. See there for the correct format for formulating
the clauses. If CONSTRAINTS or WITH-CLAUSE contain pseudo
variable symbols like `$s1' or `$r1', optional arguments ARGS are
used to fill their values in when constructing the query.

The main query provides the fields `titles:file', `titles:title',
`tags:tags', `files:meta', `tolinks' (an integer) and
`backlinks' (an integer), which can be referred to in the
CONSTRAINTS clause.

Useful values for CONSTRAINTS  are e.g.

  [:where (like fieldname string) ]
  [:limit 10 ] or
  [:order-by (asc fieldname) ]

For examples using the WITH-CLAUSE, see `delve-db-query-backlinks'.

The unconstraint query can be quite slow because is collects the
number of backlinks for each item; consider building a more
specific query for special usecases."
  (let* ((base-query
	  `[:select [ titles:file                              ;; 0 file
		      titles:title                              ;; 1 title
		      tags:tags                                 ;; 2 tags
		      files:meta                                ;; 3 meta
		      (as [ :SELECT (funcall count) :FROM links ;; 4 #tolinks
			 :WHERE (and ,delve-db-sql--link-type-restriction
				     (= links:source titles:file)) ]
			tolinks)
		    (as [ :SELECT (funcall count) :FROM links ;; 5 #backlinks
			 :WHERE (and ,delve-db-sql--link-type-restriction
				     (= links:dest titles:file)) ]
			backlinks) ]
	   :from titles
	   :left :join files :using [[ file ]]
	   :left :join tags :using  [[ file ]] ]))
    ;; use desctructuring to map the table columns to lisp vars
    ;; saw this while browsing org-roam's "v2" repository
    (cl-loop for row in (delve-db-safe-query
			 (vconcat with-clause base-query constraints)
			 args)
	     collect (pcase-let ((`(,file ,title ,tags ,times ,tolinks ,backlinks) row))
		       (funcall make-fn
				:file      file
				:title     title
				:tags      tags
				:mtime     (plist-get times :mtime)
				:atime     (plist-get times :atime)
				:tolinks   tolinks
				:backlinks backlinks)))))

;; * Queries returning plain lisp lists:

(defun delve-db-plain-roam-tags ()
  "Return a list of all #+ROAM_TAGS."
  (thread-last (delve-db-safe-query [:select :distinct tags:tags :from tags])
    (delve--flatten)
    (seq-uniq)
    (seq-sort #'string-lessp)))

(defun delve-db-count-tag (tag)
  "Count the occurences of TAG in the org roam db."
  (pcase-let* ((`((( _ ) ,n))
		(delve-db-safe-query [:select [ tags:tags
					  (as (funcall count tags:tags) n) ]
					:from tags
					:where (like tags:tags $r1)]
				     (format "%%%s%%" tag))))
    n))


(defun delve-db-count-backlinks (file)
  "Return the number of files linking to FILE."
  (caar (delve-db-safe-query `[:select
			      [ (as (funcall count links:source) n) ]
			      :from links
			      :where (and ,delve-db-sql--link-type-restriction
					  (= links:dest $s1))]
			     file)))

(defun delve-db-count-tolinks (file)
  "Return the number of files linked from FILE."
  (caar (delve-db-safe-query `[:select
			       [ (as (funcall count links:dest) n) ]
			       :from links
			       :where (and ,delve-db-sql--link-type-restriction
					   (= links:source $s1))]
			     file)))


;; * Database Queries Returning Delve Types:

(defun delve-db-query-roam-tags ()
  "Return all #+ROAM_TAGS as tag objects."
  (let* ((tags (delve-db-plain-roam-tags)))
    (seq-map (lambda (tag)
	       (delve-make-tag :tag tag
			       :count (delve-db-count-tag tag)))
	     tags)))

(defun delve-db-query-pages-with-tag (tag)
  "Return all pages tagged TAG."
  (delve-db-query-all-zettel 'delve-make-page
			     [:where (like tags:tags $r1)
			      :order-by (asc titles:title)]
			  (format "%%%s%%" tag)))

(defun delve-db-query-pages-matching-title (term)
  "Return all pages with title matching TERM."
  (delve-db-query-all-zettel 'delve-make-page
			     [:where (like titles:title $r1)
			      :order-by (asc titles:title)]
			  (format "%%%s%%" term)))

(defun delve-db-query-backlinks (zettel)
  "Return all zettel linking to ZETTEL."
  (let* ((with-clause `[:with backlinks :as [:select (as links:source file)
						     :from links
						     :where (and ,delve-db-sql--link-type-restriction
								 (= links:dest $s1))]])
	 (constraint [:join backlinks :using [[ file ]]
		      :order-by (asc titles:title)])
	 (args       (delve-zettel-file zettel)))
    (delve-db-query-all-zettel 'delve-make-backlink
			       constraint args with-clause)))

(defun delve-db-query-tolinks (zettel)
  "Return all zettel linking from ZETTEL."
  (let* ((with-clause `[:with tolinks :as [:select (as links:dest file)
  						   :from links
						   :where (and ,delve-db-sql--link-type-restriction
							       (= links:source $s1))]])
	 (constraint [:join tolinks :using [[ file ]]
		      :order-by (asc titles:title)])
	 (args       (delve-zettel-file zettel)))
    (delve-db-query-all-zettel 'delve-make-tolink
			       constraint args with-clause)))

;; * Sorting query results:

(defun delve-db-query-sort-by-mtime (zettel)
  "Sort ZETTEL by mtime, last one first."
  (cl-sort zettel (lambda (e1 e2) (time-less-p e2 e1))
	   :key #'delve-zettel-mtime))

(defun delve-db-query-last-10-modified (zettel)
  "Return the last 10 modified ZETTEL."
  (seq-take (delve-db-query-sort-by-mtime zettel) 10))

;; * Update a complete item tree

(defun delve-db-update-zettel (item make-fn)
  "Use MAKE-FN to return zettel ITEM updated."
  (car (delve-db-query-all-zettel make-fn
				  [:where (= titles:file $s1)]
				  (delve-zettel-file item))))

(defun delve-db-get-page-from-file (file)
  "Return a page item associated with FILE."
  (car (delve-db-query-all-zettel 'delve-make-page
				  [:where (= titles:file $s1)]
				  file)))

(defun delve-db-update-item (item)
  "Return the delve ITEM updated."
  (cl-typecase item
    (delve-tag (let* ((tag (delve-tag-tag item)))
		 (delve-make-tag :tag tag
				 :count (delve-db-count-tag tag))))
    (delve-page     (delve-db-update-zettel item 'delve-make-page))
    (delve-tolink   (delve-db-update-zettel item 'delve-make-tolink))
    (delve-backlink (delve-db-update-zettel item 'delve-make-backlink))
    (delve-page-search item)
    (t         nil)))

(defun delve-db-update-tree (tree)
  "Return a copy of TREE with all items updated.
If an item does not exist anymore, remove it from TREE.
TREE has to be a nested list."
  (when (and tree (listp tree))
    (cl-remove-if #'null
		  (mapcar (lambda (item)
			    (if (listp item)
				(delve-db-update-tree item)
			      (delve-db-update-item item)))
			  tree))))


;; * Merge items with same file reference into one [pseudo-] item
;; TODO This is supposed to help to get rid of alias doubles.
;; Currently unused.

;; (defun delve-db-zettel-without-slot (zettel slot)
;;   "Return ZETTEL with SLOT set to nil."
;;   (let ((copy (copy-sequence zettel)))
;;     (setf (aref copy (cl-struct-slot-offset 'delve-zettel slot)) nil)
;;     copy))

;; (defun delve-db-is-aliased-p (z1 z2)
;;   "Check if Z1 and Z2 only have different titles."
;;   (equal (delve-db-zettel-without-slot z1 'title)
;; 	 (delve-db-zettel-without-slot z2 'title)))

;; (defun delve-db-untitled-zettel-hash (zettel)
;;   "Return a hash value for ZETTEL, ignoring the title."
;;   (sxhash-equal (delve-db-zettel-without-slot zettel 'title)))

;; (defun delve-db-define-hash-test ()
;;   (define-hash-table-test 'zettel-sans-title
;;     'delve-db-is-aliased-p 'delve-db-untitled-zettel-hash))

;; (defun delve-db-merge-aliased (zettel-list)
;;   "Merge aliased items in ZETTEL-LIST."
;;   (let* ((hash-table (make-hash-table :test 'zettel-sans-title
;; 				      :size (length zettel-list))))
;;     (cl-dolist (item zettel-list)
;;       ;; TODO das macht nur ein "reduce", kein "merge".
;;       ;; um zu mergen, müsste irgendwie der titel gerettet werden.
;;       ;; also wohl (item (title1 title2 title3)
;;       (puthash item item hash-table))
;;     (let (res)
;;       (maphash (lambda (key val)
;; 		 ;; TODO hier muss noch der title wieder eingefügt werden!
;; 		 (setq res (cons val res)))
;; 	       hash-table)
;;       res)))

(provide 'delve-db)
;;; delve-db.el ends here
