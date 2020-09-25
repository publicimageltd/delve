;;; delve-db.el --- library for accessing the org roam database   -*- lexical-binding: t; -*-

;; Copyright (C) 2020  

;; Author:  <joerg@joergvolbers.de>
;; Keywords: data

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

;; * Global Variables

(defvar delve-db-error-buffer "*DELVE - Database Error*"
  "Buffer name when displaying SQL errors.")

(defvar delve-db-there-were-errors nil
  "Indicate if last query has caused an error.")

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
  (with-current-buffer (get-buffer-create delve-db-error-buffer)
    (special-mode)
    (let* ((inhibit-read-only t)
	   (date-string (format-time-string "%D %T  "))
	   ;; prevent logging if this is not the first error:
	   (message-log-max (if delve-db-there-were-errors nil  message-log-max)))
      (insert date-string
	      (format "Error message: %s\n" (error-message-string err)))
      (seq-doseq (s strings)
	(when (stringp s)
	  (insert date-string s "\n")))
      (insert "\n"))
    (unless delve-db-there-were-errors
      (message "There are errors. See buffer '%s' for more information."
	       delve-db-error-buffer)
      (setq delve-db-there-were-errors t))))

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

;; * Parse the Query Results

(defun delve-db-rearrange (pattern l)
  "For each item in L, return a new item rearranged by PATTERN.

Each item in L has to be a sequence (no atoms).

For each item in L, construct a return value (a list) by
successively parsing all elements in PATTERN. The elements of
PATTERN can be either a symbol, an integer, a list with an
integer and a function name, or a list with an integer and a
sexp.

If the element in PATTERN is a symbol or a string, add it to the
return value unmodified.

If the element in PATTERN is an integer, return the zero-indexed
value of the item currently processed.

If the element in PATTERN is a list, use the first element of
this list as an index and the second as a mapping function. In
this case, add the result of of calling the function with the
indexed value to the return value.

A third option is to use a list with an index and a sexp.  Like
the function in the second variant above, the sexp is used as a
mapping function.  The sexp will be eval'd with the variable `it'
bound to the original item's element.

Examples:

 (delve-db-rearrange [1 0] '((a b) (a b)))   -> ((b a) (b a))
 (delve-db-rearrange [0] '((a b c) (a b c))) ->  ((a) (a))

 (delve-db-rearrange [1 (0 1+)] '((1 0) (1 0)))      -> ((0 2) (0 2))
 (delve-db-rearrange [1 (0 (1+ it))] '((1 0) (1 0))) -> ((0 2) (0 2))

 (delve-db-rearrange [:count 1] '((0 20) (1 87))) -> ((:count 20) (:count 87))
 (delve-db-rearrang [:count 1 :string \"hi\"] '((0 20) (1 87)))
  -> ((:count 20 :string \"hi\")
      (:count 87 :string \"hi\"))"
  (seq-map (lambda (item)
	     (seq-mapcat (lambda (index-or-list)
			   (list
			    (if (or (symbolp index-or-list)
				    (stringp index-or-list))
				index-or-list
			      (if (listp index-or-list)
				  (progn
				    (with-no-warnings
				      (defvar it)) ;; force dynamic binding for calling the sexp
				    (let* ((fn-or-sexp (cadr index-or-list))
					   (it         (seq-elt item (car index-or-list))))
				      (if (listp fn-or-sexp)
					  (eval fn-or-sexp)
					(funcall fn-or-sexp it))))
				(seq-elt item index-or-list)))))
			 pattern))
	   l))

(defun delve-db-rearrange-into (make-fn keyed-pattern l)
  "Rearrange each item in L and pass the result to MAKE-FN.
KEYED-PATTERN is an extension of the pattern used by
`delve-db-rearrange'.  The extended pattern also requires a
keyword for each element.  The object is created by using the
keywords and the associated result value as key-value-pairs
passed to MAKE-FN."
  (seq-map (lambda (item)
	     (apply make-fn item))
	   (delve-db-rearrange keyed-pattern l)))

;; * Specific Queries to the Data Base

;; One query to rule them all, listed here for debugging purposes.
;; Just paste it into the scratch buffer, uncomment it, and insert it
;; into your sql editor of choice
;;
;; SELECT titles.file, titles.title, tags.tags, files.meta,
;;        (SELECT COUNT() FROM links WHERE links.[from]=titles.file) AS tolinks,
;; 	   (SELECT COUNT() FROM links WHERE links.[to] = titles.file) AS backlinks
;; FROM  titles
;; LEFT JOIN files USING (file)
;; LEFT JOIN tags USING (file)

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
	  [:select [ titles:file                              ;; 0 file
		    titles:title                              ;; 1 title
		    tags:tags                                 ;; 2 tags
		    files:meta                                ;; 3 meta
		    (as [ :SELECT (funcall count) :FROM links ;; 4 #tolinks
			 :WHERE (and (= links:type "file")
				     (= links:from titles:file)) ]
			tolinks)
		    (as [ :SELECT (funcall count) :FROM links ;; 5 #backlinks
			 :WHERE (and (= links:type "file")
				     (= links:to titles:file)) ]
			backlinks) ]
	   :from titles
	   :left :join files :using [[ file ]]
	   :left :join tags :using  [[ file ]] ]))
    (with-temp-message "Querying database..."
      (thread-last (delve-db-safe-query
		    (vconcat with-clause base-query constraints)
		    args)
	(delve-db-rearrange-into make-fn
				 `[ :file 0
				    :title 1
				    :tags 2
				    :mtime (3 (plist-get it :mtime))
				    :atime (3 (plist-get it :atime))
				    :tolinks 4
				    :backlinks 5 ])))))

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
  (caar (delve-db-safe-query [:select
			[ (as (funcall count links:from) n) ]
			:from links
			:where (= links:to $s1)]
		       file)))

(defun delve-db-count-tolinks (file)
  "Return the number of files linked from FILE."
  (caar (delve-db-safe-query [:select
			[ (as (funcall count links:to) n) ]
			:from links
			:where (= links:from $s1)]
		       file)))


;; * Database Queries Returning Delve Types:

(defun delve-db-query-roam-tags ()
  "Return all #+ROAM_TAGS as tag objects."
  (let* ((tags (delve-db-plain-roam-tags)))
    (seq-map (lambda (tag)
	       (delve-make-tag :tag tag
			       :count (delve-db-count-tag tag)))
	     tags)))

(defun delve-db-query-zettel-with-tag (tag)
  "Return all zettel tagged TAG."
  (delve-db-query-all-zettel 'delve-make-zettel
			     [:where (like tags:tags $r1)
			      :order-by (asc titles:title)]
			  (format "%%%s%%" tag)))

(defun delve-db-query-zettel-matching-title (term)
  "Return all zettel with title matching TERM."
  (delve-db-query-all-zettel 'delve-make-zettel
			     [:where (like titles:title $r1)
			      :order-by (asc titles:title)]
			  (format "%%%s%%" term)))

(defun delve-db-query-backlinks (zettel)
  "Return all zettel linking to ZETTEL."
  (let* ((with-clause [:with backlinks :as [:select (as links:from file)
					    :from links
					    :where (and (= links:type "file")
							(= links:to $s1))]])
	 (constraint [:join backlinks :using [[ file ]]
		      :order-by (asc titles:title)])
	 (args       (delve-generic-file zettel)))
    (delve-db-query-all-zettel 'delve-make-backlink
			       constraint args with-clause)))

(defun delve-db-query-tolinks (zettel)
  "Return all zettel linking from ZETTEL."
  (let* ((with-clause [:with tolinks :as [:select (as links:to file)
  				          :from links
					  :where (and (= links:type "file")
						      (= links:from $s1))]])
	 (constraint [:join tolinks :using [[ file ]]
		      :order-by (asc titles:title)])
	 (args       (delve-generic-file zettel)))
    (delve-db-query-all-zettel 'delve-make-tolink
			       constraint args with-clause)))

;; * Sorting query results:

(defun delve-db-query-sort-by-mtime (zettel)
  "Sort ZETTEL by mtime, last one first."
  (cl-sort zettel (lambda (e1 e2) (time-less-p e2 e1))
	   :key #'delve-generic-mtime))

(defun delve-db-query-last-10-modified (zettel)
  "Return the last 10 modified ZETTEL."
  (seq-take (delve-db-query-sort-by-mtime zettel) 10))

(provide 'delve-db)
;;; delve-db.el ends here
