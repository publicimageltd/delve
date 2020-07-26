;;; delve.el --- Delve into the depths of your org roam zettelkasten       -*- lexical-binding: t; -*-

;; Copyright (C) 2020

;; Author:  <joerg@joergvolbers.de>
;; Keywords: hypermedia, org-roam

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

;; Delve into the depths of your zettelkasten.

;;; Code:

;; * Dependencies

;; add current buffer's directory to the load path:
(require 'cl-lib)
(cl-eval-when (eval)
  (if (not (member default-directory load-path))
      (add-to-list 'load-path default-directory)))

(setq load-prefer-newer t)
(require 'lister)
(require 'lister-highlight)
(require 'org-roam)

(declare-function all-the-icons-faicon "all-the-icons" (string) t)

;; * Item data types

(cl-defstruct (delve-generic-item (:constructor delve-make-generic-item)))

(cl-defstruct (delve-tag (:constructor delve-make-tag)
		      (:include delve-generic-item))
  tag
  count)

(cl-defstruct (delve-zettel (:constructor delve-make-zettel)
			 (:include delve-generic-item))
  title
  file
  tags
  mtime
  atime
  backlinks
  tolinks
  subtype)

(cl-defstruct (delve-search (:constructor delve-make-search))
  name
  with-clause
  constraint
  args
  postprocess
  (result-subtype "ZETTEL"))

;; * Item mapper functions

(defun delve-represent-tags (zettel)
  "Return a the tags list of ZETTEL as a propertized string."
  (when (delve-zettel-tags zettel)
    (concat "(" (propertize (string-join (delve-zettel-tags zettel) ", ") 'face 'org-level-1) ") ")))

(defun delve-represent-title (zettel)
  "Return the title of ZETTEL as a propertized string."
  (propertize (or (delve-zettel-title zettel) (delve-zettel-file zettel) "NO FILE, NO TITLE.")
	      'face
	      'org-document-title))

(defun delve-format-time (time)
  "Return TIME in a more human readable form."
  (let* ((days         (time-to-days time))
	 (current-days (time-to-days (current-time))))
    (if (/= days current-days)
	(format-time-string "%b %d " time)
      (format-time-string " %R " time))))

(defun delve-format-subtype (zettel)
  "Return the subtype of ZETTEL prettified."
  (let* ((subtype (delve-zettel-subtype zettel)))
    (concat
     (if (not (featurep 'all-the-icons))
	 (propertize subtype 'face 'font-lock-constant-face)
       (pcase subtype
	 ("ZETTEL"    (all-the-icons-faicon "list-alt"))
	 ("TOLINK"    (all-the-icons-faicon "caret-left"))
	 ("BACKLINK"  (all-the-icons-faicon "caret-right"))
	 (_           (delve-zettel-subtype zettel))))
     " ")))

(defun delve-represent-zettel (zettel)
  "Return propertized strings representing a ZETTEL object."
  (list
   ;;
   (concat
    (propertize
     (delve-format-time (delve-zettel-mtime zettel))
     'face 'org-document-info-keyword)
    (delve-format-subtype zettel)
    (delve-represent-tags zettel)
    (propertize
     (format "%d → " (or (delve-zettel-backlinks zettel) 0))
     'face '(:weight bold))
    (delve-represent-title zettel)
    (propertize
     (format " →  %d" (or (delve-zettel-tolinks zettel) 0))
     'face '(:weight bold))
    )))

(defun delve-represent-tag (tag)
  "Return propertized strings representing a TAG object."
  (list (concat (if (featurep 'all-the-icons)
		    (all-the-icons-faicon "tag")
		  "Tag:")
		" "
		(propertize (delve-tag-tag tag) 'face 'org-level-1)
		(when (delve-tag-count tag)
		  (format " (%d)" (delve-tag-count tag))))))

(defun delve-represent-search (search)
  "Return propertized strings representing a SEARCH object."
  (list (concat (if (featurep 'all-the-icons)
		    (all-the-icons-faicon "search")
		  "Search:")
		" "
		(propertize (delve-search-name search) 'face 'org-level-2))))

(defun delve-mapper (data)
  "Transform DATA into a printable list."
  (cl-case (type-of data)
      (delve-zettel (delve-represent-zettel data))
      (delve-tag    (delve-represent-tag data))
      (delve-search (delve-represent-search data))
      (t      (list (format "UNKNOWN TYPE: %s"  (type-of data))))))

;; * Global variables

(defvar delve-buffer-name "*DELVE*"
  "Name of delve buffers.")

(defvar delve-version-string "0.1"
  "Current version of delve.")

;; * Helper

(defun delve--flatten (l)
  "Flatten the list L and remove any nil's in it.
This is a simple copy of dash's `-flatten' using `seq'."
  (if (and (listp l) (listp (cdr l)))
      (seq-mapcat #'delve--flatten l)
    (list l)))

;; * Buffer basics

(defun delve-new-buffer ()
  "Return a new DELVE buffer."
   (generate-new-buffer delve-buffer-name))

;; * Org Roam DB
;; TODO Move this into a separate file delve-db

(defvar delve-db-error-buffer "*DELVE - Database Error*"
  "Name for a buffer displaying SQL errors.")

(defvar delve-db-there-were-errors nil
  "Whether an error has occured since last query.")

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

(defun delve-db-rearrange (pattern l)
  "For each item in L, return a new item rearranged by PATTERN.

Each element of PATTERN can be either a symbol, an integer, a
list with an integer and a function name, or a list with an
integer and a sexp.

If the element in PATTERN is a symbol or a string, just pass it
through.

If the element in PATTERN is an integer, use it as an index to
return the correspondingly indexed element of the original item.

If the element in PATTERN is a list, use the first element of
this list as the index and the second as a mapping function.  In
this case, insert the corresponding item after passing it to the
function.

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
  
;; * Org Roam Queries

;; One query to rule them all:

(defun delve-query-all-zettel (&optional subtype constraints args with-clause)
  "Return all zettel items with all fields.
The zettel objects will be of subtype SUBTYPE.

The result list can be modified using WITH-CLAUSE, CONSTRAINTS
and ARGS.  The final query is constructed like this:
 
 WITH-CLAUSE + main query + CONSTRAINTS

This final query is passed to the SQL database.  If CONSTRAINTS or
WITH-CLAUSE contain pseudo variable symbols like `$s1' or `$r1',
use ARGS to fill their values in when constructing the query.

The main query provides the fields `titles:file', `titles:title',
`tags:tags', `files:meta', `tolinks' (an integer) and
`backlinks' (an integer), which can be referred to in the
CONSTRAINTS clause.

Useful values for CONSTRAINTS  are e.g.

  [:where (like fieldname string) ]
  [:limit 10 ] or
  [:order-by (asc fieldname) ]

For examples using the WITH-CLAUSE, see `delve-query-backlinks'.

The unconstraint query can be a bit slow because is collects the
number of backlinks for each item; consider building a more
specific query for special usecases."
  (let* ((base-query
	  [:select [ titles:file                              ;; 0 file
		    titles:title                              ;; 1 title
		    tags:tags                                 ;; 2 tags
		    files:meta                                ;; 3 meta
		    (as [ :SELECT (funcall count) :FROM links ;; 4 tolinks
			 :WHERE (= links:from titles:file) ]
			tolinks)
		    (as [ :SELECT (funcall count) :FROM links ;; 5 backlinks
			 :WHERE (= links:to titles:file) ]
			backlinks) ]
	   :from titles
	   :left :join files :using [[ file ]]
	   :left :join tags :using  [[ file ]] ]))
    (with-temp-message "Querying database..."
      (thread-last (delve-db-safe-query (vconcat with-clause base-query constraints) args)
	(delve-db-rearrange-into 'delve-make-zettel
				 `[ :file 0
				   :subtype ,subtype
				   :title 1
				   :tags 2
				   :mtime (3 (plist-get it :mtime))
				   :atime (3 (plist-get it :atime))
				   :tolinks 4
				   :backlinks 5 ])))))

;; Queries returning plain lists:

(defun delve-db-roam-tags ()
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

(defun delve-query-roam-tags ()
  "Return a list of tags of all #+ROAM_TAGS."
  (let* ((tags (delve-db-roam-tags)))
    (seq-map (lambda (tag)
	       (delve-make-tag :tag tag
			       :count (delve-db-count-tag tag)))
	     tags)))


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

;; Sorting query results:

(defun delve-query-sort-by-mtime (zettel)
  "Sort ZETTEL by mtime, last one first."
  (cl-sort zettel (lambda (e1 e2) (time-less-p e2 e1))
	   :key #'delve-zettel-mtime))

(defun delve-query-last-10-modified (zettel)
  "Return the last 10 modified ZETTEL."
  (seq-take (delve-query-sort-by-mtime zettel) 10))

;; Queries resulting in delve types:

(defun delve-query-zettel-with-tag (tag)
  "Return a list of all zettel tagged TAG."
  (delve-query-all-zettel "ZETTEL" [:where (like tags:tags $r1)
				    :order-by (asc titles:title)]
			  (format "%%%s%%" tag)))

(defun delve-query-zettel-matching-title (term)
  "Return a list of all zettel with the title matching TERM."
  (delve-query-all-zettel "ZETTEL" [:where (like titles:title $r1)
				    :order-by (asc titles:title)]
			  (format "%%%s%%" term)))

(defun delve-query-backlinks (zettel)
  "Return all zettel linking to ZETTEL."
  (let* ((with-clause [:with backlinks :as [:select (as links:to file)
					    :from links
					    :where (and (= links:type "file")
							(= links:from $s1))]])
	 (constraint [:join backlinks :using [[ file ]]
		      :order-by (asc titles:title)])
	 (args       (delve-zettel-file zettel)))
    (delve-query-all-zettel "BACKLINK" constraint args with-clause)))

(defun delve-query-tolinks (zettel)
  "Return all zettel linking from ZETTEL."
  (let* ((with-clause [:with tolinks :as [:select (as links:from file)
  				          :from links
					  :where (and (= links:type "file")
						      (= links:to $s1))]])
	 (constraint [:join tolinks :using [[ file ]]
		      :order-by (asc titles:title)])
	 (args       (delve-zettel-file zettel)))
    (delve-query-all-zettel "TOLINK" constraint args with-clause)))

;; * Delve actions and keys

;; Set or reset the global list of the buffer

(defun delve-start-with-list (buf seq)
  "Delete all items in BUF and start afresh with SEQ."
  (lister-set-list buf seq)
  (when seq
    (lister-goto buf :first)))

(defun delve-start-with-list-at-point (buf pos)
  "In BUF, move the list to which POS belongs to the top."
  (pcase-let ((`(,beg ,end _ ) (lister-sublist-boundaries buf pos)))
    (delve-start-with-list buf (lister-get-all-data-tree buf beg end))))

;; Key "."
(defun delve-initial-list (&optional empty-list)
  "Populate the current delve buffer with a useful list of tags.
If EMPTY-LIST is t, delete any items instead."
  (interactive "P")
  (delve-start-with-list (current-buffer) (unless empty-list (delve-query-roam-tags)))
  (unless empty-list
    (when lister-highlight-mode
      (lister-unhighlight-item))
    (lister-insert (current-buffer) :point
		   (delve-make-search :name "10 Last Modified"
				      :postprocess #'delve-query-last-10-modified))
    (lister-insert (current-buffer) :point
		   (delve-make-search :name "10 Most Linked to"
				      :constraint [:order-by (desc backlinks)
							     :limit 10]))
    (lister-insert (current-buffer) :point
		   (delve-make-search :name "10 Most Linked from"
				      :constraint [:order-by (desc tolinks)
							     :limit 10]))
    (when lister-highlight-mode
      (lister-highlight-item)))
  (when (equal (window-buffer) (current-buffer))
    (recenter)))

;; Key "C-l"
(defun delve-sublist-to-top ()
  "Replace all items with the current sublist at point."
  (interactive)
  (unless lister-local-marker-list
    (user-error "There are not items in this buffer"))
  (delve-start-with-list-at-point (current-buffer) (point)))

;; Item action

(defun delve-insert-zettel-with-tag (buf pos tag)
  "In BUF, insert all zettel tagged TAG below the item at POS."
  (let* ((zettel (delve-query-zettel-with-tag tag)))
    (if zettel
	(lister-insert-sublist-below buf pos zettel)
      (user-error "No zettel found matching tag %s" tag))))

(defun delve-insert-links (buf pos zettel)
  "In BUF, insert all backlinks to ZETTEL below the item at POS."
  (let* ((backlinks (delve-query-backlinks zettel))
	 (tolinks   (delve-query-tolinks zettel))
	 (all       (append backlinks tolinks)))
    (if all
	(lister-insert-sublist-below buf pos all)
      (user-error "No links found"))))

(defun delve-execute-search (search)
  "Return the results of executing SEARCH."
  (let* ((res (delve-query-all-zettel
	       (delve-search-result-subtype search)
	       (delve-search-constraint search)
	       (delve-search-args search)
	       (delve-search-with-clause search))))
    (if (and res (delve-search-postprocess search))
	(funcall (delve-search-postprocess search) res)
      res)))

;; Key "Enter"
(defun delve-action (data)
  "Act on the delve object DATA."
  (if (lister-sublist-below-p (current-buffer) (point))
      (lister-remove-sublist-below (current-buffer) (point))
    (cl-case (type-of data)
      (delve-tag     (delve-insert-zettel-with-tag (current-buffer) (point) (delve-tag-tag data)))
      (delve-zettel  (delve-insert-links (current-buffer) (point) data))
      (delve-search  (lister-insert-sublist-below
		      (current-buffer) (point)
		      (delve-execute-search data))))))

;; Other key actions

(defun delve-visit-zettel (buf pos visit-function)
  "In BUF, open zettel at POS using VISIT-FUNCTION."
  (let* ((data (lister-get-data buf pos)))
    (unless (eq (type-of data) 'delve-zettel)
      (user-error "Item at point is no zettel"))
    (funcall visit-function (delve-zettel-file data))))

;; Key "o"
(defun delve-open ()
  "Open the item on point, leaving delve."
  (interactive)
  (delve-visit-zettel (current-buffer) :point #'find-file)
  (org-roam-buffer-toggle-display))

;; Key "v"
(defun delve-view ()
  "View the item on point without leaving delve."
  (interactive)
  (save-selected-window
    (delve-visit-zettel (current-buffer) :point #'find-file-other-window)
    ;; this does not work, I have no clue why:
    (org-roam-buffer-toggle-display)))

;; Key "i"
(defun delve-insert-zettel  ()
  "Choose a zettel and insert it in the current delve buffer."
  (interactive)
  (let* ((zettel (delve-query-all-zettel "ZETTEL" [:order-by (asc titles:title)]))
	 (completion (seq-map (lambda (z) (cons (concat (delve-represent-tags z)
							(delve-represent-title z))
						z))
			      zettel))
	 (candidate  (completing-read " Insert zettel: " completion nil t))
	 (pos        (point)))
    (when lister-highlight-mode
      (lister-unhighlight-item))
    (lister-insert (current-buffer) :next (alist-get candidate completion nil nil #'string=))
    (lister-goto (current-buffer) pos)))

;; Key "n"
(defvar delve-narrow-buffer nil)
(defvar delve-narrow-list nil)
(defvar delve-narrow-pos nil)
(defvar delve-narrow-input nil)

(defun delve-narrow-interactive-minibuffer-setup ()
  "Set up minibuffer for in-buffer narrowing."
  (add-hook 'post-command-hook #'delve-narrow-update nil t))

(defun delve-narrow-update ()
  "Narrow in-buffer list.
Reinsert the list items matching DELVE-NARROW-INPUT.  The complete
list is stored in DELVE-NARROW-LIST.  Reinsert at buffer position
DELVE-NARROW-POS.  Mark the minibuffer prompt if regexp is invalid."
  (setq delve-narrow-input (minibuffer-contents-no-properties))
  (lister-remove-this-level delve-narrow-buffer delve-narrow-pos)
  (delve-narrow-propertize-minibuffer-prompt 'isearch-fail t)
  (lister-insert-sublist delve-narrow-buffer
			 delve-narrow-pos
			 (or
			  (delve-narrow-reduce-list
			   delve-narrow-input
			   delve-narrow-list)
			  delve-narrow-list)))

(defun delve-narrow-reduce-list (regexp l)
  "Return items in L matching REGEXP, preserving nesting.
If REGEXP is invalid, return nil."
  (catch 'reduce
    (reverse
     (seq-reduce (lambda (acc elt)
		   (if (listp elt)
		       (if-let ((new-list (delve-narrow-reduce-list regexp elt)))
			 (cons new-list acc)
			 acc)
		     (if (delve-narrow-match-p regexp elt)
			 (cons elt acc)
		       acc)))
		 l
		 nil))))
  
(defun delve-narrow-match-p (regexp item)
  "Check if object ITEM match REGEXP.
Also higlight the minibuffer prompt if regexp is invalid."
  (condition-case err
      (string-match-p regexp
		      (cl-case (type-of item)
			(delve-zettel (delve-zettel-title item))
			(delve-tag    (delve-tag-tag item))
			(string       item) ;; for debugging
			(t            "")))
    (error
     (ignore err) ;; silence byte compiler
     (delve-narrow-propertize-minibuffer-prompt 'isearch-fail)
     (throw 'reduce nil))))

(defun delve-narrow-propertize-minibuffer-prompt (value &optional de-propertize)
  "Add face property VALUE to the minibuffer prompt, or optionally remove it.
If option DE-PROPERTIZE is set, remove the value from the
minibuffer string, else add it."
  (with-current-buffer (window-buffer (minibuffer-window))
    (let* ((inhibit-field-text-motion t)
	   (inhibit-read-only t)
	   (fn (if de-propertize 'lister-remove-face-property 'lister-add-face-property)))
      (funcall fn (line-beginning-position) (minibuffer-prompt-end) value))))

(defun delve-narrow-sublist ()
  "Interactively narrow the sublist at point."
  (interactive)
  (unless lister-local-marker-list
    (user-error "No list to narrow"))
  (when lister-highlight-mode
    (lister-unhighlight-item))
  (pcase-let ((`(,beg ,end _ ) (lister-sublist-boundaries (current-buffer) (point))))
    ;; TODO get-all-data ignores all levels; thus they are not
    ;; reinserted. So we should ideally call get-all-data-tree
    ;; and make narrow-p recursively (if ITEM is a list, ....)
    (let* ((sublist (lister-get-all-data-tree (current-buffer) beg end)))
      (setq delve-narrow-buffer (current-buffer))
      (setq delve-narrow-pos (lister-pos-as-integer beg))
      (setq delve-narrow-input nil)
      (setq delve-narrow-list sublist))
    (minibuffer-with-setup-hook
	#'delve-narrow-interactive-minibuffer-setup
      (read-from-minibuffer "Narrow sublist: "))
    (when lister-highlight-mode
      (lister-highlight-item))))

;; Key "<delete>"
(defun delve-delete-item ()
  "Delete item at point."
  (interactive)
  (if (lister-marked-items (current-buffer))
      (lister-remove-marked-items (current-buffer))
    (lister-remove (current-buffer) :point))
  (when lister-highlight-mode
    (lister-highlight-mode)))

;; * Delve Mode

(defvar delve-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lister-mode-map)
    (define-key map "v" #'delve-view)
    (define-key map "o" #'delve-open)
    (define-key map (kbd "<C-return>") #'delve-open)
    (define-key map "N" #'delve-narrow-sublist)
    (define-key map (kbd "C-l") #'delve-sublist-to-top)
    (define-key map "."  #'delve-initial-list)
    (define-key map "i" #'delve-insert-zettel)
    (define-key map (kbd "<delete>") #'delve-delete-item)
    map)
  "Key map for `delve-mode'.")


(define-derived-mode delve-mode
  lister-mode "Delve"
  "Major mode for browsing your org roam zettelkasten."
  ;; Setup lister first since it deletes all local vars:
  (lister-setup	(current-buffer) #'delve-mapper
		nil                             ;; initial data
		(concat "DELVE Version " delve-version-string) ;; header
		nil ;; footer
		nil ;; filter
		t   ;; no major-mode
		)
  ;; Now add delve specific stuff:
  (setq-local lister-local-action #'delve-action))

;; * Interactive entry points

(defvar delve-toggle-buffer nil
  "The last created lister buffer.
Calling `delve-toggle' switches to this buffer.")

;;;###autoload
(defun delve ()
  "Delve into the org roam zettelkasten."
  (interactive)
  (unless org-roam-mode
    (with-temp-message "Turning on org roam mode..."
      (org-roam-mode)))
  (with-current-buffer (setq delve-toggle-buffer (delve-new-buffer))
    (delve-mode)
    (lister-highlight-mode)
    (delve-initial-list))
  (switch-to-buffer delve-toggle-buffer)
  (delete-other-windows))

;;;###autoload
(defun delve-toggle ()
  "Toggle the display of the delve buffer."
  (interactive)
  (if (and delve-toggle-buffer
	   (buffer-live-p delve-toggle-buffer))
      (if (equal (current-buffer) delve-toggle-buffer)
	  (bury-buffer)
	(switch-to-buffer delve-toggle-buffer)
	(delete-other-windows))
    (delve)))

(bind-key "<f2>" 'delve-toggle)

(provide 'delve)
;;; delve.el ends here
