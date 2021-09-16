;;; delve.el --- Delve into the depths of your org roam zettelkasten       -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021

;; Author:  <joerg@joergvolbers.de>
;; Version: 0.7
;; Package-Requires: ((emacs "26.1") (org-roam "1.2.3"))
;;
;; Keywords: hypermedia, org-roam
;; URL: https://github.com/publicimageltd/delve

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


;; -----------------------------------------------------------
;; * Dependencies

(require 'cl-lib)
(require 'org-roam)
(require 'delve-lister)
(require 'delve-lister-highlight)
(require 'delve-data-types)
(require 'delve-edit)
(require 'delve-pp)

;; * Silence Byte Compiler

(declare-function all-the-icons-faicon "all-the-icons" (string) t)

;; * Non-Customizable Global variables

(defvar delve-force-ignore-all-the-icons nil
  "*Internal*: Do not use icons when representing an item.")

(defvar delve-version "0.7"
  "Current version of delve.")

;; * Customizable Global variables

(defcustom delve-new-buffer-add-creation-time " (%ER)"
  "Time string to add to heading when creating new delve buffers.
If nil, do not add anything."
  :type 'string
  :group 'delve)

(defcustom delve-auto-delete-roam-buffer t
  "Delete visible *org roam* buffer when switching to DELVE."
  :type 'boolean
  :group 'delve)

(defcustom delve-buffer-name-format "delve: %.80s"
  "Prefix for delve buffer names.
New delve buffer will be created using this format spec."
  :type 'boolean
  :group 'delve)

(defcustom delve-use-icons-in-completions nil
  "Use icons when asking for completions.
If Delve asks you to choose between a list of buffers or pages,
turning this option on will use icons when displaying the items
to select from.  This is only useful if you do use a completion
interface like ivy, since it is hard to type an icon."
  :type 'boolean
  :group 'delve)

(defcustom delve-user-actions
  '(delve)
  "Lists of actions to present in `delve-toggle'.
Each action is simply an interactive function."
  :type '(repeat function)
  :group 'delve)

(defcustom delve-searches
  `((:name "Pages without tags"
	   :constraint [:where tags:tags :is :null])
    (:name "10 Last Modified"
	   :postprocess delve-db-query-last-10-modified)
    (:name "10 Most Linked To"
	   :constraint [:order-by (desc backlinks) :limit 10])
    (:name "10 Most Linked From"
	   :constraint [:order-by (desc tolinks)   :limit 10])
    (:name "10 Most Linked"
	   :constraint [:order-by (desc (+ backlinks tolinks)) :limit 10]))
  "A list of default searches offered when starting delve."
  :type '(repeat plist)
  :group 'delve)

(defcustom delve-zettel-pp-time-scheme '(mtime)
  "Specification of time values displayed with each zettel item.
A list of symbols each specifying which time value should be
displayed when printing a zettel item.  Possible values are
`mtime' (modification time), `atime' (access time) and
`ctime' (creation time).  Several values result in several time
values printed side by side.

Currently, ctime is not supported by org roam."
  :type  '(repeat (choice (const :tag "mtime (Modification Time)" mtime)
			  (const :tag "atime (Access Time)"       atime)
			  (const :tag "ctime (Creation Time)"     ctime)))
  :group 'delve)


;; * Faces

(defface delve-tags-face
  '((t (:inherit org-roam-tag)))
  "Face for displaying #+ROAM-TAGs in a delve list."
  :group 'delve)

(defface delve-title-face
  ;;  '((t (:inherit variabe-pitch)))
  '((t (:inherit org-document-title)))
  "Face for displaying org roam page titles in a delve list."
  :group 'delve)

(defface delve-subtype-face
  '((t (:inherit font-lock-constant-face)))
  "Face for displaying the subtype of a delve item."
  :group 'delve)

(defface delve-mtime-face
  '((t (:inherit org-document-info-keyword)))
  "Face for displaying the mtime of a delve item."
  :group 'delve)

(defface delve-atime-face
  '((t (:inherit org-document-info-keyword)))
  "Face for displaying the atime of a delve item."
  :group 'delve)

(defface delve-ctime-face
  '((t (:inherit org-document-info-keyword)))
  "Face for displaying the ctime of a delve item."
  :group 'delve)

(defface delve-nbacklinks-face
  '((t (:weight bold)))
  "Face for displaying the number of backlinks to a delve zettel."
  :group 'delve)

(defface delve-ntolinks-face
  '((t (:weight bold)))
  "Face for displaying the number of tolinks to a delve zettel."
  :group 'delve)

(defface delve-search-face
  '((t (:inherit org-level-2)))
  "Face for displaying the title of a delve search."
  :group 'delve)


;; * Buffer local variables for delve mode

(defvar-local delve-local-initial-list nil
  "Buffer list when first creating this delve buffer.")

;; -----------------------------------------------------------
;; * Utilities

(defun delve--acomplete (prompt collection &optional require-match history)
  "Complete on alist COLLECTION and return the associated result.
For the values of PROMPT, REQUIRE-MATCH and HISTORY, see the
documentation of `completing-read'."
  (let* ((res         (completing-read prompt collection nil require-match nil history))
	 (associated  (alist-get res collection nil nil #'string=)))
    (if (and (not require-match) (not associated))
	res
      associated)))

;; -----------------------------------------------------------
;; * Item Mapper for the List Display (lister)

;; -- visually indicate the type of the item:

(defun delve-pp-generic:type (delve-object)
  "Represent the type of DELVE-OBJECT, if possible with an icon."
  (let* ((representation
	  (pcase (type-of delve-object)
	    (`delve-error          (list "ERROR" "bug"))
	    (`delve-generic-search (list "SEARCH"  "search"))
	    (`delve-page-search    (list "SEARCH"  "search"))
	    (`delve-tag            (list "TAG"     "tag"))
	    (`delve-page           (list "PAGE"    "list-alt"))
	    (`delve-tolink         (list "TOLINK"  "caret-left"))
	    (`delve-backlink       (list "BACKLINK" "caret-right"))
	    (_                     (list "SUBTYPE?" "question")))))
    (if (and (featurep 'all-the-icons)
	     (not delve-force-ignore-all-the-icons))
	(all-the-icons-faicon (cl-second representation))
      (delve-pp-mod:width (cl-first representation) 8))))

;; -- presenting a zettel object:

(defvar delve-zettel-pp-scheme
  '((delve-pp-zettel:needs-update (:set-face org-warning))
    (delve-pp-generic:type        (:add-face delve-subtype-face))
    delve-pp-zettel:times
    (delve-pp-zettel:tags         (:format "(%s)"
				   :set-face delve-tags-face))
    (delve-pp-zettel:backlinks    (:format "%d → "
				   :set-face delve-nbacklinks-face))
    (delve-pp-zettel:title        (:set-face delve-title-face))
    (delve-pp-zettel:tolinks      (:format " →  %d"
				   :set-face delve-ntolinks-face)))
  "Pretty printing scheme for displaying delve zettel.
See `delve-pp-line' for possible values.")

(defun delve-represent-zettel (zettel)
  "Represent ZETTEL as a pretty printed list item."
  (list (delve-pp-line zettel delve-zettel-pp-scheme)))

(defun delve-pp-zettel:needs-update (zettel)
  "Return information that ZETTEL needs to be updated."
  (when (delve-zettel-needs-update zettel)
    "item changed, not up to date ->"))

(defun delve-pp-zettel:format-time (zettel time-fun)
  "Return the TIME-FUN of ZETTEL in a human readable form."
  (let* ((time                 (funcall time-fun zettel))
	 (days                 (time-to-days time))
	 (current-time-in-days (time-to-days (current-time)))
	 (day-difference       (- current-time-in-days days))
	 (current-year         (string-to-number (format-time-string "%y" (current-time))))
	 (zettel-year          (string-to-number (format-time-string "%y" time)))
	 (format-spec  (cond
			 ((/= current-year zettel-year) "%b %d %y")
			 ((> day-difference 1)   "%b %d")
			 (t        "%R"))))
;;    (format "%d" (time-to-day-in-year time))))
    (format-time-string format-spec time)))

(defun delve-pp-zettel:mtime (zettel)
  "Return the mtime of ZETTEL in a human readable form."
  (delve-pp-zettel:format-time zettel 'delve-zettel-mtime))

(defun delve-pp-zettel:atime (zettel)
  "Return the atime of ZETTEL in a human readable form."
  (delve-pp-zettel:format-time zettel 'delve-zettel-atime))

(defun delve-pp-zettel:ctime (zettel)
  "Return the ctime of ZETTEL in a human readable form."
  (delve-pp-zettel:format-time zettel 'delve-zettel-ctime))

(defun delve-pp-zettel:times (zettel)
  "Return pretty printed {a/m/c}-times of ZETTEL."
  (let ((schemes '((mtime . ((delve-pp-zettel:mtime (:format "%10s"
						   :set-face delve-mtime-face))))
		  (atime  . ((delve-pp-zettel:atime (:format "%10s"
						   :set-face delve-atime-face))))
		  (ctime  . ((delve-pp-zettel:ctime (:format "%10s"
						   :set-face delve-ctime-face)))))))
    ;;
    (mapconcat (lambda (time-key)
		 (when-let ((pp-scheme (alist-get time-key schemes)))
		   (delve-pp-line zettel pp-scheme)))
	       delve-zettel-pp-time-scheme
	       " ")))

(defun delve-pp-zettel:tags (zettel)
  "Join all tags from ZETTEL."
  (when-let ((tags (delve-zettel-tags zettel)))
    (string-join tags ",")))

(defun delve-pp-zettel:backlinks (zettel)
  "Return number of backlinks to ZETTEL."
  (or (delve-zettel-backlinks zettel) 0))

(defun delve-pp-zettel:title (zettel)
  "Return the title of ZETTEL."
  (or (delve-zettel-title zettel)
      (delve-zettel-file zettel)
      "NO FILE OR TITLE"))

(defun delve-pp-zettel:tolinks (zettel)
  "Return number of tolinks to ZETTEL."
  (or (delve-zettel-tolinks zettel)))

;; -- presenting a search item:

(defvar delve-search-pp-scheme
  '(delve-pp-generic:type
    (delve-generic-search-name (:set-face delve-search-face)))
  "Pretty printing scheme for displaying delve searches.
See `delve-pp-line' for possible values.")

(defun delve-represent-search (search)
  "Represent SEARCH object as a  pretty printed  list item."
  (list (delve-pp-line search delve-search-pp-scheme)))

;; -- presenting a tag object:

(defvar delve-tag-pp-scheme
  '(delve-pp-generic:type
    ;; FIXME explicitly declare this face
    (delve-tag-tag   (:set-face org-level-1))
    (delve-tag-count (:format "(%d)")))
  "Pretty printing scheme for displaying tag objects.")

(defun delve-represent-tag (tag)
  "Represent TAG object as a pretty printed list item."
  (list (delve-pp-line tag delve-tag-pp-scheme)))

;; -- presenting an error object:

(defvar delve-error-pp-scheme
  '((delve-pp-generic:type (:add-face error))
    delve-pp-error:message)
  "Pretty printing scheme for displaying error objects.
See `delve-pp-line' for possible values.")

(defun delve-pp-error:message (error-object)
  "Return an informative message about the error.
ERROR-OBJECT must be a delve object, not an Emacs error object."
  ;; TODO Use slot "message" in error object to
  ;; make this message even more specific
     (format " SQL error logged in buffer '%s' (press ENTER to view)"
	     (buffer-name (delve-error-buffer error-object))))

(defun delve-represent-error (error-object)
  "Represent ERROR-OBJECT as a pretty printed list item."
  (list (delve-pp-line error-object delve-error-pp-scheme)))

;; the actual mapper:

(defun delve-mapper (data)
  "Transform DATA into a printable list."
  (pcase data
    ((pred delve-zettel-p)         (delve-represent-zettel data))
    ((pred delve-tag-p)            (delve-represent-tag data))
    ((pred delve-generic-search-p) (delve-represent-search data))
    ((pred delve-error-p)          (delve-represent-error data))
    (_        (list (format "UNKNOWN TYPE: %s"  (type-of data))))))

(defun delve-mapper-for-completion (data)
  "Transform DATA to an item suitable for completion."
  (let* ((delve-force-ignore-all-the-icons t)
	 (delve-pp-inhibit-faces t))
    (delve-mapper data)))

;; -----------------------------------------------------------
;; * Expand items by creating sublists
;;
;; "Expanding" basically means to do something (or to do several
;; things) with a given item, collecting and passing the subsequent
;; results in a list. It is akin to "evaluating" the operation with
;; the item as its argument.
;;

;; This function does all the work:

(defun delve-expand (item &rest operator-fns)
  "Collect the result of applying all OPERATOR-FNS on ITEM."
  (with-temp-message "Querying database, please wait..."
    (cl-loop for fn in operator-fns
	     append (let ((res (funcall fn item)))
		      (if (listp res) res (list res))))))

;; These are the operators.
;;
;; Each operator should have one argument, a delve object, and should
;; return a list of further delve objects.
;;

(defun delve-operate-search (search)
  "Return the results of executing SEARCH."
  (let* ((res (delve-db-query-all-zettel
	       ;; subtype:
	       (delve-generic-search-result-makefn search)
	       ;; constraint
	       (delve-generic-search-constraint search)
	       ;; args
	       (delve-generic-search-args search)
	       ;; with-clause
	       (delve-generic-search-with-clause search))))
    (if (and res (delve-generic-search-postprocess search))
	(funcall (delve-generic-search-postprocess search) res)
      res)))

(defun delve-operate-backlinks (zettel)
  "Get a list of all zettel linking to ZETTEL."
  (delve-db-query-backlinks zettel))

(defun delve-operate-tolinks (zettel)
  "Get a list of all zettel linking from ZETTEL."
  (delve-db-query-tolinks zettel))

(defun delve-operate-taglist (tag)
  "Get a list of all zettel with TAG."
  (delve-db-query-pages-with-tag (delve-tag-tag tag)))

;; * Create and insert sublists by expanding items

(defun delve-expansion-operators-for (item)
  "Return a list of valid expansion operators to apply to ITEM."
  (pcase item
    ((pred delve-tag-p)
     (list #'delve-operate-taglist))
    ((pred delve-zettel-p)
     (list #'delve-operate-backlinks  #'delve-operate-tolinks))
    ((pred delve-generic-search-p)
     (list #'delve-operate-search))
    (_ nil)))

;; TODO Add error handling
(defun delve-expand-item (item)
  "Return a useful expansion for delve object ITEM, or nil."
  (when-let* ((ops (delve-expansion-operators-for item)))
      (apply #'delve-expand item ops)))

(defun delve-expand-and-insert (buf pos &optional operator-fn)
  "Determine expansion operators and insert results for item at POS.
Determine the expansion operator for the item at the indicated
position, collect the results and insert them as sublist.

The expansion operator is determined using the item type.  (See
`delve-expansion-operators-for'.) If OPERATOR-FN is set, use this
function as an operator instead.

BUF must be a valid lister buffer populated with delve items.  POS
can be an integer or the symbol `:point'."
  (interactive (list (current-buffer) (point)))
  (let* ((position (pcase pos
		     ((and (pred integerp) pos) pos)
		     (:point (with-current-buffer buf (point)))
		     (_ (error "Invalid value for POS: %s" pos))))
	 (item     (delve-lister-get-data buf position))
	 (sublist  (if operator-fn
		       (funcall operator-fn item)
		     (delve-expand-item item))))
    (if sublist
	(with-temp-message "Inserting expansion results..."
	  (delve-lister-insert-sublist-below buf position sublist))
      (user-error "No expansion found"))))

;; -----------------------------------------------------------
;;; * Delve Mode: Interactive Functions, Mode Definition

;;  Generic function for marked items

(cl-defun delve-walk-marked-items (buf action-fn &optional
				       (mark-current-if-none t)
				       (unmark t))
  "Apply ACTION-FN on all marked items in BUF.
Return the accumulated results.

If no item is marked, apply the function to the item at point
unless MARK-CURRENT-IF-NONE is nil.  Remove the marks after
operation unless UNMARK is nil."
  (let* ((marked-items (delve-lister-all-marked-items buf))
	 (mark-pred-fn (buffer-local-value 'delve-lister-local-marking-predicate buf)))
    ;; maybe mark item at point
    (when (and (null marked-items)
	       mark-current-if-none)
      (if (or (not (delve-lister-item-p buf :point))
	      ;; TODO Replace this with "delve-lister-markable-p" in 0.6
	       (not (and (not (null mark-pred-fn))
			 (funcall mark-pred-fn (delve-lister-get-data buf :point)))))
	  (user-error "Item at point has to be zettel")
	(delve-lister-mark-item buf :point t)
	(setq marked-items (list (delve-lister-marker-at buf :point)))
	;; always unmark since the mark is internal only
	(setq unmark t)))
    ;; now do the walk:
    (unless marked-items
      (user-error "There are no marked items"))
    (let* ((res (delve-lister-walk-marked-items buf action-fn)))
      (when unmark
	(delve-lister-mark-some-items buf marked-items nil))
      res)))

;; * Filter

(defun delve-filter--build-filter-predicate (slot filter-fn &rest args)
  "Build predicate applying FILTER-FN on SLOT for a zettel item.
Optional arguments ARGS will also be passed to FILTER-FN.

The resulting function filters out any zettel items where
FILTER-FN, when applied to the value of the SLOT of the zettel,
returns a nil value.  Items of a different type will be left
as-is."
  (lambda (d)
    (if (delve-zettel-p d)
	(apply filter-fn (cl-struct-slot-value 'delve-zettel slot d) args)
      t)))

(defun delve-filter--build-title-filter (title-pattern)
  "Build predicate for all zettel titles matching TITLE-PATTERN."
  (delve-filter--build-filter-predicate 'title (apply-partially #'string-match-p title-pattern)))

(defun delve-filter--match-tag-p (tags pattern)
  "Check if PATTERN matches any of TAGS."
  (seq-find (apply-partially #'string-match-p pattern) tags))

(defun delve-filter--build-tag-filter (tag-pattern)
  "Build predicate for all zettel with tags matching TAG-PATTERN."
  (delve-filter--build-filter-predicate 'tags #'delve-filter--match-tag-p
					tag-pattern))

(defun delve-filter-by-tag (buf tag-pattern)
  "Show only zettel items in BUF matching TAG-PATTERN."
  (interactive (list (current-buffer)
		     (read-string "Enter tag pattern (regexp): ")))
  (when (string-empty-p tag-pattern)
    (user-error "No pattern for filtering"))
  (delve-lister-set-filter buf (delve-filter--build-tag-filter tag-pattern)))

(defun delve-filter-by-title (buf title-pattern)
  "Show only zettel items in BUF matching TITLE-PATTERN."
  (interactive (list (current-buffer)
		     (read-string "Enter title pattern (regexp): ")))
  (when (string-empty-p title-pattern)
    (user-error "No pattern for filtering"))
  (delve-lister-set-filter buf (delve-filter--build-title-filter title-pattern)))

(defun delve-filter-remove (buf)
  "Remove active filter in delve buffer BUF."
  (interactive (list (current-buffer)))
  (unless (delve-lister-filter-active-p buf)
    (user-error "No filter active"))
  (delve-lister-set-filter buf nil))

;; * Sort

(defun delve-sort--offer-predicates ()
  "Let the user choose between sorting predicates."
  ;; FIXME rename "zettel-sorting-pred" to "build-sorting-pred."
  (let* ((pred-alist `(("Modification time (oldest first)" . ,(delve-db-zettel-sorting-pred #'time-less-p     'mtime))
		       ("Modification time (latest first)" . ,(delve-db-zettel-sorting-pred #'time-less-p 'mtime :not))
		       ("Access time (oldest first)"     . ,(delve-db-zettel-sorting-pred #'time-less-p     'atime))
		       ("Title (from a to z)" . ,(delve-db-zettel-sorting-pred #'string-lessp    'title))
		       ("Title (from z to a)". ,(delve-db-zettel-sorting-pred #'string-greaterp 'title)))))
    (delve--acomplete "Sort by: " pred-alist t)))

(defun delve-sort-sublist (buf pos sort-pred)
  "Sort the sublist at POS in delve buffer BUF.
SORT-PRED has to be a function which returns the right order when
comparing two zettel items. The macro
`delve-db-zettel-sorting-pred' can be used for creating such
functions on the fly.

If called interactively, let the user select the predicate for
sorting the sublist at point."
  (interactive (list (current-buffer)
		     (point)
		     (delve-sort--offer-predicates)))
  (let (data)
    (unless (delve-lister-nonempty-p buf)
      (user-error "Nothing to sort"))
    (unless (and (delve-lister-item-p buf pos)
		 (setq data (delve-lister-get-data buf pos))
		 (delve-zettel-p data))
      (user-error "For sorting, point must be on a zettel item"))
    (pcase-let* ((`(,beg ,end _ ) (delve-lister-sublist-boundaries buf pos)))
      (delve-lister-sort-list buf sort-pred beg end))))

;; * Refresh or update the display in various ways

(defun delve-refresh-buffer (buf)
  "Refresh all items in BUF."
  (interactive (list (current-buffer)))
  (when-let* ((all-data (delve-lister-get-all-data-tree buf)))
    (delve-lister-with-locked-cursor buf
      (with-temp-message "Updating the whole buffer, that might take some time...."
	(delve-lister-set-list buf (delve-db-update-tree all-data))))))

(defun delve-refresh-tainted-items (buf)
  "Update all items in BUF which are marked as needing update.
Also update all marked items, if any."
  (interactive (list (current-buffer)))
  (cl-labels ((tainted-zettel-p (data)
				(and (delve-zettel-p data)
				     (or (delve-zettel-needs-update data)
					 (delve-lister-get-mark-state buf :point))))
	      (update-zettel (data)
			     (when-let* ((new-item (delve-db-update-item data)))
			       (delve-lister-replace buf :point new-item))))
    (let ((res (delve-lister-walk-all buf #'update-zettel #'tainted-zettel-p)))
      (message (if res
		   (format "Updated %d items" (length res))
		 "All items up to date. To force an update, mark the item(s) and redo this function")))))

(defun delve-revert (buf)
  "Revert delve buffer BUF to its initial list."
  (interactive (list (current-buffer)))
  (with-current-buffer buf
    (delve-lister-set-list buf delve-local-initial-list)
    (delve-lister-goto buf :first)))

;; * Collect items

(defun delve-collect (buf &optional keep-visible)
  "In BUF, copy marked items or item at point into a (new) buffer.
After copying, unmark the items and switch to the target buffer.
If KEEP-VISIBLE is non-nil, keep the items marked and do not
switch to the target buffer."
  (interactive (list (current-buffer) current-prefix-arg))
  (let* ((bufs         (delve--all-delve-buffers-for-completion (cl-remove buf (delve-all-buffers))))
	 (buf-or-name  (delve--acomplete "Choose collection name or enter a new one: " bufs))
	 (collection   (delve-walk-marked-items buf #'identity t (not keep-visible)))
	 (new-buf      nil)
	 (msg          (format "Added %d items" (length collection))))
    (if (stringp buf-or-name)
	(setq new-buf (delve collection buf-or-name (not keep-visible)))
      (delve-add-to-buffer buf-or-name collection)
      (setq new-buf buf-or-name))
    (if (not keep-visible)
	(switch-to-buffer new-buf)
      (setq msg (concat msg
			" to"
			(when (stringp buf-or-name)
			  " new")
			" buffer " (buffer-name new-buf))))
    (message msg)))

;; * Expand items

(defun delve-expand-insert-tolinks ()
  "Insert all tolinks from the item at point."
  (interactive)
  (unless (delve-zettel-p (delve-lister-get-data (current-buffer) :point))
    (user-error "This item has no tolinks"))
  (delve-expand-and-insert (current-buffer)
			   :point
			   #'delve-operate-tolinks))

(defun delve-expand-insert-backlinks ()
  "Insert all backlinks from the item at point."
  (interactive)
  (unless (delve-zettel-p (delve-lister-get-data (current-buffer) :point))
    (user-error "This item has no backlinks"))
  (delve-expand-and-insert (current-buffer)
			   :point
			   #'delve-operate-backlinks))

(defun delve-expand-toggle-sublist ()
  "Close or open the item's sublist at point."
  (interactive)
  (let* ((buf (current-buffer))
	 (pos (point)))
    (if (delve-lister-sublist-below-p buf pos)
	(delve-lister-remove-sublist-below buf pos)
      (delve-expand-and-insert buf pos))))

(defun delve-expand-in-new-bufffer (buf pos &optional expand-parent)
  "Expand the item at point in a new buffer.
With prefix arg EXPAND-PARENT, open the current subtree to which
the item at point belongs in a new buffer.

BUF is a lister buffer, POS marks the position of the item."
  (interactive (list (current-buffer) (point) current-prefix-arg))
  (unless delve-lister-local-marker-list
    (user-error "There are no items in this buffer"))
  (let* ((item-at-point (delve-lister-get-data buf pos)))
    (if expand-parent
	(if (not (delve-zettel-p item-at-point))
	    (user-error "Only zettel sublists can be re-opened in a new buffer")
	  (pcase-let* ((`(,beg ,end _ ) (delve-lister-sublist-boundaries buf pos)))
	    (let* ((items (delve-lister-get-all-data-tree buf beg end)))
	      (delve items))))
      (delve item-at-point))))

;;; * Remote editing: add / remove tags

(defun delve-remote-edit (buf edit-fn arg)
  "Apply EDIT-FN with ARG on all marked items, or the item at point.
BUF must be a delve buffer.

EDIT-FN has to accept two arguments: an org roam file, to which
the editing will apply, and an additional argument ARG."
  (cl-labels ((add-it (data)
		      (funcall edit-fn (delve-zettel-file data) arg)
		      (setf (delve-zettel-needs-update data) t)
		      (delve-lister-replace buf :point data)))
    (let ((res (delve-walk-marked-items buf #'add-it)))
      (message "Changed %d items" (or (length res) 0)))))

(defun delve-add-tag ()
  "Add tags to all marked zettel or the zettel at point."
  (interactive)
  (let* ((new-tag (completing-read
		   "Add tag: " (delve-db-plain-roam-tags))))
    (delve-remote-edit (current-buffer) #'delve-edit-add-tag new-tag)))

(defun delve-remove-tag ()
  "Remove tags from the zettel at point."
  (interactive)
  (let* ((tag-to-delete (completing-read
			 "Remove tag: " (delve-db-plain-roam-tags)
			 nil t)))
    (delve-remote-edit (current-buffer) #'delve-edit-remove-tag tag-to-delete)))

;; Act on the item at point

(defun delve-key-visit-zettel (buf pos)
  "Visit the zettel at point."
  (interactive (list (current-buffer) (point)))
  (unless (delve-lister-item-p buf pos)
    (user-error "No item to visit"))
  (let ((data (delve-lister-get-data buf pos)))
    (pcase data
      ((pred delve-error-p)  (switch-to-buffer (delve-error-buffer data)))
      ((pred delve-zettel-p) (find-file (delve-zettel-file data)))
      (_                     (error "Cannot visit this item")))))

;;; Delve Major Mode

(defvar delve-mode-map
  (let ((map (make-sparse-keymap)))
    ;; inherit standard key bindings:
    ;; FIXME Remove reference to deprecated delve-lister-keys-mode-map
    ;; once we dependend on lister>0.7.1
    (set-keymap-parent map (if (fboundp 'delve-lister-keys-mode-map)
			       delve-lister-keys-mode-map
			     delve-lister-mode-map))
    ;; Visit Zettel at point:
    (define-key map (kbd "<RET>")      #'delve-key-visit-zettel)
    ;;
    ;; expand items:
    (define-key map "\t"               #'delve-expand-toggle-sublist)
    (define-key map (kbd "C-l")        #'delve-expand-in-new-bufffer)
    (define-key map (kbd "<left>")     #'delve-expand-insert-backlinks)
    (define-key map (kbd "<right>")    #'delve-expand-insert-tolinks)
    ;;
    ;; restore buffer or items:
    (define-key map "r"                #'delve-revert)
    (define-key map (kbd "g")          #'delve-refresh-buffer)
    (define-key map "."                #'delve-refresh-tainted-items)
    ;;
    ;; edit the list:
    (define-key map (kbd "s")          #'delve-sort-sublist)
    ;;
    ;; collect items:
    (define-key map (kbd "c")          #'delve-collect)
    ;;
    ;; remote editing of zettel pages:
    (define-key map (kbd "+")          #'delve-add-tag)
    (define-key map (kbd" -")          #'delve-remove-tag)
    (define-key map (kbd "X")          #'delve-kill-buffer)
    map)
  "Key map for `delve-mode'.")

(define-derived-mode delve-mode
  delve-lister-mode "Delve"
  "Major mode for browsing your org roam zettelkasten."
  ;; Setup lister first since it deletes all local vars:
  (delve-lister-setup	(current-buffer) #'delve-mapper
		nil
		(concat "DELVE Version " delve-version))
  ;; --- Now add delve specific stuff:
  ;; do not mark searches:
  (setq-local delve-lister-local-marking-predicate #'delve-zettel-p))

;; * Some delve specific buffer handling

;; TODO Define all these faces explicitly via defface
(defun delve--pretty-main-buffer-header ()
  "Return a pretty header for the main buffer."
  (list
   (delve-pp-line nil `(("DELVE"  (:set-face font-lock-comment-face))
			(,delve-version (:set-face font-lock-comment-face))
			("Main buffer" (:set-face org-document-title))))
   (delve-pp-line nil '((" This is the main entry page. Use <TAB> to expand an item, C-l expands in a new buffer."
			 (:set-face font-lock-comment-face))))))

;; TODO Define all these faces explicitly via defface
(defun delve--pretty-collection-header (collection-name)
  "Use COLLECTION-NAME to build a pretty lister header."
  (list
   (delve-pp-line nil `(("DELVE Collection" (:set-face font-lock-comment-face))
			(,collection-name (:set-face '((t (:foreground-color "green")))))))))

(defun delve--modify-first-item (l modify-fn)
  "Replace the first item in L with the result of MODIFY-FN.
MODIFY-FN is called with the original first item as its
argument."
  (apply #'list (funcall modify-fn (car l)) (cdr l)))

(defun delve-new-buffer-with-expansion (delve-object)
  "Return a new buffer with an expansion of DELVE-OBJECT.
If there are no expansions for this object, throw an error."
  (let (heading-prefix object-name)
    (unless (delve-expansion-operators-for delve-object)
      (error "Unknown delve object, cannot create a collection"))
    (cl-etypecase delve-object
      (delve-page          (setq heading-prefix "Links to and from"
				 object-name    (delve-page-title delve-object)))
      (delve-tag           (setq heading-prefix "Zettel tagged with"
				 object-name    (delve-tag-tag delve-object)))
      (delve-backlink      (setq heading-prefix "Links to"
				 object-name   (delve-backlink-title delve-object)))
      (delve-tolink        (setq heading-prefix "All links from"
				 object-name   (delve-tolink-title delve-object)))
      (delve-page-search   (setq heading-prefix "Search results"
				 object-name   (delve-page-search-name delve-object))))
    (let ((object-type     (let ((delve-force-ignore-all-the-icons t))
			     (string-trim (delve-pp-generic:type delve-object))))
	  (object-mapped   (let ((delve-pp-inhibit-faces t))
			     (delve-mapper delve-object)))
	  (items (delve-expand-item delve-object)))
      (unless items
	(user-error (concat "Expanding " (downcase heading-prefix) " '" object-name "' yields no result")))
      (delve-new-collection-buffer items
				   (delve--modify-first-item  object-mapped
							      (apply-partially #'concat heading-prefix " "))
				   (concat heading-prefix  " " object-type " '"  object-name "' ")))))

;; TODO Rename to delve-new-buffer
(defun delve-new-collection-buffer (items heading buffer-name)
  "List delve ITEMS in a new buffer.

HEADING has to be a list item (a list of strings) which will be
used as a heading for the list. As special case, if HEADING is
nil, no heading will be displayed, and if HEADING is a string,
implictly convert it into a valid item.

The new buffer name will be created by using
`delve-buffer-name-format' with the value of BUFFER-NAME."
  (let* ((buf (generate-new-buffer (format delve-buffer-name-format buffer-name))))
    (with-current-buffer buf
      (delve-mode)
      (delve-lister-set-list buf items)
      (setq-local delve-local-initial-list items)
      (delve-lister-set-header buf heading)
      (delve-lister-goto buf :first)
      (delve-lister-highlight-mode))
    buf))

(defun delve-add-to-buffer (target-buffer item-or-list)
  "Add delve ITEM-OR-LIST to an existing delve TARGET-BUFFER."
  (unless (and (buffer-live-p target-buffer)
	       (delve-buffer-p target-buffer))
    (user-error "Target buffer does not exist"))
  (if (listp item-or-list)
      (delve-lister-add-sequence target-buffer item-or-list)
    (delve-lister-add target-buffer item-or-list))
  ;; unhighlight inserted item if buffer is not visible:
  (unless (get-buffer-window target-buffer)
    (delve-lister-sensor-leave target-buffer)))

(defun delve-buffer-p (buf)
  "Test if BUF is a delve buffer."
  (with-current-buffer buf
    (eq major-mode 'delve-mode)))

(defun delve-kill-buffer (buf)
  "Kill BUF w/o asking, but w/ feedback."
  (interactive (list (current-buffer)))
  (let* ((name (buffer-name buf)))
    (kill-buffer buf)
    (message "Killed buffer '%s'" name)))

(defun delve-all-buffers ()
  "Get all buffers in `delve mode'."
  (cl-remove-if-not #'delve-buffer-p (buffer-list)))

(defun delve-kill-all-buffers ()
  "Kill all delve buffers."
  (interactive)
  (cl-dolist (buf (cl-remove-if-not #'delve-buffer-p (buffer-list)))
    (kill-buffer buf)))

;; -----------------------------------------------------------
;; * Starting delve: entry point

(defun delve-create-searches (searches-plists)
  "Create a list of expandable delve search objects.

SEARCHES-PLISTS is a list of property lists. Each property list
will be passed as keyword arguments to `delve-make-page-search'.
Thus, all keywords for this function can be used. See the
documentation string of `delve-make-page-search' for all
available options.

Minimally, you should set the keywords `:name' (a string) and
`:constraint' (with a vector representing an SQL query). See
`delve-searches' for some valid examples."
  (cl-mapcar (lambda (the-plist)
	       (apply #'delve-make-page-search the-plist))
	     searches-plists))

;;;###autoload
(cl-defun delve (&optional item-or-list header-info (switch-buffer t))
  "Delve into the org roam zettelkasten with predefined searches.
Alternatively, display a list in a new buffer.

ITEM-OR-LIST can be nil, an expandable delve object or a list of
delve objects:

If ITEM-OR-LIST is nil, create the main buffer with predefined
searches.

If ITEM-OR-LIST is a delve object, expand this item and insert
the results of expanding in a new buffer.

If ITEM-OR-LIST is list, treat it as a list of delve objects and
insert them as they are.

When creating a new buffer, Optionally use HEADER-INFO for the
title.

If final optional argument SWITCH-BUFFER is non-nil (which is the
default setting), also switch to the newly created buffer.

Return the buffer created."
  (interactive)
  (unless org-roam-mode
    (with-temp-message "Turning on org roam mode..."
      (org-roam-mode)))
  (let* ((header-info (or header-info ""))
	 (buf (cond
	       ((null item-or-list)
		(delve-new-collection-buffer (append (delve-create-searches delve-searches)
						     (delve-db-query-roam-tags))
					     (delve--pretty-main-buffer-header)
					     "Main Buffer"))
	       ((listp item-or-list)
		(delve-new-collection-buffer item-or-list
					     (delve--pretty-collection-header header-info)
					     (concat "Collection " header-info)))
	       (t
		(delve-new-buffer-with-expansion item-or-list)))))
    (when switch-buffer
      (switch-to-buffer buf)
      (when delve-auto-delete-roam-buffer
	(when-let* ((win (get-buffer-window org-roam-buffer)))
	  (delete-window win))))
    buf))

(defun delve-get-fn-documentation (fn)
  "Return the first line of the documentation string of FN."
  (if-let ((doc (documentation fn)))
      (car (split-string doc "[\\\n]+"))
    (format "undocumented function %s" fn)))

(defun delve-prettify-delve-buffer-name (name)
  "Prettify NAME."
  (concat (if (and delve-use-icons-in-completions
		   (featurep 'all-the-icons))
	      (all-the-icons-faicon "bars")
	    "BUFFER")
	  " " name))

(defun delve-prettify-delve-fn-doc (doc)
  "Prettify DOC."
  (concat (if (and delve-use-icons-in-completions
		   (featurep 'all-the-icons))
	      (all-the-icons-faicon "plus")
	    "ACTION ")
	  " " doc))

(defun delve--all-delve-buffers-for-completion (bufs)
  "Return an alist with the names of BUFS prettified ."
  (mapcar (lambda (buf)
	    (cons (delve-prettify-delve-buffer-name (buffer-name buf)) buf))
	  bufs))

(defun delve-complete-on-bufs-and-fns (prompt bufs fns)
  "PROMPT user to select one of BUFS or FNS.
BUFS is a list of buffer, FNS a list of function names."
  (let* ((collection (append (delve--all-delve-buffers-for-completion bufs)
			     (mapcar (lambda (fn)
				       (cons (delve-prettify-delve-fn-doc (delve-get-fn-documentation fn))
					     fn))
				     fns))))
    (delve--acomplete prompt collection t)))

;;;###autoload
(defun delve-open-or-select ()
  "Open a new delve buffers or choose between existing ones."
  (interactive)
  (let* ((bufs       (delve-all-buffers))
	 (choice     (if bufs
			 (delve-complete-on-bufs-and-fns "Select delve buffer: "
							 (if (eq major-mode 'delve-mode)
							     (cl-remove (current-buffer) bufs)
							   bufs)
							 delve-user-actions)
		       'delve)))
    (if (bufferp choice)
	(progn
	  (switch-to-buffer choice)
	  (when delve-auto-delete-roam-buffer
	    (when-let* ((win (get-buffer-window org-roam-buffer)))
	      (delete-window win))))
      (funcall choice))))

(provide 'delve)
;;; delve.el ends here
