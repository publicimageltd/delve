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


;; -----------------------------------------------------------
;; * Dependencies

(require 'cl-lib)
(require 'org-roam)
(require 'lister)
(require 'lister-highlight)
(require 'delve-data-types)
(require 'delve-edit)

;; * Silence Byte Compiler

(declare-function all-the-icons-faicon "all-the-icons" (string) t)

;; * Global variables

(defvar delve-auto-delete-roam-buffer t
  "Delete visible *org roam* buffer when switchung to DELVE.")

(defvar delve-buffer-name "delve:"
  "Name of delve buffers.")

(defvar delve-version-string "0.3"
  "Current version of delve.")

(defvar delve-searches
  (list (delve-make-page-search :name "Orphaned Pages"
    			   :constraint [:where tags:tags :is :null])
	(delve-make-page-search :name "10 Last Modified"
			   :postprocess #'delve-db-query-last-10-modified)
	(delve-make-page-search :name "10 Most Linked To"
			   :constraint [:order-by (desc backlinks)
					:limit 10])
	(delve-make-page-search :name "10 Most Linked From"
			   :constraint [:order-by (desc tolinks)
					:limit 10])
	(delve-make-page-search :name "10 Most Linked"
			   :constraint [:order-by (desc (+ backlinks tolinks))
					:limit 10]))
  "A list of default searches offered when starting delve.")

;; * Buffer local variables for delve mode

(defvar-local delve-local-initial-list nil
  "Buffer list when first creating this delve buffer.")

;; -----------------------------------------------------------
;; * Item Mapper for the List Display (lister)

;; -- presenting a zettel object:

(defun delve-represent-tags (zettel)
  "Return all tags from GENERIC-ZETTEL as a propertized string."
  (when (delve-zettel-tags zettel)
    (concat "("
	    (propertize
	     (string-join (delve-zettel-tags zettel) ", ")
	     'face 'org-level-1)
	    ") ")))

(defun delve-represent-title (zettel)
  "Return the title of ZETTEL as a propertized string."
  (propertize (or
	       (delve-zettel-title zettel)
	       (delve-zettel-file zettel)
	       "NO FILE, NO TITLE.")
	      'face 'org-document-title))

(defun delve-format-time (time)
  "Return TIME in a more human readable form."
  (let* ((days         (time-to-days time))
	 (current-days (time-to-days (current-time))))
    (if (/= days current-days)
	(format-time-string "%b %d " time)
      (format-time-string " %R " time))))

(defvar delve-subtype-icons-alist
  '((delve-page     :default "    PAGE" :faicon "list-alt")
    (delve-tolink   :default "  TOLINK" :faicon "caret-left")
    (delve-backlink :default "BACKLINK" :faicon "caret-right"))
  "Alist associating a delve zettel subtype with a name and symbol.
The name and the symbol are determined by the properties
`:default' (for the name) and `:faicon' (for the symbol).

If `all-the-icons' is installed, use the symbol. Else, display
the name (a simple string).")

(defun delve-format-subtype (zettel)
  "Return the subtype of ZETTEL prettified."
  (let* ((subtype     (type-of zettel))
	 (type-plist  (alist-get subtype delve-subtype-icons-alist nil)))
    (concat 
     (if (and type-plist (featurep 'all-the-icons))
	 (all-the-icons-faicon (plist-get type-plist :faicon))
       (propertize (if type-plist
		       (plist-get type-plist :default)
		     "subtype?")
		   'face 'font-lock-constant-face))
     " ")))

(defun delve-represent-zettel (zettel)
  "Return ZETTEL as a pretty propertized string.
ZETTEL can be either a page, a backlink or a tolink."
  (list  (concat
	  ;; creation time:
	  (propertize
	   (delve-format-time (delve-zettel-mtime zettel))
	   'face 'org-document-info-keyword)
	  ;; subtype (tolink, backlink, zettel)
	  (delve-format-subtype zettel)
	  ;; associated tags:
	  (delve-represent-tags zettel)
	  ;; # of backlinks:
	  (propertize
	   (format "%d → " (or (delve-zettel-backlinks zettel) 0))
	   'face '(:weight bold))
	  ;; title:
	  (delve-represent-title zettel)
	  ;; # of tolinks:
	  (propertize
	   (format " →  %d" (or (delve-zettel-tolinks zettel) 0))
	   'face '(:weight bold)))))

;; -- presenting a search item:

(defun delve-represent-search (search)
  "Return propertized strings representing a SEARCH object."
  (list (concat (if (featurep 'all-the-icons)
		    (all-the-icons-faicon "search")
		  "Search:")
		" "
		(propertize
		 (delve-generic-search-name search)
		 'face 'org-level-2))))

;; -- presenting a tag object:

(defun delve-represent-tag (tag)
  "Return propertized strings representing a TAG object."
  (list (concat (if (featurep 'all-the-icons)
		    (all-the-icons-faicon "tag")
		  "Tag:")
		" "
		(propertize (delve-tag-tag tag) 'face 'org-level-1)
		(when (delve-tag-count tag)
		  (format " (%d)" (delve-tag-count tag))))))


;; the actual mapper:

(defun delve-mapper (data)
  "Transform DATA into a printable list."
  (pcase data
    ((pred delve-zettel-p)         (delve-represent-zettel data))
    ((pred delve-tag-p)            (delve-represent-tag data))
    ((pred delve-generic-search-p) (delve-represent-search data))
    (_        (list (format "UNKNOWN TYPE: %s"  (type-of data))))))

;; -----------------------------------------------------------
;; * Buffer basics

(defun delve-new-buffer-deprecated ()
  "Return a new DELVE buffer."
   (generate-new-buffer delve-buffer-name))

;; -----------------------------------------------------------
;; * Expand items by creating sublists

(defun delve-expand (item &rest operator-fns)
  "Collect the result of applying all OPERATOR-FNS on ITEM."
  (cl-loop for fn in operator-fns
	   append (let ((res (funcall fn item)))
		    (if (listp res) res (list res)))))

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

;; * Insert sublists by expanding items

(defun delve-expand-and-insert (buf position &rest operator-fns)
  "Insert result of OPERATOR-FNS applied to item at POSITION.
BUF must be a valid lister buffer.

POSITION is either an integer or the symbol `:point'."
  (let* ((pos  (pcase position
		 ((and (pred integerp) position) position)
		 (:point (with-current-buffer buf (point)))
		 (_      (error "Invalid value for POSITION: %s" position))))
	 (item (lister-get-data buf pos))
	 (res (apply #'delve-expand item operator-fns)))
    (if res
	(lister-insert-sublist-below buf pos res)
      (message "Cannot expand item; no results"))))

(defun delve-guess-expansion-and-insert (buf pos)
  "Guess useful expansion for item at POS and insert it.
BUF must be a valid lister buffer populated with delve items. POS
can be an integer or the symbol `:point'."
  (interactive (list (current-buffer) (point)))
  (let* ((position (pcase pos
		     ((and (pred integerp) pos) pos)
		     (:point (with-current-buffer buf (point)))
		     (_ (error "Invalid value for POS: %s" pos))))
	 (item (lister-get-data buf position))
	 (ops   (pcase item
		 ((pred delve-tag-p)
		  (list #'delve-operate-taglist))
		 ((pred delve-zettel-p)
		  (list #'delve-operate-backlinks  #'delve-operate-tolinks))
		 ((pred delve-generic-search-p)
		  (list #'delve-operate-on-search))
		 (_ nil))))
    (if ops
	(apply #'delve-expand-and-insert buf position ops)
      (user-error "No useful expansion found"))))
  
;; -----------------------------------------------------------
;;; * Delve Mode: Interactive Functions, Mode Definition 

(defun delve-refresh-buffer (buf)
  "Refresh all items in BUF."
  (interactive (list (current-buffer)))
  (when-let* ((all-data (lister-get-all-data-tree buf)))
    (lister-with-locked-cursor buf
      (with-temp-message "Updating the whole buffer, that might take some time...."
	(lister-set-list buf (delve-db-update-tree all-data))))))

(defun delve-expand-insert-tolinks ()
  "Insert all tolinks from the item at point."
  (interactive)
  (delve-expand-and-insert (current-buffer) :point #'delve-operate-tolinks))

(defun delve-expand-insert-backlinks ()
  "Insert all backlinks from the item at point."
  (interactive)
  (delve-expand-and-insert (current-buffer) :point #'delve-operate-backlinks))

(defun delve-expand-toggle-sublist ()
  "Close or open the item's sublist at point."
  (interactive)
  (let* ((buf (current-buffer))
	 (pos (point)))
    (if (lister-sublist-below-p buf pos)
	(lister-remove-sublist-below buf pos)
      (delve-guess-expansion-and-insert buf pos))))

(defun delve-initial-list ()
  "Populate the current delve buffer with predefined items."
  (interactive)
  (lister-set-list (current-buffer) (delve-db-query-roam-tags))
  (cl-dolist (search delve-searches)
    (lister-insert (current-buffer) :first search))
  (when (equal (window-buffer) (current-buffer))
    (recenter)))  

(defun delve-revert (buf)
  "Revert delve buffer BUF to its initial list."
  (interactive (list (current-buffer)))
  (with-current-buffer buf
    (lister-set-list buf delve-local-initial-list)
    (lister-goto buf :first)))

(defun delve-new-buffer (items heading)
  "Create a new delve buffer displaying ITEMS.
HEADING will be used to construct the list title and the buffer name."
  (let* ((buffer-title (concat delve-buffer-name " " (string-trim heading)))
	 (list-title   (concat "DELVE " delve-version-string " - " (string-trim heading)))
	 (buf          (generate-new-buffer buffer-title)))
    (with-current-buffer buf
      (delve-mode)
      (lister-set-list buf items)
      (setq-local delve-local-initial-list items)
      (lister-set-header buf list-title)
      (lister-goto buf :first)
      (lister-highlight-mode))
    buf))

;; TODO noch nicht 
(defun delve-new-from-sublist (buf pos)
  "Open new delve buffer with the current sublist at point."
  (interactive (list (current-buffer) (point)))
  (unless lister-local-marker-list
    (user-error "There are not items in this buffer"))
  (pcase-let* ((lister-display-transaction-p t)
	       (`(,beg ,end _ ) (lister-sublist-boundaries buf pos)))
    (lister-sensor-leave buf)
    (lister-set-list buf (lister-get-all-data-tree buf beg end)))
  (lister-goto buf :first))

;; TODO Currently unused
(defun delve-insert-zettel  ()
  "Choose a zettel and insert it in the current delve buffer."
  (interactive)
  (let* ((zettel (delve-db-query-all-zettel 'delve-make-page
					    [:order-by (asc titles:title)]))
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

(defun delve-visit-zettel ()
  "Visit the zettel item on point, leaving delve."
  (interactive)
  (let* ((data (lister-get-data (current-buffer) :point)))
    (unless (delve-zettel-p data)
      (user-error "Item at point is no zettel"))
    (find-file (delve-zettel-file data))
    (org-roam-buffer-toggle-display)))

(defun delve-add-tag ()
  "Add tags to the zettel at point."
  (interactive)
  (let* ((data (lister-get-data (current-buffer) :point)))
    (unless (delve-zettel-p data)
      (user-error "Item at point is no zettel"))
    (delve-edit-prompt-add-tag (delve-zettel-file data))))

(defun delve-remove-tag ()
  "Remove tags from the zettel at point."
  (interactive)
  (let* ((data (lister-get-data (current-buffer) :point)))
    (unless (delve-zettel-p data)
      (user-error "Item at point is no zettel"))
    (delve-edit-prompt-remove-tag (delve-zettel-file data))))

(defun delve-update-item-at-point ()
  "Update the item at point."
  (interactive)
  (let* ((new-item (delve-db-update-item (lister-get-data (current-buffer) :point))))
    (if (null new-item)
	(user-error "No update possible")
      (lister-replace (current-buffer) :point new-item)
      (message "Item updated"))))

(defun delve-action (data)
  "Act on the delve object DATA."
  (ignore data)
  (delve-visit-zettel))

(defvar delve-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lister-mode-map)
    ;; <RETURN> is mapped to #'delve-action (via lister-local-action)
    (define-key map "\t"               #'delve-expand-toggle-sublist)
    (define-key map (kbd "C-l")        #'delve-new-from-sublist)
    (define-key map "r"                #'delve-revert)
    (define-key map "."                #'delve-update-item-at-point)
    (define-key map (kbd "<left>")     #'delve-expand-insert-backlinks)
    (define-key map (kbd "<right>")    #'delve-expand-insert-tolinks)
    (define-key map (kbd "+")          #'delve-add-tag)
    (define-key map (kbd" -")          #'delve-remove-tag)
    (define-key map (kbd "g")          #'delve-refresh-buffer)
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

;; -----------------------------------------------------------
;; * Starting delve: entry points

(defun delve-all-buffers ()
  "Get all buffers in `delve mode'."
  (seq-filter (lambda (buf)
		(with-current-buffer buf
		  (derived-mode-p 'delve-mode)))
	      (buffer-list)))

;;;###autoload
(defun delve (&optional zettel-file)
  "Delve into the org roam zettelkasten.
Use ZETTEL-FILE as starting point if not nil."
  (interactive)
  (unless org-roam-mode
    (with-temp-message "Turning on org roam mode..."
      (org-roam-mode)))
  (let* ((items (if zettel-file
		    (list (delve-db-get-page-from-file zettel-file))
		  (append delve-searches (delve-db-query-roam-tags))))
	 (heading (if zettel-file
		      (org-roam--get-title-or-slug zettel-file)
		    "Initial list"))
	 (buf     (delve-new-buffer items heading)))
    (when zettel-file
      (lister-goto buf :first)
      (delve-guess-expansion-and-insert buf :point))
    (switch-to-buffer buf)
    (when delve-auto-delete-roam-buffer
      (when-let* ((win (get-buffer-window org-roam-buffer)))
	(delete-window win)))))

(defun delve-get-fn-documentation (fn)
  "Return the first line of the documentation string of fn."
  (if-let ((doc (documentation fn)))
      (car (split-string doc "[\\\n]+"))
    (format "undocumented function %s" fn)))

;; TODO Only prettify with faicons if var is explicitly set (opt-in)
(defun delve-prettify-delve-buffer-name (name)
  "Prettify NAME."
  (concat (if (featurep 'all-the-icons)
	      (all-the-icons-faicon "bars")
	    "BUFFER")
	  " " name))

;; TODO Only prettify with faicons if var is explicitly set (opt-in)
(defun delve-prettify-delve-fn-doc (doc)
  "Prettify DOC."
  (concat (if (featurep 'all-the-icons)
	      (all-the-icons-faicon "plus")
	    "ACTION ")
	  " " doc))

(defun delve-complete-on-bufs-and-fns (prompt bufs fns)
  "PROMPT user to complete on BUFS and FNs."
  (let* ((collection (append (mapcar (lambda (buf)
				       (cons (delve-prettify-delve-buffer-name (buffer-name buf)) buf))
				     bufs)
			     (mapcar (lambda (fn)
				       (cons (delve-prettify-delve-fn-doc (delve-get-fn-documentation fn))
					     fn))
				     fns)))
	 (selection  (completing-read prompt collection nil t)))
    (cdr (assoc selection collection #'string=))))

;;;###autoload
(defun delve-open-or-select ()
  "Open a new delve buffers or choose between existing ones."
  (interactive)
  (let* ((bufs       (delve-all-buffers))
	 (choice     (if bufs
			 (delve-complete-on-bufs-and-fns "Select delve buffer: " bufs '(delve))
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
