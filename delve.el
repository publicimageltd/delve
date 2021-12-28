;;; delve.el --- Delve into the depths of your org roam zettelkasten       -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021

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

;; Delve into the depths of your zettelkasten.

;; Terminology:
;;
;; - A "collection" is a list of Zettels, which can be
;; stored in a file.  A buffer thus 'visits' a collection (or creates
;; one without associated file.)

;;; Code:

;; * Dependencies

(require 'org-roam)
(require 'dash)
(require 'seq)
(require 'lister)
(require 'lister-mode)
(require 'button)

(require 'delve-transient)
(require 'delve-data-types)
(require 'delve-query)
(require 'delve-pp)
(require 'delve-store)
(require 'delve-edit)
(require 'delve-export)

;; * Silence Byte Compiler

(declare-function all-the-icons-faicon "all-the-icons" (string) t)
(declare-function consult-completing-read-multiple "consult")
(defvar org-roam-node-read--cached-display-format)

;;; * Customizable Variables

(defgroup delve nil
  "A zettelkasten tool on top of Org Roam."
  :group 'org-roam
  :link '(url-link :tag "Github" "https://github.com/publicimageltd/delve"))

(defcustom delve-dashboard-tags '("Dashboard")
  "Tags for which to insert query objects in the Dashboard.
Each element can be a tag or a list of tags."
  :group 'delve
  :type '(repeat (choice (string :tag "Tag")
                         (repeat (string :tag "Tags")))))

(defcustom delve-store-directory (concat (file-name-directory user-emacs-directory)
                                         "delve-store")
  "Path to a default directory for storing delve buffers in."
  :group 'delve
  :type  'directory)

(defcustom delve-display-path t
  "Turn on display of paths before the node's title.
If non-nil, insert the file title and the outline path, if they
exist, before the node's title.  This can cause quite long
entries."
  :group 'delve
  :type 'boolean)

;;; * Global Variables

(defvar delve-version "0.9"
  "Current version of delve.")

(defvar delve--no-icons nil
  "If bound, do not use any icons when creating output.")

(defvar delve-dashboard-name "Dashboard"
  "Name of the dashboard buffer.")

(defvar delve--select-history nil
  "History for `delve--select-collection-buffer'.")

(defvar delve--last-selected-buffer nil
  "Last buffer selected with `delve--select-collection-buffer'.")

;; * Buffer Local Variables

(defvar-local delve-local-storage-file nil
  "Associated local storage file.")

(defun delve-get-storage-file (buf)
  "Get the buffer local storage file for BUF."
  (buffer-local-value 'delve-local-storage-file buf))

(defvar-local delve-local-header-info "DELVE"
  "First line of the local Lister header.")

;; * Faces

(defgroup delve-faces nil
  "Faces used by Delve."
  :group 'Delve
  :group 'faces)

(defface delve-mark-face
  '((t (:inherit highlight)))
  "Face for highlighting manually marked items."
  :group 'delve-faces)

(defface delve-preview-face
  '((t (:inherit variable-pitch)))
  "Face for displaying preview (for non-monospaced display)."
  :group 'delve-faces)

(defface delve-path-face
  '((t (:inherit transient-inactive-value)))
  "Face for displaying the node path."
  :group 'delve-faces)

(defface delve-header-face
  '((t (:inherit org-document-title)))
  "Face for displaying the header of a Delve list."
  :group 'delve-faces)

(defface delve-note-face
  '((t (:inherit font-lock-comment-face)))
  "Face for displaying note items in a Delve list."
  :group 'delve-faces)

(defface delve-info-face
  '((t (:inherit warning)))
  "Face for displaying info items in a Delve list."
  :group 'delve-faces)

(defface delve-tags-face
  '((t (:inherit org-tag)))
  "Face for displaying roam tags in a Delve list."
  :group 'delve-faces)

(defface delve-title-face
  '((t (:inherit org-roam-title)))
  "Face for displaying org roam page titles in a Delve list."
  :group 'delve-faces)

(defface delve-pile-name-face
  '((t (:inherit org-document-title)))
  "Face for displaying the name of a Zettel pile."
  :group 'delve-faces)

(defface delve-subtype-face
  '((t (:inherit font-lock-constant-face)))
  "Face for displaying the subtype of a Delve item."
  :group 'delve-faces)

(defface delve-mtime-face
  '((t (:inherit org-document-info-keyword)))
  "Face for displaying the mtime of a Delve item."
  :group 'delve-faces)

(defface delve-atime-face
  '((t (:inherit org-document-info-keyword)))
  "Face for displaying the atime of a Delve item."
  :group 'delve-faces)

(defface delve-ctime-face
  '((t (:inherit org-document-info-keyword)))
  "Face for displaying the ctime of a Delve item."
  :group 'delve-faces)

(defface delve-nbacklinks-face
  '((t (:weight bold)))
  "Face for displaying the number of backlinks to a Delve zettel."
  :group 'delve-faces)

(defface delve-ntolinks-face
  '((t (:weight bold)))
  "Face for displaying the number of tolinks to a Delve zettel."
  :group 'delve-faces)

(defface delve-pile-face
  '((t (:inherit org-level-1)))
  "Face for displaying the title of a Delve pile."
  :group 'delve-faces)

(defface delve-query-face
  '((t (:inherit org-level-2)))
  "Face for displaying the title of a Delve query."
  :group 'delve-faces)

;;; * Assertions

(defun delve--assert-buf (&optional buf-or-ewoc error-msg)
  "Cancel if BUF-OR-EWOC does not belong to a Delve buffer with an Ewoc.
Check if the buffer belonging to BUF-OR-EWOC is a Delve buffer
and has a local Ewoc object.  BUF-OR-EWOC can be nil, a buffer
object or an ewoc object.  If BUF-OR-EWOC is nil, check the
current buffer.  When canceling, display standard error or
optionally use ERROR-MSG."
  (let ((buf (if (not buf-or-ewoc)
                 (current-buffer)
               (cl-etypecase buf-or-ewoc
                 (buffer buf-or-ewoc)
                 (ewoc   (ewoc-buffer buf-or-ewoc))))))
    (or (and (delve--buffer-p buf)
             ;; don't pass the ewoc object around too much
             (not (null (buffer-local-value 'lister-local-ewoc buf))))
        (error (or error-msg "Function has to be called in a Delve buffer")))))


;;; * The Lister Mapper

;; Type icon

(defun delve--type-as-string (delve-item)
  "Return a string or icon representing the type of DELVE-ITEM.
If the global variable `delve--no-icons' is bound, always only
return strings."
  (pcase-let ((`(,s ,icon-name)
              (pcase (type-of delve-item)
                ('delve--query   (list "QUERY" "search"))
                ('delve--pile    (list "PILE"  "list-ul"))
                ('delve--info    (list "INFO"  "info"))
                ('delve--heading (list "HEADING" "header"))
                ('delve--note    (list "NOTE"  "pencil"))
                ('delve--zettel
                 (if (eq 0 (delve--zettel-level delve-item))
                     (list "FILE" "file-text-o")
                   (list "NODE" "dot-circle-o")))
                (_              (list "TYPE?" "question")))))
    (if (and (featurep 'all-the-icons)
             (not delve--no-icons))
        (concat (all-the-icons-faicon icon-name) " ")
      (delve-pp--set-width s 8))))

;; Printing Zettel

(defun delve--node-title (node)
  "Return the title of NODE."
  (or (org-roam-node-title node)
      (org-roam-node-file node)
      "No file or title"))

(defun delve--get-button (label &rest properties)
  "Make a button LABEL in a temporary buffer and return it as a string.
Pass PROPERTIES to `insert-text-button', which creates the
button."
  (declare (indent 1))
  (with-temp-buffer
    (apply #'insert-text-button label properties)
    (buffer-string)))

(defun delve--tag-button (tag)
  "Return TAG as a button object."
  (delve--get-button tag
    'action (lambda (_) (message "Do something with %s" tag))))

(defun delve--string-join (strings &optional separator prefix)
  "Join all non-nil strings in STRINGS using SEPARATOR.
Optionally add string PREFIX to each non-nil item."
  (let ((strings (flatten-tree strings)))
    (when prefix
      (setq strings (--map (concat prefix it) strings)))
    (string-join strings separator)))

;; TODO Change that to buttons which open all links with that tag in a
;; new buffer?
(defun delve--tags-as-string (node)
  "Return the NODE's tags as a list of strings."
  (if-let ((tags (org-roam-node-tags node)))
      (delve--string-join tags ", " "#")
    "No tags."))

(defun delve--collect-link (link)
  "In an org mode buffer, collect data for buttonizing LINK.
LINK has to be a LINK element as returned by
`org-element-parse-buffer'."
  (when (equal (org-element-property :type link) "id")
    (list
     :beg    (copy-marker (org-element-property :begin link))
     :end    (copy-marker (org-element-property :end link))
     :id     (org-element-property :path link)
     :blanks (org-element-property :post-blanks link)
     :label  (buffer-substring-no-properties
              (org-element-property :contents-begin link)
              (org-element-property :contents-end link))
     :link   link)))

(defun delve--buttonize-link (link-plist)
  "In current buffer, replace link LINK-PLIST with a button."
  (let ((beg (plist-get link-plist :beg))
        (end (plist-get link-plist :end))
        (link  (plist-get link-plist :link))
        (label (plist-get link-plist :label))
        (blanks (plist-get link-plist :blanks))
        (id    (plist-get link-plist :id)))
    (delete-region beg (- end (or blanks 1)))
    (goto-char beg)
    (insert-text-button
     label
     'follow-link t
     'action (lambda (_)
               (org-link-open link))
     'keymap (let ((map (make-sparse-keymap)))
               (set-keymap-parent map button-map)
               (define-key map (kbd "i")
                 (lambda ()
                   (interactive)
                   (delve-insert-nodes-by-id (current-buffer) id)
                   (message "Inserted node %s"
                            (replace-regexp-in-string "\n" " " label))))
               map))))

(defun delve--prepare-preview (s)
  "Prepare preview string S for insertion.
Return the prepared string."
  (with-temp-buffer
    (insert s)
    (let ((org-inhibit-startup nil))
      (org-mode)
      (let* ((tree  (org-element-parse-buffer))
             (links (org-element-map tree 'link
                      #'delve--collect-link)))
        (font-lock-ensure)
        ;; FIXME This wipes out any different variable/monospace fonts,
        ;;       replacing it by one single font. It also ignores
        ;;       buffer specific face remappings, such as variable-pitch-mode.
        (add-face-text-property (point-min) (point-max) 'delve-preview-face)
        (cl-dolist (link links)
          (delve--buttonize-link link)))
      (buffer-string))))

(defun delve--zettel-strings (zettel)
  "Return a list of strings representing ZETTEL."
  (let ((node (delve--zettel-node zettel)))
    (list
     ;; -- Title
     (concat
      ;; Maybe mark out of sync:
      (when (delve--zettel-out-of-sync zettel)
        (delve-pp--add-face "* " 'warning))
      ;; Maybe display path:
      (when (and delve-display-path
                 (not (eq 0 (delve--zettel-level zettel))))
        (let ((path (delve--zettel-olp zettel)))
          (delve-pp--add-face
           (concat (delve--zettel-filetitle zettel) "/"
                   (string-join path "/")
                   (when path "/"))
           'delve-path-face)))
      ;; Display node title:
      (propertize (org-roam-node-title node) 'face 'delve-title-face))
     ;; -- Tag line:
     (propertize (delve--tags-as-string node) 'face 'delve-tags-face)
     ;; -- additional Zettel slots:
     (delve--zettel-info zettel)
     ;; -- Preview:
     (when-let ((preview (delve--zettel-preview zettel)))
       (split-string (delve--prepare-preview preview) "\n")))))

;; Printing a pile item

(defun delve--pile-size (pile)
  "Return information of the size of PILE."
  (format "(%d)" (length (delve--pile-zettels pile))))

(defun delve--pile-strings (pile)
  "Return a list of strings representing PILE."
  (list
   (delve-pp-fields pile '((delve--pile-size     (:set-face delve-pile-face))
                           (delve--pile-name     (:set-face delve-pile-face))))))

;; Printing Queries

(defun delve--query-strings (query)
  "Return a list of strings representing QUERY."
  (list (delve--query-info query)))

;; Printing Notes

(defun delve--note-s-to-list (string)
  "Format STRING as a paragraph and return it as a list of strings."
  (split-string
   (with-temp-buffer
     ;; (let ((org-inhibit-startup nil))
     ;;   (org-mode)
       (insert "  " string)
       (goto-char (point-min))
       (fill-paragraph)
;;       (font-lock-ensure)
       (buffer-string))
     "\n"))

(defun delve--note-strings (note)
  "Return a list of strings representing NOTE."
  (delve--note-s-to-list
     (delve-pp--add-face (delve--note-text note) 'delve-note-face)))

(defun delve--info-strings (info)
  "Return a list of strings representing INFO."
  (delve--note-s-to-list
     (delve-pp--add-face (delve--note-text info) 'delve-info-face)))

(defun delve--heading-strings (heading)
  "Return a list of strings representing HEADING."
  (list (delve-pp--add-face
         (delve--heading-text heading)
         'outline-1)))

;; The actual mapper

(defun delve-mapper (item)
  "Transform ITEM into a list of printable strings."
  (let* ((typestring  (delve--type-as-string item))
         (datastrings (cl-typecase item
                        (delve--zettel  (delve--zettel-strings item))
                        (delve--pile    (delve--pile-strings item))
                        (delve--query   (delve--query-strings item))
                        ;; always check the basic type "delve--note" last!
                        (delve--heading (delve--heading-strings item))
                        (delve--info    (delve--info-strings item))
                        (delve--note    (delve--note-strings item))
                        (t (list "no printer available for that item type")))))
    ;; hanging indent:
    (let* ((datastrings (flatten-tree datastrings))
           (pad         (make-string (length typestring) ? ))
           (first-line  (concat typestring (car datastrings)))
           (rest-lines  (--map (concat pad it)
                                (cdr datastrings))))
      (apply #'list first-line rest-lines))))

;; * Dynamic Header

(defun delve--db-info ()
  "Return strings with some basic infos."
  (let* ((nnodes  (caar (delve-query [:select (funcall count *) :from nodes])))
         (n0nodes (caar (delve-query [:select (funcall count *) :from nodes :where (= level 0)])))
         (tags    (caar (delve-query [:select :distinct tag :from tags :order :by asc]))))
    (list
     (propertize (format "Current db has %d nodes in %d files. %d tags are in use."
                         nnodes n0nodes (length tags))
                 'face 'font-lock-comment-face))))

(defun delve--header-function ()
  "Generate a Lister header item from local buffer vars."
  (flatten-tree
   (list (propertize delve-local-header-info 'face 'delve-header-face)
         (delve--db-info)
         (when delve-local-storage-file
           (concat
            (when lister-local-modified
              "* ")
            (propertize delve-local-storage-file 'face 'font-lock-string-face))))))

;; * Buffer and Dashboard

(defun delve--storage-files (&optional full-path)
  "Return all storage file names.
Only return the file name relative to `delve-store-directory'.
With optional argument FULL-PATH return the full path.
If `delve-store-directoy' does not exist, create it."
  (when (not (file-exists-p delve-store-directory))
    (if (y-or-n-p (format "Storage directory %s does not exist.  Create it? " delve-store-directory))
        (make-directory delve-store-directory t)
      (error "Storage directory %s does not exist" delve-store-directory)))
  (directory-files delve-store-directory full-path (rx string-start (not "."))))

(defun delve--storage-p (file-name)
  "Check if FILE-NAME represents an existing Delve storage file.
Just check the existence of the file, don't look at the contents."
  (-contains-p (delve--storage-files :full-path)
               (expand-file-name file-name)))

(defun delve--create-buffer-name (name)
  "Create a name for a Delve buffer using NAME."
  (concat "DELVE " name))

(defun delve--new-buffer (name &optional initial-list)
  "Create a new delve buffer NAME with INITIAL-LIST.
Return the buffer object."
  (let* ((name (generate-new-buffer-name (delve--create-buffer-name name))))
    (with-current-buffer (get-buffer-create name)
      (delve-mode)
      (when initial-list
        (lister-set-list lister-local-ewoc initial-list))
      ;; setting a major mode clears all buffer local variables
      (setq delve-local-header-info name)
      (lister-refresh-header-footer lister-local-ewoc)
      (current-buffer))))

(defun delve--create-tag-query (tags)
  "Create an item searching for nodes matching TAGS."
  (let* ((tags (-list tags)))
    (delve--query-create :info (format "Query for nodes matching %s"
                                       (delve--string-join tags " and " "#"))
                         :fn (lambda ()
                               (delve-query-nodes-by-tags tags)))))

(defun delve--create-unlinked-query ()
  "Create an item searching for unlinked nodes."
  (delve--query-create :info "Unlinked nodes"
                       :fn #'delve-query-unlinked))

(defun delve--new-dashboard ()
  "Return a new Delve dashboard buffer."
  (with-temp-message "Setting up dashboard..."
    (let* ((tag-queries (--map (delve--create-tag-query (-list it)) delve-dashboard-tags))
           (items       (list  tag-queries
                               (delve--create-unlinked-query)))
           (buf         (delve--new-buffer delve-dashboard-name (flatten-tree items))))
      (lister-goto (lister-get-ewoc buf) :first)
      buf)))

(defun delve--dashboard-p (&optional buf)
  "Check if BUF is the Delve dashboard."
  (string= (delve--create-buffer-name delve-dashboard-name)
           (buffer-name buf)))

(defun delve--dashboard-buf ()
  "Return the dashboard buffer, if existing."
  (seq-find #'delve--dashboard-p (buffer-list)))

(defun delve--buffer-p (&optional buf)
  "Check if BUF is a Delve buffer."
  (and (eq (buffer-local-value 'major-mode (or buf (current-buffer)))
           'delve-mode)))

(defun delve-buffer-list ()
  "Return a list of all Delve buffers."
  (seq-filter #'delve--buffer-p (buffer-list)))

(defun delve-unopened-storages ()
  "Return all Delve storage files which are not visited yet."
  (-map #'abbreviate-file-name
        (-difference (delve--storage-files :full-path)
                     (-map #'expand-file-name
                           (-keep #'delve-get-storage-file
                                  (delve-buffer-list))))))

(defun delve--prepare-candidates (cand key-fn suffix)
  "Return list CAND as an alist with a string key.
Use KEY-FN to create the string key.  It will have SUFFIX added
to the end, in parentheses.  To remove SUFFIX, use
`delve--remove-candidate-suffix'."
  (--group-by (format "%s (%s)" (funcall key-fn it) suffix)
              cand))

(defun delve--remove-candidate-suffix (cand)
  "Remove suffix in parentheses from CAND."
  (replace-regexp-in-string (rx string-start
                                (zero-or-more space)
                                (group (*? nonl))
                                (zero-or-more space)
                                "(" (one-or-more nonl) ")")
                            "\\1"
                            cand))

(defun delve--get-collection-buffer (buf-or-name)
  "Get a buffer containing the collection BUF-OR-NAME.
BUF-OR-NAME must be a string or a buffer object.  If it is a
buffer object, return it unchanged.  The string is either a
buffer name, a file name (with an extension) or the title for a
new collection.  If it is a file name, first check if it
designates an existing file or else use the base name as title
for a new collection."
  (if (bufferp buf-or-name)
      buf-or-name
    (or (get-buffer buf-or-name)
        (if (delve--storage-p buf-or-name)
            (delve--read-storage-file buf-or-name)
          (when (file-name-extension buf-or-name)
            (setq buf-or-name (file-name-sans-extension (file-name-base buf-or-name))))
          (delve--new-buffer buf-or-name)))))

(defun delve--select-collection-buffer (prompt)
  "Select Delve buffer, collection, or create a new buffer.
Use PROMPT as a prompt to prompt the user to choose promptly."
  (let* ((alist (append
                 ;; Existing buffers:
                 (delve--prepare-candidates (delve-buffer-list) #'buffer-name "Existing buffer")
                 ;; Dashboard, if not yet open:
                 (unless (delve--dashboard-buf)
                   (delve--prepare-candidates '("Dashboard") #'identity "Create"))
                 ;; Storage files:
                 (delve--prepare-candidates (delve-unopened-storages) #'identity "Open file")))
         (new-name (delve--remove-candidate-suffix
                    (completing-read prompt alist nil nil nil 'delve--select-history))))
    ;;
    (setq delve--last-selected-buffer
          (if (string= new-name "Dashboard")
              (delve--new-dashboard)
            (delve--get-collection-buffer new-name)))))

(defun delve-kill-all-delve-buffers ()
  "Kill all Delve buffers."
  (interactive)
  (seq-do #'kill-buffer (delve-buffer-list)))

;;; * Insert

(defun delve-insert-items (collection items)
  "Insert ITEMS in COLLECTION after current item and move point.
ITEMS is a list of Delve objects.  Move point to the last item
inserted.  For possible values for COLLECTION, see
`delve--get-collection-buffer'.  Return the collection buffer."
  (let* ((buf   (delve--get-collection-buffer collection))
         (ewoc  (lister-get-ewoc buf))
         (was-empty-p (lister-empty-p ewoc))
         (eolp  (lister-eolp buf))
         (next-node (lister-get-node-at ewoc :next))) ;; nil if no next
    (lister-insert-list-at ewoc :point items nil t)
    (unless was-empty-p
      ;; Switch to buffer for `lister-goto' to be effective. Simple
      ;; `with-current-buffer' does not work with underlying
      ;; `ewoc-next' (?!?!)
      (save-window-excursion
        (switch-to-buffer buf)
        (if eolp
            (goto-char (point-max))
          (lister-goto ewoc (or (ewoc-prev ewoc next-node)
                                :last)))))
    buf))

(defun delve-insert-nodes (collection nodes)
  "Insert NODES in COLLECTION after current item and move point.
NODES is a list of Org Roam nodes.  Move point to the last item
inserted.  For possible values for COLLECTION, see
`delve--get-collection-buffer'.  Return the collection buffer."
  (delve-insert-items collection (-map #'delve--zettel-create nodes)))

(defun delve-insert-nodes-by-id (collection ids)
  "Insert nodes with IDS in COLLECTION after current item and move point.
IDS can be either a single ID string or a list of IDs.  For
  possible values for COLLECTION, see
  `delve--get-collection-buffer'.  Return the collection buffer."
  (let* ((ids    (-list ids))
         (nodes  (delve-query-nodes-by-id ids)))
    (unless (eq (length nodes) (length ids))
      (error "Could not get all nodes; maybe the DB is out of sync?"))
    (delve-insert-nodes collection nodes)))

(defun delve--add-to-buffer (items prompt)
  "Add ITEMS to a Delve buffer and return that buffer object.
ITEMS is a list of Delve objects.  Use PROMPT when asking the
user to select or create a buffer."
  (delve-insert-items (delve--select-collection-buffer prompt) items))

;; * Sort

(defun delve--cmp-fn (sort-fn slot-fn &optional map-fn)
  "Return a function for sorting Zettels by SLOT-FN.
SORT-FN must be a binary predicate.  Access the value using
SLOT-FN.  Optionally pass the slot value through MAP-FN before
using it for sorting."
  (-on sort-fn (-compose (or map-fn #'identity) slot-fn)))

(cl-defstruct (delve-dual-cmp (:constructor delve-dual-cmp--create))
  "Structure holding two comparator functions for sorting in
ascending and descending order, and a description for user
selection."
  comp-asc comp-desc desc)

(defmacro delve--build-dual-cmp (name desc sort-fn-asc sort-fn-desc slot-fn &optional map-fn)
  "Define a dual comperator object NAME with DESC.
A dual comperator object holds two comparator functions.
SORT-FN-ASC sort is responsible for sorting in ascending order
and SORT-FN-DESC for the other direction.  For the meaning of
SLOT-FN and MAP-FN, see `delve--cmp-fn'."
  (declare (indent 1))
  `(defvar ,name
     (delve-dual-cmp--create :comp-asc (delve--cmp-fn ,sort-fn-asc ,slot-fn ,map-fn)
                             :comp-desc (delve--cmp-fn ,sort-fn-desc ,slot-fn ,map-fn)
                             :desc ,desc)
     ,(concat "Structure holding a dual comporator to sorty by " (downcase desc))))

;; * Define the comparators for sorting

(delve--build-dual-cmp delve-2cmp--title
  "title"
  #'string< #'string> #'delve--zettel-title)

(delve--build-dual-cmp delve-2cmp--tag-n
  "number of tags"
  #'< #'> #'delve--zettel-tags #'length)

(delve--build-dual-cmp delve-2cmp--level
  "nesting level"
  #'< #'> #'delve--zettel-level)

(delve--build-dual-cmp delve-2cmp--file-title
  "file title"
  #'string< #'string> #'delve--zettel-filetitle)

;; * Utilities to use comparators in transients

(defvar delve--all-2cmps-symbols
  '(delve-2cmp--title
    delve-2cmp--file-title
    delve-2cmp--level
    delve-2cmp--tag-n)
  "*Internal* All comperator names, as symbols.")

(defvar delve--all-2cmps
  (-map #'eval delve--all-2cmps-symbols)
  "*Internal* All comparators for sorting.")

(defun delve--cmp-info (dual-cmp slot-name)
  "Return informative string about the comparator DUAL-CMP.
SLOT-NAME.  SLOT-NAME must be either \"comp-asc\" or
\"comp-desc\", designating the respective slot in the dual
comperator object. Return nil if one of the
args is nil."
  (when (and dual-cmp slot-name)
    (format "by %s (%s)" (delve-dual-cmp-desc dual-cmp)
            (if (equal slot-name "comp-desc")
                "from Z to A"
              "from A to Z"))))

(defun delve--get-dual-cmp-by-string (cmp-string)
  "Return a dual comparator object using CMP-STRING.
CMP-STRING must be the name of a symbol, the value of which is an
instance of a `delve-dual-cmp' object and is listed in the
variable `delve--all-2cmps-symbols'."
  (when cmp-string
    (let* ((cmp-sym (intern cmp-string)))
      (unless (-contains? delve--all-2cmps-symbols cmp-sym)
        (error "No dual comparator object %s" cmp-string))
      ;; don't do this at home, guys, it's ev(i|a)l:
      (eval cmp-sym))))

(defun delve--get-cmp-fn (dual-cmp slot-name)
  "Return comparator fn of DUAL-CMP using SLOT-NAME.
DUAL-CMP must be a dual comparator object.  SLOT-NAME must be the
string (not symbol!) \"comp-asc\" or \"comp-desc\", designating
the respective slot in the dual comperator object.  Return nil if
one of the args is nil."
  (when (and dual-cmp slot-name)
    (let ((order-sym (intern slot-name)))
      (unless (-contains? '(comp-desc comp-asc) order-sym)
        (error "Wrong slot name: %s" slot-name))
      (cl-struct-slot-value 'delve-dual-cmp order-sym dual-cmp))))

;;; * Define Sort Transient

(defclass delve--transient-cmp-switches (delve-transient-switches)
  ((choices        :initform (-map #'symbol-name delve--all-2cmps-symbols))
   (pretty-choices :initform (-map #'delve-dual-cmp-desc delve--all-2cmps)))
  "Transient switch with preset values for choosing a comparator.")

(defclass delve--transient-cmp-order (delve-transient-switches)
  ((choices        :initform '("comp-asc" "comp-desc")) ;; slots in delve-dual-cmp
   (pretty-choices :initform '("A → Z" "Z → A"))))

(transient-define-suffix delve--do-sort (&rest args)
  "Do the actual sorting with ARGS defined in transient `delve-sort'."
  (interactive (list (transient-args transient-current-command)))
  (when (equal args '(nil))
    (error "No arguments defined for sorting?"))

  (let* ((plist (delve-transient--args-to-plist args))
         (sort1  (plist-get plist :sort1))
         (sort2  (plist-get plist :sort2))
         (order1 (plist-get plist :order1))
         (order2 (plist-get plist :order2)))

    ;; Assert that second sorting criterion is consistent
    (when (or (plist-get plist :sort2) (plist-get plist :order2))
      (unless (and (plist-get plist :sort2) (plist-get plist :order2))
        (user-error "Second sorting criterion not fully defined: %s missing"
                    (if (plist-get plist :sort2)
                        "order (ascending or descending)"
                      "criterion"))))
    ;; Do it
    (let* ((cmp1 (delve--get-dual-cmp-by-string sort1))
           (cmp2 (delve--get-dual-cmp-by-string sort2)))
      ;;
      (lister-save-current-node lister-local-ewoc
          (lister-sort-sublist-at lister-local-ewoc :point
                                  (-non-nil (list (delve--get-cmp-fn cmp1 order1)
                                                  (delve--get-cmp-fn cmp2 order2)))))
      ;;. ..and talk about it:
      (let ((s1   (delve--cmp-info cmp1 order1))
            (s2   (delve--cmp-info cmp2 order2)))
        (message (concat "Sorting "
                         (when (and s1 s2) "first ")
                         s1
                         (when s2 (concat ", then " s2))
                         "."))))))

(transient-define-suffix delve--echo-transient-value (&rest _)
  "Echo the current value of the current transient for debugging purposes."
  (interactive)
  (message "Value: %S" (transient-get-value)))

(transient-define-prefix delve--key--sort ()
  "Sort"
  [["First sorting criterion"
    ("s" "Sort by" "--sort1="  :class delve--transient-cmp-switches
     :allow-nil nil)
    ("o" "Order"   "--order1=" :class delve--transient-cmp-order
     :allow-nil nil)]]
   [["Second sorting criterion"
     ("S" "Sort by" "--sort2="  :class delve--transient-cmp-switches)
     ("O" "Order" "--order2=" :class delve--transient-cmp-order)]]
  [["Actions on (sub-)list"
    ("r" "Reverse" delve--key--reverse)
    ("x" "Sort" delve--do-sort)]
   ;; fake column
   ["        "
    ""]
   ["Quit"
    ("q" "Quit" transient-quit-one)
    ;;("v" "Value" delve--echo-transient-value :transient t)
    ]])

;; * Remote Editing - Background Utilites

(defun delve--sync-zettel (zettels)
  "Force sync of all ZETTELS with the org roam db.
First update the db, then reload the ZETTELS.  Do not redisplay
anything; that's up to the calling function."
  (when-let* ((filelist (-map #'delve--zettel-file zettels)))
    (cl-dolist (file (seq-uniq filelist #'string=))
      (org-roam-db-update-file file))
    ;; we first prefetch all nodes in one query, then update the
    ;; zettel objects each one by one:
    (let ((hash (delve-store--prefetch-ids (-map #'delve--zettel-id zettels))))
      (cl-dolist (z zettels)
        (setf (delve--zettel-node z)       (gethash (delve--zettel-id z) hash)
              (delve--zettel-out-of-sync z) nil)
        (when (delve--zettel-preview z)
          (setf (delve--zettel-preview z)  (delve--get-preview-contents z)))))))

(defun delve--out-of-sync-p (node)
  "Check if NODE has a zettel item which is out of sync."
  (let ((data (lister-node-get-data node)))
    (and (eq (type-of data) 'delve--zettel)
         (delve--zettel-out-of-sync data))))

(defun delve--taint-nodes (nodes)
  "Mark all zettel in NODES as out of sync."
  (cl-dolist (node nodes)
    (setf (delve--zettel-out-of-sync (lister-node-get-data node)) t)))

(defun delve--refresh-nodes (ewoc nodes)
  "Redisplay and unmark all NODES in EWOC."
  (lister-save-current-node ewoc
    (cl-dolist (node nodes)
      (lister-mark-unmark-at ewoc node nil)
      (lister-refresh-at ewoc node))))

(defun delve--taint-and-refresh-marked-nodes (ewoc)
  "Set all marked zettels in EWOC as out of sync and redisplay them.
Also unmark the items."
  (let ((nodes (lister-collect-nodes ewoc :first :last #'lister-node-marked-p)))
    (delve--taint-nodes nodes)
    (delve--refresh-nodes ewoc nodes)))

;;; * Key handling / Commands

(cl-defun delve--find-zettel-by-id (id &optional (buf (current-buffer)))
  "Find first ewoc node with ID in Delve buffer BUF.
Searches both zettels and piles."
  (cl-labels ((match-id (z)
                        (equal (delve--zettel-id z) id))
              (find-zettel (delve-object)
                           (cl-typecase delve-object
                               (delve--zettel (match-id delve-object))
                               (delve--pile   (seq-find #'match-id (delve--pile-zettels delve-object))))))
    (lister-first-matching (lister-get-ewoc buf) :first
                           #'find-zettel)))

(defun delve--truncate-string (s width)
  "Truncate string S to maximal WIDTH."
  (if (> (string-width s) width)
      (substring s 0 width)
    s))

(defun delve--zettel-stub (zettel)
  "Return a shortened title of ZETTEL.
Return titel unchanged if it has less than 40 characters; else
produce a shortened version with each word of the title truncated
to 5 characters."
  (let* ((stub-width 40)
         (s          (delve--zettel-title zettel)))
    (if (> (string-width s) stub-width)
        (delve--truncate-string
         (string-join
          (--map (delve--truncate-string it 5)
                 (split-string s))
          " ")
         stub-width)
      s)))

;;; Generic key related stuff

(defun delve--push-to-global-mark-ring ()
  "Push current point on the global mark ring."
  (add-to-history 'global-mark-ring (copy-marker (point-marker)) global-mark-ring-max t))

(defun delve--type-p (item &rest types)
  "Check if ITEM is one of TYPES."
  (memq (type-of item) types))

(defun delve--current-item (&optional types ewoc pos)
  "Get the item bound to the current Lister node.
Use the current buffer or the buffer of EWOC, if non-nil.  TYPES
is a type symbol or a list of type symbols.  If the item is not
of type TYPES, throw an error.  Use the position at point in EWOC
or POS, if supplied.  Skip any typechecking if TYPES is nil."
  (let ((ewoc (or ewoc lister-local-ewoc)))
    (delve--assert-buf ewoc "Command must be called in a Delve buffer")
    (let ((item (lister-get-data-at ewoc (or pos :point))))
      (if (or (not types)
              (apply #'delve--type-p item (-list types)))
          item
        (error "The item at point is not of the right type for that command")))))

;; TODO Refactor: delve--current-item nearly does the same
(defun delve--current-item-or-marked (&optional types)
  "Get either all marked items or the item at point, marking it.
Use the current buffer.  Always return a list.  TYPES is a type
symbol or a list of type symbols.  If on of the items is not of
type TYPES, throw an error."
  (let ((ewoc lister-local-ewoc))
    (delve--assert-buf ewoc "Command must be called in a Delve buffer")
    (delve--maybe-mark-region ewoc)
    (unless (lister-items-marked-p ewoc)
      (lister-mark-unmark-at ewoc :point t))
    (let ((items (lister-get-marked-list ewoc)))
      (when types
        (cl-dolist (item items)
          (unless (apply #'delve--type-p item (-list types))
            (error "The item at point is not of the right type for that command"))))
      items)))

(defun delve--insert-or-select (items do-select)
  "Insert ITEMS into current collection or select one.
Use the current buffer unless DO-SELECT is non-nil.  If inserting
in current buffer, insert as a sublist below point.  Else add to
the selected collection after point, moving it.

Do nothing and return nil if ITEMS is empty, else print a message
on how many items have been inserted and return non-nil."
  (when items
    (let* ((buf (if do-select
                    (delve--select-collection-buffer "Insert new items into collection: ")
                  (current-buffer))))
      ;; TODO Warn when list is too big
      (let* ((n    (length items))
             (msg  (format "Inserting %d zettels..." n)))
        (with-temp-message msg
          (if (eq buf (current-buffer))
              (lister-insert-sublist-below lister-local-ewoc :point items)
            (delve-insert-items buf items))
          (message (concat msg "done")))
        (when do-select
          (message "Added %d items to collection buffer '%s'" n (buffer-name buf))))
      t)))

;; TODO Isn't the same function used in delve-minor-mode?
(defun delve--maybe-mark-region (ewoc)
  "In EWOC, mark all items in the active region.
If no region is active, do nothing."
  (when (use-region-p)
    (lister-mode--mark-unmark-region ewoc
                                     (region-beginning)
                                     (region-end)
                                     t)))

(defun delve--select-nodes (nodes-or-node-fn prompt)
  "Let the user select multiple nodes from NODES-OR-NODE-FN.
NODES-OR-NODE-FN can be either a list of nodes or a function
which returns such a list.  PROMPT is padded with spaces and
passed to completing read."
  (setq org-roam-node-read--cached-display-format nil)
  ;; Standard completing-read-multiple can't work with node
  ;; candidates, so check if alternatives are available:
  (cl-letf (((symbol-function 'completing-read-multiple)
             (cond
              ((featurep 'consult) #'consult-completing-read-multiple)
              (t #'completing-read))))
    ;; ...collect and select:
    (let* ((template       (org-roam-node--process-display-format org-roam-node-display-template))
           (node-alist     (with-temp-message "Collecting nodes..."
                             (--map (org-roam-node-read--to-candidate it template)
                                    (if (listp nodes-or-node-fn)
                                        nodes-or-node-fn
                                      (funcall nodes-or-node-fn)))))
           (prompt         (concat " " (string-trim prompt) " "))
           (node-selected  (if node-alist
                               (completing-read-multiple prompt node-alist)
                             (user-error "No nodes to choose from"))))
      (--map (alist-get it node-alist nil nil #'string=)
             (-list node-selected)))))

;;; * Key commands working with the "item at point"

;; Every key command in this subsection should have an optional prefix
;; argument even if it is not used, so that it can be used by
;; delve--key--dispatch.

(defun delve--key--add-tags (zettels &optional prefix)
  "Add tags to all marked ZETTELS or the zettel at point.
Optional argument PREFIX is currently not used."
  (interactive (list (delve--current-item-or-marked 'delve--zettel)))
  (ignore prefix)
  (delve-edit--add-tags zettels)
  (delve--taint-and-refresh-marked-nodes lister-local-ewoc)
  (message "Edited %d nodes" (length zettels)))

(defun delve--key--remove-tags (zettels &optional prefix)
  "Remove tags from all marked ZETTELS or the zettel at point.
Optional argument PREFIX is currently not used."
  (interactive (list (delve--current-item-or-marked 'delve--zettel)))
  (ignore prefix)
  (delve-edit--remove-tags zettels)
  (delve--taint-and-refresh-marked-nodes lister-local-ewoc)
  (message "Edited %d nodes" (length zettels)))

(defun delve--key--open-zettel (zettel &optional prefix)
  "Open the ZETTEL at point.
Optional argument PREFIX is currently not used."
  (interactive (list (delve--current-item 'delve--zettel)))
  (ignore prefix)
  (delve--push-to-global-mark-ring)
  (let ((buf (org-roam-node-find-noselect (delve--zettel-node zettel))))
    (switch-to-buffer buf)
    (when (org-invisible-p) (org-show-context))))


;; * Copy / Paste

(defun delve--lister-node-at-pos (ewoc pos)
  "Return lister node at POS in Delve EWOC."
  (with-current-buffer (ewoc-buffer ewoc)
    (save-excursion
      (goto-char pos)
      (lister-get-node-at ewoc :point))))

(defun delve--tokenize-filter (beg end &optional delete)
  "In current buffer, return the region BEG END as a tokenized string.
Optionally also DELETE the items.  This function is intended to
be used as a value for `filter-buffer-substring-function'."
  (delve--assert-buf nil "Filter function has to be called in a Delve buffer")

  (let* (acc verb
         (move-counter 0)
         (ewoc     lister-local-ewoc)
         (node-beg (delve--lister-node-at-pos ewoc beg))
         (node-end (delve--lister-node-at-pos ewoc (1- end))))

    (unless (and node-beg node-end)
      (error "Could not find any list items in region %s - %s"
             beg end))

    (lister-dolist (ewoc delve-object node-beg node-end)
      (push (delve-store--tokenize-object delve-object) acc))

    (setq verb "Copied")
    (when delete
      ;; Decrease any sublists:
      (lister-walk-nodes ewoc
                         (lambda (ewoc node)
                           (lister-with-sublist-below ewoc node beg end
                             (cl-incf move-counter)
                             (lister-walk-nodes ewoc #'lister-move-item-left beg end)))
                         node-beg node-end)
      ;; and delete the items
      (lister-delete-list ewoc node-beg node-end)
      (setq verb "Killed"))

    (when (or delete
              (pcase this-command
                ('kill-region t)
                ('kill-ring-save t)
                (_ nil)))
      (message "%s %d items%s" verb (length acc)
               (if (> move-counter 0)
                   (format ", realigning %d sublists" move-counter)
                 "")))

    (propertize
     (concat "("
             (string-join (--map (format "%S" it) (nreverse acc)))
             ")")
     'yank-handler
     '(delve--yank-handler))))

(defun delve--yank-handler (s)
  "Insert tokenized string S as a Delve object at point."
  (let* ((backends (delve-export--available-backends delve-export--yank-handlers))
         (objects (delve-store--create-object-list (ignore-errors (read (substring-no-properties s))))))
    (cond
     ;; insert in Delve buffer
     ((derived-mode-p 'delve-mode)
      (if (not objects)
          (error "Invalid kill value; could not insert object(s)")
        (lister-insert-list-at lister-local-ewoc :point objects)
        (message "Inserted %d items" (length objects))))
     ;; insert according to export backend
     (backends
      (if (not objects)
          (error "Invalid kill value; could not insert object(s)")
        (delve-export--insert (current-buffer) (delve-export--select-backend backends) objects)))
     ;; insert in any other buffer
     (t
      (insert s)))))

(defun delve--yank (&optional arg)
  "Yank last kill, if it is a Delve token string.
Option ARG is currently ignored."
  (interactive)
  (ignore arg)
  (delve--assert-buf nil "This yank function has to be called in a Delve buffer")
  (let* ((yank (current-kill 0)))
    (if (eq (car (get-text-property 0 'yank-handler yank))
            'delve--yank-handler)
        (delve--yank-handler yank)
      (user-error "Current kill is not a Delve object; cannot yank"))))

;; (current-kill 0)

;; * Insert backlinks and fromlinks

(defvar delve--backlink-info "Backlink to %s"
  "Info format string for backlinks.")

(defvar delve--fromlink-info "Link from %s"
  "Info format string for fromlinks.")

(defun delve--verzetteln (nodes &optional info)
    "Turn all NODES into Delve zettel.
Optional fill the info slot with the string INFO."
    (--map (let ((z (delve--zettel-create it)))
             (setf (delve--zettel-info z) info)
             z)
           nodes))

(defun delve--zettel-info-stub (format-string zettel)
  "Create a buttonized info stub for ZETTEL using FORMAT-STRING.
Replace '%s' in FORMAT-STRING with the button."
  (format format-string
          (delve--get-button (delve--zettel-stub zettel)
            'action (lambda (_)
                      (if-let ((node (delve--find-zettel-by-id (delve--zettel-id zettel))))
                          (progn
                            (push-mark)
                            (lister-goto lister-local-ewoc node))
                        (user-error "Zettel not found in this buffer"))))))

(defun delve--get-infolinks (zettel query-fn info-format)
  "Call QUERY-FN with ID and add formatted info.
INFO-STRING must contain '%s' which is replaced by the stub of
ZETTEL."
  (-some--> (funcall query-fn (delve--zettel-id zettel))
    (delve--verzetteln it (delve--zettel-info-stub info-format zettel))))

(defun delve--key--backlinks (zettel &optional select-target)
  "Insert all backlinks to ZETTEL.
With SELECT-TARGET, select target collection for the link list,
else insert it as a sublist below point."
  (interactive (list (delve--current-item 'delve--zettel) current-prefix-arg))
  (let* ((nodes   (delve-query-backlinks-by-id (delve--zettel-id zettel)))
         (stub    (delve--zettel-info-stub delve--backlink-info zettel))
         (zettels (delve--verzetteln nodes stub)))
    (or (delve--insert-or-select zettels select-target)
        (message "No backlinks"))))

(defun delve--key--fromlinks (zettel &optional select-target)
  "Insert fromlinks of current ZETTEL.
With SELECT-TARGET, select target collection for the link list,
else insert it as a sublist below point."
  (interactive (list (delve--current-item 'delve--zettel) current-prefix-arg))
  (let* ((nodes   (delve-query-fromlinks-by-id (delve--zettel-id zettel)))
         (stub    (delve--zettel-info-stub delve--fromlink-info zettel))
         (zettels (delve--verzetteln nodes stub)))
    (or (delve--insert-or-select zettels select-target)
        (message "No tolinks to this zettel node"))))

(defun delve--key--links (zettel &optional select-target)
  "Insert all backlinks and fromlinks from ZETTEL.
With SELECT-TARGET, select target collection for the link list,
else insert it as a sublist below point."
  (interactive (list (delve--current-item 'delve--zettel) current-prefix-arg))
  (let* ((id       (delve--zettel-id zettel))
         (b-zettel (-some--> (delve-query-backlinks-by-id id)
                     (delve--verzetteln it (delve--zettel-info-stub delve--backlink-info zettel))))
         (f-zettel (-some--> (delve-query-fromlinks-by-id id)
                     (delve--verzetteln it (delve--zettel-info-stub delve--fromlink-info zettel)))))
    (or (delve--insert-or-select (append b-zettel f-zettel) select-target)
        (message "This zettel has no backlinks or fromlinks (you should change that)"))))

(defun delve--key--insert-backlink (zettel &optional select-target)
  "Select and insert backlinks from current ZETTEL.
With SELECT-TARGET, select target collection for the link list,
else insert it as a sublist below point."
  (interactive (list (delve--current-item 'delve--zettel) current-prefix-arg))
  (let* ((all-nodes  (delve-query-backlinks-by-id (delve--zettel-id zettel)))
         (nodes      (delve--select-nodes all-nodes "Insert backlinks:"))
         (zettels    (delve--verzetteln nodes (delve--zettel-info-stub delve--backlink-info zettel))))
    (or (delve--insert-or-select zettels select-target)
        (user-error "No backlinks to insert"))))

(defun delve--key--insert-fromlink (zettel &optional select-target)
  "Select and insert fromlinks from current ZETTEL.
With SELECT-TARGET, select target collection for the link list,
else insert it as a sublist below point."
  (interactive (list (delve--current-item 'delve--zettel) current-prefix-arg))
  (let* ((all-nodes  (delve-query-backlinks-by-id (delve--zettel-id zettel)))
         (nodes      (delve--select-nodes all-nodes "Insert fromlinks:"))
         (zettels    (delve--verzetteln nodes (delve--zettel-info-stub delve--fromlink-info zettel))))
    (or (delve--insert-or-select zettels select-target)
        (user-error "No fromlinks to insert"))))

(defun delve--key--insert-pile (pile &optional select-target)
  "Insert all zettels in PILE, removing it.
With SELECT-TARGET, select target for the results, else insert it
as a sublist below point."
  (interactive (list (delve--current-item '(delve--pile)) current-prefix-arg))
  (let ((pile-node (lister-get-node-at lister-local-ewoc :point)))
    (delve--insert-or-select (delve--pile-zettels pile) select-target)
    (unless select-target
      (lister-delete-at lister-local-ewoc pile-node))))

(defun delve--key--insert-query (query &optional select-target)
  "Insert results of QUERY.
With SELECT-TARGET, select target for the results, else insert it
as a sublist below point."
  (interactive (list (delve--current-item '(delve--query)) current-prefix-arg))
  (or (delve--insert-or-select (-map #'delve--zettel-create (funcall (delve--query-fn query))) select-target)
      (user-error "No matching items found")))

(defun delve--key--insert-query-or-pile (item &optional select-target)
  "Insert results from ITEM, either a query or a pile object.
With SELECT-TARGET, select target for the results, else insert it
as a sublist below point."
  (interactive (list (delve--current-item '(delve--query delve--pile)) current-prefix-arg))
  (cl-typecase item
    (delve--pile (delve--key--insert-pile item select-target))
    (delve--query (delve--key--insert-query item select-target))))

;;

(defun delve--get-preview-contents (zettel)
  "Get the raw preview contents for ZETTEL."
  (org-roam-with-temp-buffer (delve--zettel-file zettel)
    (org-roam-preview-get-contents
     (delve--zettel-file zettel)
     (if (eq (delve--zettel-level zettel) 0)
         (if (org-goto-first-child)
             (1- (point))
           (point-max))
       (delve--zettel-point zettel)))))

(defun delve--key--toggle-preview (zettel &optional prefix)
  "Toggle the display of the preview of ZETTEL.
With PREFIX, open ZETTEL's file in a buffer."
  (interactive (list (delve--current-item 'delve--zettel) current-prefix-arg))
  (if prefix
      (delve--key--open-zettel zettel)
    (let ((preview (and (not (delve--zettel-preview zettel))
                        (or (delve--get-preview-contents zettel)
                            "No preview available"))))
      (setf (delve--zettel-preview zettel) preview)
      (lister-refresh-at lister-local-ewoc :point))))

(defun delve--key--roam (zettel &optional prefix)
  "Open the org roam buffer for ZETTEL.
Optional argument PREFIX is currently not used."
  (interactive (list (delve--current-item 'delve--zettel)))
  (ignore prefix)
  (org-roam-buffer-display-dedicated (delve--zettel-node zettel)))

(defun delve--key--tab (ewoc &optional prefix)
  "In EWOC, expand item at point or toggle subtree visibility.
With PREFIX, expand all hidden subtrees in the EWOC's buffer."
  (interactive (list lister-local-ewoc current-prefix-arg))
  (if (lister-sublist-below-p ewoc :point)
      (lister-mode-cycle-sublist ewoc :point prefix)
    (let ((item (delve--current-item)))
      (cl-typecase item
        (delve--query  (delve--key--insert-query item prefix))
        (delve--pile   (delve--key--insert-pile  item prefix))
        (delve--zettel (delve--key--links        item prefix))
        (t             (user-error "Cannot do anything useful here"))))))

(defun delve--key--reverse (ewoc)
  "In EWOC, reverse the order of all items in current (sub-) list."
  (interactive (list lister-local-ewoc))
  (lister-with-sublist-at ewoc :point beg end
    (lister-save-current-node ewoc
        (lister-reverse-list ewoc beg end)))
  (message "Reversed order of list items"))

;;; * Key commands not bound to a specific item at point

;; Add heading

(defun delve--key--insert-heading (ewoc)
  "Edit a heading at point in EWOC."
  (interactive (list lister-local-ewoc))
  (let ((heading (read-string "Heading: ")))
    (lister-insert-at ewoc :point (delve--heading-create :text heading) nil (lister-eolp))))

;; Edit buffer header

(defun delve--key--edit-title (ewoc)
  "Edit the title of the Delve buffer linked with EWOC."
  (interactive (list lister-local-ewoc))
  (with-current-buffer (ewoc-buffer ewoc)
    (setq-local delve-local-header-info
                (read-string "New title: " "DELVE " delve-local-header-info))
    (lister-refresh-header-footer ewoc)))

;; Force refresh

(defun delve--key--sync (ewoc &optional prefix)
  "In EWOC, sync all zettel out of sync with the org roam database.
With PREFIX, force sync all marked zettel or, if none is marked,
the zettel at point."
  (interactive (list lister-local-ewoc current-prefix-arg))
  (when prefix
    (delve--current-item-or-marked 'delve--zettel))
  (let ((nodes (lister-collect-nodes ewoc
                                     :first :last
                                     (if prefix
                                         #'lister-node-marked-and-visible-p
                                       #'delve--out-of-sync-p))))
    (unless nodes
      (error (if prefix
                 "Cannot sync items; no items marked"
               "Cannot sync items; no items out of sync.  Maybe use prefix arg to force resyncing")))
    (delve--sync-zettel (-map #'lister-node-get-data nodes))
    (lister-save-current-node ewoc
      (let ((n 0))
        (cl-dolist (node nodes)
          (cl-incf n)
          (lister-mark-unmark-at ewoc node nil)
          (lister-refresh-at ewoc node))
        (message "Redisplayed %d nodes" n)))))

;; Insert tagged subset of all nodes

(defun delve--key--insert-tagged (tags &optional prefix)
  "Insert all zettel matching TAGS.
With PREFIX, let the user select the target collection, else
insert into current buffer."
  (interactive (list (completing-read-multiple " Insert all zettels matching tag(s): " (delve-query-tags))
                     current-prefix-arg))
  (if-let* ((matching-string (delve--string-join tags " and " "#"))
            (buf             (if prefix
                                 (delve--select-collection-buffer "Select target collection")
                               (current-buffer)))
            (zettels (-map #'delve--zettel-create (delve-query-nodes-by-tags tags))))
      (delve-insert-items buf zettels)
    (message "No zettels found matching %s" matching-string)))

;; Collect marked items

(defun delve--key--collect-into-buffer (ewoc &optional move)
  "In Delve EWOC, copy all marked items into a (new) collection.
If region is active, also collect the items in that region.  If
MOVE is non-nil, also delete the items in the source buffer,
effectively moving the marked items.  Switch to the target
collection."
  (interactive (list lister-local-ewoc current-prefix-arg))
  (delve--maybe-mark-region ewoc)
  (unless (lister-items-marked-p ewoc)
    (user-error "No items marked"))
  (let (acc)
    (lister-walk-marked-nodes ewoc
                              (lambda (_ node)
                                (push (lister-node-get-data node) acc)))
    (let* ((counted-items (format "%d items" (length acc)))
           (prompt        (format "%s %s to collection: "
                                  (if move "Move" "Add")
                                  counted-items))
           (target      (delve--add-to-buffer (nreverse acc) prompt)))
      ;; now we can safely delete the source nodes
      (when move
        (lister-delete-marked-list ewoc))
      (switch-to-buffer target)
      (message (format "%s %s to this buffer"
                       (if move "Moved" "Added")
                       counted-items)))))

;; Insert node(s)

(defun delve--key--insert-node ()
  "Interactively add node(s) to current buffer's Delve list."
  (interactive)
  (let ((nodes (delve--select-nodes #'delve-query-node-list "Insert node:")))
    (delve-insert-nodes (current-buffer) nodes)
    (message "Inserted %d nodes" (length nodes))))

(defun delve--key--insert-node-by-tags ()
  "Insert nodes matching user selected tags."
  (interactive)
  (let* ((tags (completing-read-multiple " Limit to nodes matching tags:"
                                         (delve-query-tags)))
         (nodes (delve--select-nodes (delve-query-nodes-by-tags tags) "Insert nodes:")))
    (delve-insert-nodes (current-buffer) nodes)
    (message "Inserted %d nodes" (length nodes))))

;; Collect items into a pile

(defun delve--move-into-pile-at (ewoc pos)
  "In EWOC, move all marked nodes into the pile at POS.
Delete all copied nodes.  Throw an error if there is no pile at
POS, or if PILE is marked."
  (let ((pile (lister-get-data-at ewoc pos)))
    (unless (delve--type-p pile 'delve--pile)
      (error "Item at position is not a pile"))
    ;; push:
    (lister-walk-marked-nodes ewoc
                              (lambda (ewoc node)
                                (let ((item (lister-node-get-data node)))
                                  (when (delve--type-p item 'delve--zettel)
                                    (push item (delve--pile-zettels pile))
                                    (lister-delete-at ewoc node)))))
    ;; uniqify the pile zettels:
    (setf (delve--pile-zettels pile)
          (seq-uniq (delve--pile-zettels pile)))
    (lister-refresh-at ewoc pos)))

(defun delve--key--collect-into-pile (ewoc)
  "In EWOC, collect all marked Zettel items in a new pile and insert it.
If region is active, mark all items in the region.  If point is
on a pile item, add marked items to this pile instead.  Remove
any duplicates in the final pile.  Skip non-zettel items when
collecting."
  (interactive (list lister-local-ewoc))
  (delve--maybe-mark-region ewoc)
  (unless (lister-items-marked-p ewoc)
    (user-error "No items marked"))
  ;; Quit if there are piles marked:
  (lister-walk-marked-nodes ewoc
                            (lambda (_ node)
                              (when (delve--type-p (lister-node-get-data node)
                                                   'delve--pile)
                                (user-error "Cannot move pile in itself"))))
  ;; If there is no pile at point, create one:
  (unless (delve--type-p (lister-get-data-at ewoc :point) 'delve--pile)
    (lister-insert-at ewoc :point
                      (delve--pile-create
                       :name (read-string "Name for the new pile: "))))
  ;; now stuff marked items into this pile at point:
  (delve--move-into-pile-at ewoc :point)
  ;; FIXME Once "lister-walk-marked" returns a count, use this to give
  ;; feedback how many items have been moved
  (lister-refresh-at ewoc :point))

;; Delete Items

(defun delve--key--multi-delete ()
  "Delete all marked items or the single item at point.
If a region is active, additionally delete all the items in the
region.  Only delete the item at point if no region is active and
no items are marked.  If any of the deleted items has a sublist,
decrease the indentation of its subitems."
  (interactive)
  (let ((ewoc lister-local-ewoc))
    (delve--maybe-mark-region ewoc)
    ;; If there are no marked items, mark the item at point:
    (unless (lister-items-marked-p ewoc)
      (lister-mark-unmark-at ewoc :point t))
    ;; Ask the user:
    (if (not (y-or-n-p (format "Delete %s item(s)? " (lister-count-marked-items ewoc))))
        (user-error "Canceled")
      ;; Decrease any sublists:
      (lister-walk-marked-nodes ewoc
                                (lambda (ewoc node)
                                  (lister-with-sublist-below ewoc node beg end
                                    (lister-walk-nodes ewoc #'lister-move-item-left beg end))))
      ;; Actually delete the items:
      (lister-delete-marked-list ewoc))))

;; * Storing and reading buffer lists in a file

(defun delve--maybe-refresh-header (sym new op buf)
  "Watch function for refreshing the header if the value has changed.
For the meaning of SYM, NEW, OP and BUF, see the info page for
`add-variable-watcher'."
  (when (and (eq op 'set)
             (not (eq (symbol-value sym) new))
             buf)
    (set sym new)
    (lister-refresh-header-footer (lister-get-ewoc buf))))

(defun delve--ask-storage-file-name (&optional existing-only)
  "Ask for a file name for a Delve store.
Limit selection to only existing files if EXISTING-ONLY is
non-nil.  Offer completion of files in the directory
`delve-store-directory'."
  (let* ((default-dir (concat (file-name-as-directory delve-store-directory)))
         (file-name (read-file-name "File name for a Delve store: " default-dir
                                    nil existing-only)))
    (cond
     ((file-directory-p file-name)
      (user-error "File must not be a directory"))
     ((and (not existing-only) (file-exists-p file-name))
      (if (y-or-n-p "File exists, overwrite it? ")
          file-name
        (user-error "Canceled")))
     (t file-name))))

(defun delve--setup-file-buffer (buf file-name)
  "Set up Delve buffer BUF as a buffer visiting FILE-NAME.
Return BUF."
  (with-current-buffer buf
    (setq-local lister-local-modified nil)
    (setq-local delve-local-storage-file file-name)
    (add-variable-watcher 'lister-local-modified #'delve--maybe-refresh-header)
    (add-variable-watcher 'delve-local-storage-file #'delve--maybe-refresh-header)
    ;; this will be called twice in some cases; don't care.
    (lister-refresh-header-footer lister-local-ewoc))
  buf)

;;; TODO Write test
(defun delve--do-save-buffer (buf file-name)
  "Store the Delve list of BUF in FILE-NAME."
  ;; store list:
  (let ((l (lister-map (lister-get-ewoc buf)  #'delve-store--tokenize-object)))
    (unless (file-exists-p file-name)
      (make-empty-file file-name t))
    (delve-store--write file-name l)
    ;; link buffer to file:
    (delve--setup-file-buffer buf file-name)))

;;; TODO Write test
(defun delve--read-storage-file (file-name)
  "Return a new Delve buffer read from FILE-NAME."
  (unless (file-exists-p file-name)
    (error "File not found %s" file-name))
  (let* ((l          (delve-store--read file-name))
         (delve-list (with-temp-message "Creating data objects..."
                       (delve-store--create-object-list l)))
         (buf-name   (format "Zettel imported from '%s'" (file-name-nondirectory file-name))))
    (delve--setup-file-buffer (delve--new-buffer buf-name delve-list) file-name)))

(defun delve-save-buffer (buf &optional file-name)
  "Store BUF in its existing storage file or create a new one.
If FILE-NAME is not set, use the file name BUF is linked to.  If
BUF is not yet visiting any file, ask the user."
  (interactive (list (current-buffer)))
  (delve--assert-buf buf "Buffer to save must be in Delve mode")
  (let ((name  (or file-name
                   (buffer-local-value 'delve-local-storage-file buf)
                   (delve--ask-storage-file-name))))
    (delve--do-save-buffer buf name))
  (with-current-buffer buf
    (message "Collection stored in file %s" delve-local-storage-file)))

(defun delve-write-buffer (buf file-name)
  "Store BUF in FILE-NAME and associate it."
  (interactive (list (current-buffer) (delve--ask-storage-file-name)))
  (delve-save-buffer buf file-name))

(defun delve-open-storage-file ()
  "Open an existing storage file.
If the user selects a non-storage file, pass to `find-file'."
  (interactive)
  (let* ((default-dir (expand-file-name (file-name-as-directory delve-store-directory)))
         (file-name   (expand-file-name (read-file-name "Open Delve store or other file: " default-dir))))
    (if (delve--storage-p file-name)
        (switch-to-buffer (delve--read-storage-file file-name))
      ;; TODO Replace that test with testing the file suffix
      (if (string= (file-name-directory file-name) default-dir)
          (switch-to-buffer (delve--new-buffer (file-name-base file-name)))
        (find-file file-name)))))

;;; * Delve Major Mode

;; * Delve Keymap
(defvar delve-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Buffer as a whole:
    (define-key map (kbd "q")                        #'bury-buffer)
    (define-key map [remap save-buffer]              #'delve-save-buffer)
    (define-key map [remap write-file]               #'delve-write-buffer)
    (define-key map [remap find-file]                #'delve-open-storage-file)
    (define-key map (kbd "g")                        #'delve--key--sync)
    ;; Any item:
    (define-key map (kbd "<delete>")                 #'delve--key--multi-delete)
    (define-key map [remap yank]                     #'delve--yank)
    ;; Insert node(s):
    (let ((prefix (define-prefix-command 'delve--key--insert-prefix nil "Insert")))
      (define-key prefix "n" '(" node"     . delve--key--insert-node))
      (define-key prefix "t" '(" by tag"   . delve--key--insert-node-by-tags))
      (define-key prefix "b" '(" backlink" . delve--key--insert-backlink))
      (define-key prefix "f" '(" fromlink" . delve--key--insert-fromlink))
      (define-key map (kbd "n") prefix))
    ;; Insert other stuff:
    (define-key map (kbd "h")                        #'delve--key--insert-heading)
    ;; Work with marks:
    (define-key map (kbd "c")                        #'delve--key--collect-into-buffer)
    (define-key map (kbd "p")                        #'delve--key--collect-into-pile)
    ;; Work with the Zettel at point:
    (define-key map [remap org-roam-buffer-toggle]   #'delve--key--roam)
    (define-key map (kbd "o")                        #'delve--key--open-zettel)
    (define-key map (kbd "<C-return>")               #'delve--key--open-zettel)
    (define-key map (kbd "f")                        #'delve--key--fromlinks)
    (define-key map (kbd "<C-right>")                #'delve--key--fromlinks)
    (define-key map (kbd "b")                        #'delve--key--backlinks)
    (define-key map (kbd "<C-left>")                 #'delve--key--backlinks)
    (define-key map (kbd "<RET>")                    #'delve--key--toggle-preview)
    (define-key map [remap lister-mode-cycle-sublist]  #'delve--key--tab)
    ;; Insert Queries or Piles:
    (define-key map (kbd "i")                        #'delve--key--insert-query-or-pile)
    (define-key map (kbd "t")                        #'delve--key--insert-tagged)
    ;; Sorting / Reordering:
    (define-key map (kbd "s")                        #'delve--key--sort)
    ;; Remote Editing:
    (define-key map (kbd "+")                        #'delve--key--add-tags)
    (define-key map (kbd "-")                        #'delve--key--remove-tags)
    map)
  "Key map for `delve-mode'.")

(define-derived-mode delve-mode
  fundamental-mode "Delve"
  "Major mode for browsing your org roam zettelkasten."
  (lister-setup	(current-buffer) #'delve-mapper  #'delve--header-function)
  (add-to-invisibility-spec '(org-link))
  (lister-mode)
  (setq-local lister-mark-face-or-property 'delve-mark-face)
  (setq-local filter-buffer-substring-function #'delve--tokenize-filter))

;;; * Main Entry Point

(defun delve (&optional jump-to-last-buffer)
  "Visit a Delve collection.
With prefix argument JUMP-TO-LAST-BUFFER, directly switch to the
last selected buffer."
  (interactive "P")
  (let (buf)
    (when (and jump-to-last-buffer
               delve--last-selected-buffer
               (buffer-live-p delve--last-selected-buffer))
      (setq buf delve--last-selected-buffer))
    (switch-to-buffer
     (or buf (delve--select-collection-buffer "Visit collection: ")))))

(provide 'delve)
;;; delve.el ends here

;; Local Variables:
;; eval: (add-to-list 'load-path (file-name-directory (buffer-file-name)))
;; End:
