;;; delve.el --- Delve into the depths of your org roam zettelkasten       -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021

;; Author:  <joerg@joergvolbers.de>
;; Version: 0.9

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
(require 'delve-data-types)
(require 'delve-query)
(require 'delve-pp)
(require 'delve-store)
(require 'delve-edit)

;; * Silence Byte Compiler

(declare-function all-the-icons-faicon "all-the-icons" (string) t)
(declare-function consult-completing-read-multiple "consult")
(defvar org-roam-node-read--cached-display-format)

;;; * Customizable Variables

(defgroup Delve nil
  "A zettelkasten tool on top of Org Roam."
  :group 'org-roam
  :link '(url-link :tag "Github" "https://github.com/publicimageltd/delve"))

(defcustom delve-dashboard-tags '("Dashboard")
  "Tags for which to insert query objects in the Dashboard.
Each element can be a tag or a list of tags."
  :group 'Delve
  :type '(repeat (choice (string :tag "Tag")
                         (repeat (string :tag "Tags")))))

(defcustom delve-store-directory (concat (file-name-directory user-emacs-directory)
                                         "delve-store")
  "Path to a default directory for storing Delve buffers in."
  :group 'Delve
  :type  'directory)

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

(defgroup Delve-faces nil
  "Faces used by Delve."
  :group 'Delve
  :group 'faces)

(defface delve-header-face
  '((t (:inherit org-document-title)))
  "Face for displaying the header of a Delve list."
  :group 'Delve-faces)

(defface delve-note-face
  '((t (:inherit font-lock-comment-face)))
  "Face for displaying note items in a Delve list."
  :group 'Delve-faces)

(defface delve-info-face
  '((t (:inherit warning)))
  "Face for displaying info items in a Delve list."
  :group 'Delve-faces)

(defface delve-tags-face
  '((t (:inherit org-tag)))
  "Face for displaying roam tags in a Delve list."
  :group 'Delve-faces)

(defface delve-title-face
  '((t (:inherit org-roam-title)))
  "Face for displaying org roam page titles in a Delve list."
  :group 'Delve-faces)

(defface delve-pile-name-face
  '((t (:inherit org-document-title)))
  "Face for displaying the name of a Zettel pile."
  :group 'Delve-faces)

(defface delve-subtype-face
  '((t (:inherit font-lock-constant-face)))
  "Face for displaying the subtype of a Delve item."
  :group 'Delve-faces)

(defface delve-mtime-face
  '((t (:inherit org-document-info-keyword)))
  "Face for displaying the mtime of a Delve item."
  :group 'Delve-faces)

(defface delve-atime-face
  '((t (:inherit org-document-info-keyword)))
  "Face for displaying the atime of a Delve item."
  :group 'Delve-faces)

(defface delve-ctime-face
  '((t (:inherit org-document-info-keyword)))
  "Face for displaying the ctime of a Delve item."
  :group 'Delve-faces)

(defface delve-nbacklinks-face
  '((t (:weight bold)))
  "Face for displaying the number of backlinks to a Delve zettel."
  :group 'Delve-faces)

(defface delve-ntolinks-face
  '((t (:weight bold)))
  "Face for displaying the number of tolinks to a Delve zettel."
  :group 'Delve-faces)

(defface delve-pile-face
  '((t (:inherit org-level-1)))
  "Face for displaying the title of a Delve pile."
  :group 'Delve-faces)

(defface delve-query-face
  '((t (:inherit org-level-2)))
  "Face for displaying the title of a Delve query."
  :group 'Delve-faces)

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
                ('delve--note    (list "NOTE"  "pencil"))
                ('delve--zettel
                 (if (eq 0 (delve--zettel-level delve-item))
                     (list "FILE" "file-text-o")
                   (list "NODE" "dot-circle-o")))
                (_              (list "TYPE?" "question")))))
    (if (and (featurep 'all-the-icons)
             (not delve--no-icons))
        (concat (all-the-icons-faicon icon-name) " ")
      (delve-pp--set-width s 6))))

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

(defun delve--insert-by-id (id)
  "In current Delve buffer, insert Zettel by ID after point."
  (if-let ((node (delve-query-node-by-id id)))
      (lister-insert-at lister-local-ewoc :point
                        (delve--zettel-create node)
                        nil t)
    (user-error "No node with this ID found")))

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
                   (delve--insert-by-id id)
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
        (cl-dolist (link links)
          (delve--buttonize-link link)))
      (buffer-string))))

(defun delve--zettel-strings (zettel)
  "Return a list of strings representing ZETTEL."
  (let ((node (delve--zettel-node zettel)))
    (list
     ;; -- First line:
     (concat
      ;; Maybe mark out of sync:
      (when (delve--zettel-out-of-sync zettel)
        (delve-pp--add-face "* " 'warning))
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

;; The actual mapper

(defun delve-mapper (item)
  "Transform ITEM into a list of printable strings."
  (let* ((typestring  (delve--type-as-string item))
         (datastrings (cl-typecase item
                        (delve--zettel  (delve--zettel-strings item))
                        (delve--pile    (delve--pile-strings item))
                        (delve--info    (delve--info-strings item))
                        (delve--note    (delve--note-strings item))
                        (delve--query   (delve--query-strings item))
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
to the end, in parentheses."
  (--group-by (format "%s (%s)" (funcall key-fn it) suffix)
              cand))

(defun delve--select-collection-buffer (prompt)
  "Select Delve buffer, collection, or create a new buffer.
Use PROMPT as a prompt to prompt the user to choose promptly."
  (let* ((buffer-suffix     "Existing buffer")
         (collection-suffix "Open file")
         (dashboard-suffix  "Create")
         (alist (append  (delve--prepare-candidates
                          (delve-buffer-list) #'buffer-name buffer-suffix)
                         ;; Dashboard, if not yet open:
                         (unless (delve--dashboard-buf)
                           (delve--prepare-candidates
                            '("Dashboard") #'identity dashboard-suffix))
                         ;; Storage files:
                         (delve--prepare-candidates
                          (delve-unopened-storages) #'identity collection-suffix)))
         (new-name  (completing-read prompt alist nil nil nil 'delve--select-history)))
    ;;
    (setq delve--last-selected-buffer
          (if-let ((result (car (alist-get new-name alist nil nil #'string=))))
              (pcase new-name
                ;; We could also extract the string and then compare,
                ;; but I had always wanted to use the rx matcher!
                ((rx "(" (literal buffer-suffix) ")" string-end)
                 result)
                ((rx "(" (literal collection-suffix) ")" string-end)
                 (delve--read-storage-file result))
                ((rx "(" (literal dashboard-suffix) ")" string-end)
                 (delve--new-dashboard))
                (_                   (error "Something went wrong")))
            (delve--new-buffer new-name)))))

(defun delve--add-to-buffer (l prompt)
  "Add L to a Delve buffer and return that buffer object.
Use PROMPT when asking the user to select or create a buffer."
  (let ((buf (delve--select-collection-buffer prompt)))
    (lister-add-list (with-current-buffer buf lister-local-ewoc)
                     l)
    buf))

(defun delve-kill-all-delve-buffers ()
  "Kill all Delve buffers."
  (interactive)
  (seq-do #'kill-buffer (delve-buffer-list)))

;;; * Remote Editing - Background Utilites

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
TYPES is a type symbol or a list of type symbols.  If the item is
not of type TYPES, throw an error.  Use the position at point in
EWOC or POS, if supplied.  Skip any typechecking if TYPES is nil."
  (let ((ewoc (or ewoc lister-local-ewoc)))
    (unless ewoc
      (error "Command must be called in a Delve buffer"))
    (let ((item (lister-get-data-at ewoc (or pos :point))))
      (if (or (not types)
              (apply #'delve--type-p item (-list types)))
          item
        (error "The item at point is not of the right type for that command")))))

;; TODO Refactor: delve--current-item nearly does the same
(defun delve--current-item-or-marked (&optional types)
  "Get either all marked items or the item at point, marking it.
Always return a list.  TYPES is a type symbol or a list of type
symbols.  If on of the items is not of type TYPES, throw an
error."
  (let ((ewoc lister-local-ewoc))
    (unless ewoc
      (error "Command must be called in a Delve buffer"))
    (delve--maybe-mark-region ewoc)
    (unless (lister-items-marked-p ewoc)
      (lister-mark-unmark-at ewoc :point t))
    (let ((items (lister-get-marked-list ewoc)))
      (when types
        (cl-dolist (item items)
          (unless (apply #'delve--type-p item (-list types))
            (error "The item at point is not of the right type for that command"))))
      items)))

(defun delve--insert-or-open-zettels (zettels &optional prefix as-sublist)
  "Insert ZETTELS in the current Delve buffer, at point.
If AS-SUBLIST is non-nil, insert as a sublist below point.  If
called with PREFIX, insert ZETTEL in a new Delve buffer instead.
Do nothing and return nil if ZETTELS is empty, else print a
message on how many zettels have been inserted and return
non-nil."
  (when zettels
    (if prefix
        (switch-to-buffer (delve--add-to-buffer zettels " Insert zettels in buffer or collection: "))
      ;; TODO Warn when list is too big
      (let* ((n    (length zettels))
             (msg  (format "Inserting %d zettels..." n)))
        (with-temp-message msg
          (if as-sublist
              (lister-insert-sublist-below lister-local-ewoc
                                           :point
                                           zettels)
            (lister-insert-list-at lister-local-ewoc
                                   :point
                                   zettels
                                   nil
                                   (lister-eolp))))
        (message (concat msg "done"))))
      t))

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

;; Insert backlinks and fromlinks

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

(defun delve--key--backlinks (zettel &optional prefix)
  "Insert all backlinks to ZETTEL.
With PREFIX, open link list in a new buffer, else insert it as a
sublist below point."
  (interactive (list (delve--current-item 'delve--zettel) current-prefix-arg))
  (let* ((nodes   (delve-query-backlinks-by-id (delve--zettel-id zettel)))
         (stub    (delve--zettel-info-stub delve--backlink-info zettel))
         (zettels (delve--verzetteln nodes stub)))
    (or (delve--insert-or-open-zettels zettels prefix :as-sublist)
        (message "No backlinks"))))

(defun delve--key--fromlinks (zettel &optional prefix)
  "Insert fromlinks of current ZETTEL.
With PREFIX, open link list in a new buffer, else insert it as a
sublist below point."
  (interactive (list (delve--current-item 'delve--zettel) current-prefix-arg))
  (let* ((nodes   (delve-query-fromlinks-by-id (delve--zettel-id zettel)))
         (stub    (delve--zettel-info-stub delve--fromlink-info zettel))
         (zettels (delve--verzetteln nodes stub)))
    (or (delve--insert-or-open-zettels zettels prefix :as-sublist)
        (message "No tolinks to this zettel node"))))

(defun delve--key--links (zettel &optional prefix)
  "Insert all backlinks and fromlinks from ZETTEL.
With PREFIX, open link list in a new buffer, else insert it as a
sublist below point."
  (interactive (list (delve--current-item 'delve--zettel) current-prefix-arg))
  (let* ((id       (delve--zettel-id zettel))
         (b-zettel (-some--> (delve-query-backlinks-by-id id)
                     (delve--verzetteln it (delve--zettel-info-stub delve--backlink-info zettel))))
         (f-zettel (-some--> (delve-query-fromlinks-by-id id)
                     (delve--verzetteln it (delve--zettel-info-stub delve--fromlink-info zettel)))))
    (or (delve--insert-or-open-zettels (append b-zettel f-zettel) prefix :as-sublist)
        (message "This zettel has no backlinks or fromlinks (you should change that)"))))

(defun delve--key--insert-backlink (zettel)
  "Interactively insert backlinks from current ZETTEL."
  (interactive (list (delve--current-item 'delve--zettel)))
  (if-let* ((all-nodes  (delve-query-backlinks-by-id (delve--zettel-id zettel)))
            (nodes      (delve--select-nodes all-nodes "Insert backlinks:"))
            (zettels    (delve--verzetteln nodes (delve--zettel-info-stub delve--backlink-info zettel))))
      (lister-insert-sublist-below lister-local-ewoc :point zettels))
  (user-error "No backlinks to insert"))

(defun delve--key--insert-fromlink (zettel)
  "Interactively insert fromlinks from current ZETTEL."
  (interactive (list (delve--current-item 'delve--zettel)))
  (if-let* ((all-nodes  (delve-query-backlinks-by-id (delve--zettel-id zettel)))
            (nodes      (delve--select-nodes all-nodes "Insert fromlinks:"))
            (zettels    (delve--verzetteln nodes (delve--zettel-info-stub delve--fromlink-info zettel))))
      (lister-insert-sublist-below lister-local-ewoc :point zettels))
  (user-error "No fromlinks to insert"))

;;

(defun delve--key--insert-query-or-pile (item &optional prefix)
  "Insert results from ITEM, either a query or a pile object.
With PREFIX, insert results in a new buffer, else insert it as a
sublist below point."
  (interactive (list (delve--current-item '(delve--query delve--pile))
                     current-prefix-arg))
  (let ((pile-node (lister-get-node-at lister-local-ewoc :point))
        zettels insertion-type)
    (cl-typecase item
      (delve--pile  (setq zettels (delve--pile-zettels item)
                          insertion-type nil))
      (delve--query (setq zettels (-map #'delve--zettel-create (funcall (delve--query-fn item)))
                          insertion-type :as-sublist)))
    (if (null zettels)
        (message "No matching zettels found")
      (delve--insert-or-open-zettels zettels prefix insertion-type)
      (unless prefix
        (if (delve--type-p item 'delve--pile)
            (lister-delete-at lister-local-ewoc pile-node))))))

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
With PREFIX, open the ZETTEL in a buffer."
  (interactive (list (delve--current-item 'delve--zettel)
                     current-prefix-arg))
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
        (delve--query  (delve--key--insert-query-or-pile item prefix))
        (delve--pile   (delve--key--insert-query-or-pile item prefix))
        (delve--zettel (delve--key--links                item prefix))
        (t             (user-error "Cannot do anything useful here"))))))

;;; * Key commands not bound to a specific item at point

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
  "Insert zettel matching TAGS.
With PREFIX, open search results in a new buffer."
  (interactive (list (completing-read-multiple " Insert zettels matching tag(s): "
                                               (delve-query-tags))
                     current-prefix-arg))
  (let* ((matching-string (delve--string-join tags " and " "#"))
         (zettels         (-map #'delve--zettel-create (delve-query-nodes-by-tags tags))))
    (if zettels
        (delve--insert-or-open-zettels zettels
                                       (format "Nodes matching %s" matching-string)
                                       prefix)
      (message "No zettels found matching %s" matching-string))))

;; Collect marked items

(defun delve--key--collect-into-buffer (ewoc &optional move)
  "In Delve EWOC, collect all marked items in a new buffer.
If region is active, mark items in that region.  Switch to the
new buffer.  If MOVE is non-nil, also delete the items in the
source buffer, effectively moving the marked items into the new
buffer."
  (interactive (list lister-local-ewoc current-prefix-arg))
  (delve--maybe-mark-region ewoc)
  (unless (lister-items-marked-p ewoc)
    (user-error "No items marked"))
  ;; Collect:
  (let (acc n)
    (lister-walk-marked-nodes ewoc
                              (lambda (_ node)
                                (push (lister-node-get-data node) acc)))
    (setq n (length acc))
    (when move
      (lister-delete-marked-list ewoc))
    (switch-to-buffer
     (delve--add-to-buffer (nreverse acc)
                           (format "%s items to buffer: "
                                   (if move "Move" "Add"))))
    (message "%s %d items to this buffer"
             (if move "Moved" "Added")
             n)))

;; Insert node(s)

(defun delve--insert-nodes (ewoc nodes &optional always-insert-after)
  "Insert NODES as zettel at point in EWOC.
Insert the new items before node at point, if any.  Force
insertion after current node if ALWAYS-INSERT-AFTER is non-nil."
  (let ((nodes (-map #'delve--zettel-create nodes)))
    (if (lister-empty-p ewoc)
        (lister-add ewoc nodes)
      (let ((item (lister-get-data-at ewoc :point)))
        (lister-insert-list-at ewoc :point nodes
                               ;; don't indent if current item is not
                               ;; a zettel
                               (and (not (eq (type-of item) 'delve--zettel)) 0)
                               (or always-insert-after (lister-eolp)))))))

(defun delve--key--insert-node ()
  "Interactively add node(s) to current buffer's Delve list."
  (interactive)
  (delve--insert-nodes lister-local-ewoc
                       (delve--select-nodes #'delve-query-node-list "Insert node:")))

(defun delve--key--insert-node-by-tags ()
  "Insert nodes matching user selected tags."
  (interactive)
  (let* ((tags (completing-read-multiple " Limit to nodes matching tags:"
                                         (delve-query-tags)))
         (nodes (delve--select-nodes (delve-query-nodes-by-tags tags) "Insert nodes:")))
    (delve--insert-nodes lister-local-ewoc nodes)))

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
  "Delete all marked items or the single iten at point.
If a region is active, first mark all the items in the region.
Delete all marked items, if there are any.  Only delete the item
at point if there are no marked items.  If any of the items has a
sublist, also decrease the indentation of these subitems."
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


;; The multi key dispatcher

(defun delve--key--dispatch (item &optional prefix)
  "Call key command according to the type of ITEM.
Also pass PREFIX to the corresponding function."
  (interactive (list (delve--current-item)
                     current-prefix-arg))
  (cl-typecase item
    (delve--zettel (delve--key--toggle-preview item prefix))
    ;; That's not useful on RET if we have the TAB key:
;;    (delve--query  (delve--key--insert-query-or-pile item prefix))
;;    (delve--pile   (delve--key--insert-query-or-pile item prefix))
    (t (user-error "No operation defined for items of that type"))))

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
  (unless (eq 'delve-mode (buffer-local-value 'major-mode buf))
    (error "Buffer must be in Delve mode"))
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

(defun delve-open-buffer ()
  "Open an existing storage file.
If the user selects a non-storage file, pass to `find-file'."
  (interactive)
  (let* ((default-dir (expand-file-name (file-name-as-directory delve-store-directory)))
         (file-name   (expand-file-name (read-file-name "Open Delve store or other file: " default-dir))))
    (if (-contains-p (delve--storage-files :full-path) file-name)
        (switch-to-buffer (delve--read-storage-file file-name))
      ;; TODO Replace that test with testing the file suffix
      (if (string= (file-name-directory file-name)
                   default-dir)
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
    (define-key map [remap find-file]                #'delve-open-buffer)
    (define-key map (kbd "g")                        #'delve--key--sync)
    ;; Any item:
    (define-key map (kbd "<delete>")                 #'delve--key--multi-delete)
    ;; Insert node(s):
    (let ((prefix (define-prefix-command 'delve--key--insert-prefix nil "Insert")))
      (define-key prefix "n" '(" node"     . delve--key--insert-node))
      (define-key prefix "t" '(" by tag"   . delve--key--insert-node-by-tags))
      (define-key prefix "b" '(" backlink" . delve--key--insert-backlink))
      (define-key prefix "f" '(" fromlink" . delve--key--insert-fromlink))
      (define-key map (kbd "n") prefix))
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
    (define-key map (kbd "<RET>")                    #'delve--key--dispatch)
    (define-key map [remap lister-mode-cycle-sublist]  #'delve--key--tab)
    ;; Insert Queries or Piles:
    (define-key map (kbd "i")                        #'delve--key--insert-query-or-pile)
    (define-key map (kbd "t")                        #'delve--key--insert-tagged)
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
  (lister-mode))

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
