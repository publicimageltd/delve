;;; delve.el --- Delve into the depths of your Org Roam zettelkasten       -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023

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
(require 'bookmark)
(require 'cursor-sensor) ;; see `delve--get-hl-line-range'
(require 'face-remap)    ;; see `delve--prepare-preview'

(require 'delve-transient)
(require 'delve-data-types)
(require 'delve-query)
(require 'delve-pp)
(require 'delve-store)
(require 'delve-edit)
(require 'delve-export)
(require 'delve-crm)

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

(defcustom delve-dashboard-queries (list #'delve--create-todo-query
                                         #'delve--create-unlinked-query
                                         #'delve--create-last-modified-query)
  "Additional non-tag queries for the initial Dashboard.
A list of functions which each returns a `delve--query'
struct (which see). If nil, do not add any non-tag queries to the
Dashboard."
  :group 'delve
  :type '(choice
          (const :tag "No additional queries" nil)
          (repeat function)))

(defcustom delve-storage-paths (concat (file-name-directory user-emacs-directory)
                                         "delve-store")
  "Paths to for default directories to store Delve buffers in.
When the user is prompted for reading a storage, all Delve stores
found here will be offered as default choices.  It is, however,
not obligatory to store files in these locations.  Storages
located elsewhere just don't show up in the prompt.

When writing a Delve list to a new storage file, the first (or
only) value of this variable is initially used as the default
directory.

The value of this variable can be either a file path, a list of
file paths, or nil.

Use the file path \".\" to add the current directory to the list
of default storage paths."
  :group 'delve
  :type  '(choice
           (const :tag "No default" nil)
           (directory :tag "Single path (use M-TAB for completion)")
           (repeat :tag "List of paths (use M-TAB for completion)" directory)))

(defcustom delve-storage-suffix ".delve"
  "Suffix for Delve storage files."
  :group 'delve
  :type  'string)

(defcustom delve-display-path t
  "Turn on display of paths before the node's title.
If non-nil, insert the file title and the outline path, if they
exist, before the node's title.  This can cause quite long
entries."
  :group 'delve
  :type 'boolean)

(defcustom delve-last-modified-limit 20
  "Number of items for the Dashboard query \\='last modified\\='."
  :group 'delve
  :type 'integer)

(defcustom delve-compact-view-shows-node-path t
  "If set, also show complete node path in compact view."
  :group 'delve
  :type 'boolean)

;;; * Global Variables

(defvar delve-version "0.9.5"
  "Current version of delve.")

(defvar delve--no-icons nil
  "If bound, do not use any icons when creating output.")

(defvar delve-dashboard-name "Dashboard"
  "Name of the dashboard buffer.")

(defvar delve--select-history nil
  "History for `delve--select-collection-buffer'.")

(defvar delve--last-selected-buffer nil
  "Last buffer selected with `delve--select-collection-buffer'.")

(defvar delve--last-storage-dir nil
  "Directory from last used storage file.")


;; * Buffer Local Variables

(defvar-local delve-local-storage-file nil
  "Associated local storage file.")

(defun delve-get-storage-file (buf)
  "Get the buffer local storage file for BUF."
  (buffer-local-value 'delve-local-storage-file buf))

(defvar-local delve-local-header-info "DELVE"
  "First line of the local Lister header.")

(defvar-local delve-local-compact-view nil
  "Whether to display the nodes in a compact way.")

;; * Faces

(defgroup delve-faces nil
  "Faces used by Delve."
  :group 'Delve
  :group 'faces)

(defface delve-mark-face
  '((t (:inherit highlight)))
  "Face for highlighting manually marked items."
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
  "Face for displaying Org Roam page titles in a Delve list."
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

;;;; -----------------------------------------------------------
;; * Small utilities

;; this is a direct copy from s-capitalize
(defun delve--capitalize (s)
  "Convert S first word's first character to upper and the rest to lower case."
  (declare (side-effect-free t))
  (concat (upcase (substring s 0 1)) (downcase (substring s 1))))

;; * Debugging in the live environment

(defun delve-reload ()
  "Reload Delve."
  (interactive)
  (when (y-or-n-p "This will close all existing Delve buffers. Reload Delve? ")
    (let ((kill-buffer-query-functions nil))
      (-each (delve-buffer-list) #'kill-buffer))
    (when (featurep 'delve-minor-mode)
      (unload-feature 'delve-minor-mode t))
    (unload-feature 'delve t)
    (require 'delve)
    (message "Loaded Delve version %s" delve-version)))

;; * Assertions

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


;;; -----------------------------------------------------------
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

(defun delve--string-join (strings &optional separator prefix)
  "Join all non-nil strings in STRINGS using SEPARATOR.
Optionally add string PREFIX to each non-nil item."
  (let ((strings (flatten-tree strings)))
    (when prefix
      (setq strings (--map (concat prefix it) strings)))
    (string-join strings separator)))

(defun delve--tag-button (tag)
  "Return TAG as a button object."
  (delve--get-button tag
    'action (lambda (_) (message "Do something with %s" tag))))

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
  "In current buffer, replace link LINK-PLIST with a button.
For the format of LINK-PLIST, see `delve--collect-link'."
  (let ((beg    (plist-get link-plist :beg))
        (end    (plist-get link-plist :end))
        (link   (plist-get link-plist :link))
        (label  (plist-get link-plist :label))
        (blanks (plist-get link-plist :blanks))
        (id     (plist-get link-plist :id)))
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
        ;; use org-mode's special face remapping, if it exists:
        (when buffer-face-mode
          (add-face-text-property (point-min) (point-max)
                                  buffer-face-mode-face))
        ;; If buffer has too many lines, inhibit setting the
        ;; `intangible' property so that it can be read properly:
        (when (> (count-lines (point-min) (point-max))
                 (/ (window-body-height) 3))
          (add-text-properties (point-min) (point-max)  '(field t)))
        ;; now buttonize the linke:
        (cl-dolist (link links)
          (delve--buttonize-link link)))
      (buffer-string))))

(defun delve--zettel-strings (zettel)
  "Return a list of strings representing ZETTEL."
  (let ((main-item-list
         ;; Compact View - one line only::
         (if delve-local-compact-view
             (list
              (delve-pp-fields zettel `(;; path:
                                        ((when (and delve-compact-view-shows-node-path
                                                    (> (delve--zettel-level it) 0))
                                           (delve--zettel-file-title it))
                                         :format "%s/"
                                         :add-face delve-path-face)
                                        ;; NOTE Seems unnecessary.
                                        ;; ((when (and delve-compact-view-shows-node-path
                                        ;;             (> (delve--zettel-level it) 0)
                                        ;;             (delve--zettel-olp it))
                                        ;;    (string-join (delve--zettel-olp it) "/"))
                                        ;;  :format "%s/"
                                        ;;  :add-face delve-path-face)
                                        ;; title:
                                        ((or (delve--zettel-title it) "<No title>")
                                         :add-face delve-title-face)
                                        ;; tags:
                                         ((delve--tags-as-string (delve--zettel-node it))
                                         :add-face delve-tags-face))))
           ;; Default view:
           (list
            ;; -- first line:
            (when (and delve-display-path
                       (not (eq 0 (delve--zettel-level zettel))))
              (delve-pp-fields zettel `(;; path:
                                        (,#'delve--zettel-file-title
                                         :format "%s/"
                                         :add-face delve-path-face)
                                        ((when (delve--zettel-olp it)
                                           (string-join (delve--zettel-olp it) "/"))
                                         :format "%s/"
                                         :add-face delve-path-face))))
            ;; -- second line:
            (delve-pp-fields zettel `(;; title
                                      (,#'delve--zettel-title
                                       :add-face delve-title-face))
                             " " "<No title>")
            ;; -- third line:
            (delve-pp-fields zettel `(;; tags
                                      ((delve--tags-as-string (delve--zettel-node it))
                                       :add-face delve-tags-face)
                                      ;; info
                                      ,#'delve--zettel-info)
                             " " nil)))))
    ;; add sync marker to front:
    (when (and (setq main-item-list (cl-remove-if #'null main-item-list))
               (delve--zettel-out-of-sync zettel))
      (setq main-item-list (cons (concat (delve-pp--add-face "* " 'warning)
                                         (car main-item-list))
                                 (cdr main-item-list))))
    ;; Now add everything which is common both to compact and default
    ;; view:
    (list
     main-item-list
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
         (datastrings (or (cl-typecase item
                            (delve--zettel  (delve--zettel-strings item))
                            (delve--pile    (delve--pile-strings item))
                            (delve--query   (delve--query-strings item))
                            ;; always check the basic type "delve--note" last!
                            (delve--heading (delve--heading-strings item))
                            (delve--info    (delve--info-strings item))
                            (delve--note    (delve--note-strings item))
                            (t (list "no printer available for that item type")))
                          (list (format "Error: Mapper for item type %s returned NIL" item)))))
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
         (if delve-local-storage-file
             (concat (when lister-local-modified "* ")
                     (propertize delve-local-storage-file 'face 'font-lock-string-face))
           (propertize "<Collection not saved>" 'face 'warning)))))


;; -----------------------------------------------------------
;; * Storage File Handling

(defun delve--file-as-dir (s)
  "Return S as a directory file name iff it is a string.
Unlike `file-name-as-directory', this function returns nil if S
is nil."
  (when s (file-name-as-directory s)))

(defun delve--set-storage-dir (&optional last-file-name)
  "Make sure that `delve--last-storage-dir' has a value.
Optionally set the value to the directory part of LAST-FILE-NAME."
  (setq delve--last-storage-dir (delve--file-as-dir
                            (if last-file-name
                                (delve--file-as-dir (file-name-directory last-file-name))
                              (or delve--last-storage-dir
                                  (car (-list delve-storage-paths)))))))

(defun delve--fix-suffix (s suffix)
  "Add file SUFFIX to S, maybe removing existing suffixes."
  (let ((ext (file-name-extension s t)))
    (if (eq "" ext)
        (concat s suffix)
      (concat (file-name-sans-extension s) suffix))))

(defun delve--all-files-in-paths (paths &optional suffix)
  "Return all files ending in SUFFIX within list of PATHS.
Returns the full paths (expanded file names)."
  (let* ((the-suffix (or suffix "")))
    ;; delete dups in case there's a "." in the list
    (-uniq (-flatten (--map (directory-files it t (rx (and string-start
                                                           (* (not "."))
                                                           (literal the-suffix) string-end)))
                            paths)))))

(defun delve--all-file-extensions (files)
  ;; DONT TEST, will be removed in 1.1
  "Return an aggregate list counting all extensions in FILES.
The result is an alist with the file extension (without period)
as its key and an integer count as value."
  (--map (cons (car it) (length (cdr it)))
         (-group-by #'file-name-extension files)))

(defun delve-convert-storage-directory (delve-store)
  ;; DONT TEST, will be removed in 1.1
  "Change all files in DELVE-STORE to conform to `delve-storage-suffix'.
Prompt the user before doing any real changes.  If DELVE-STORE is
not provided, also prompt the user for the directory to be
converted."
  (interactive (list (read-directory-name " Convert all files in this directory:"
                                          (concat (file-name-directory user-emacs-directory) "delve-store"))))
  (let* ((files (delve--all-files-in-paths (list delve-store) ""))
         (ext-list (delve--all-file-extensions files))
         ;; That's way too explicit for this one-time function, but
         ;; hey, it's so easy in lisp:
         (ext-string (string-join (--map (let ((ext (car it))
                                               (n   (cdr it)))
                                           (format "%d %s %s"
                                                   n
                                                   (if (eq 1 n) "file has" "files have")
                                                   (if ext (format "the extension .%s" ext)
                                                     "no extension")))
                                         ext-list)
                                  ", and "))
         (total    (length files)))
    (if (not (y-or-n-p (format "There are total %d files.  %s.  Convert them all to end in %s? "
                               total ext-string delve-storage-suffix)))
        (user-error "Canceled")
      (dolist-with-progress-reporter (file files)
        "Renaming files..."
        (rename-file file (concat (file-name-sans-extension file) delve-storage-suffix) t)))))

(defun delve--storage-files ()
  "Return all available storage files as full path names.
If `delve-storage-paths' only contains one single directory which
does not yet exist, create it."
  (when delve-storage-paths
    ;; If delve-storage-path is a single directory, maybe create it:
    (when (and (eq 'string (type-of delve-storage-paths))
               (not (file-exists-p delve-storage-paths)))
      (if (y-or-n-p (format "Storage directory %s does not exist.  Create it? " delve-storage-paths))
          (make-directory delve-storage-paths t)
        (user-error "Canceled.  To skip this test, call (setq delve-storage-paths '(\"%s\")) in your config file.  " delve-storage-paths)))
    ;; Now collect the files
    (delve--all-files-in-paths (-list delve-storage-paths) delve-storage-suffix)))

(defun delve--storage-p (file-path)
  "Check if FILE-PATH represents an existing Delve storage file.
Just check the existence of the file, don't look at the contents."
  (-contains-p (delve--storage-files)
               (expand-file-name file-path)))

(defun delve--storage-file-name-p (file-name)
  "Check if FILE-NAME is a valid storage file name.
Do not check for existence; just validate the name."
  (string-match-p (rx (literal delve-storage-suffix) string-end) file-name))

;; * Buffer and Dashboard

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
  "Create a Query list item searching for nodes matching TAGS."
  (let* ((tags (-list tags)))
    (delve--query-create :info (format "Query for nodes matching %s"
                                       (delve--string-join tags " and " "#"))
                         :fn (lambda ()
                               (delve-query-nodes-by-tags tags)))))

(defun delve--create-unlinked-query ()
  "Create a Query list item searching for unlinked nodes."
  (delve--query-create :info "Unlinked nodes"
                       :fn #'delve-query-unlinked))

(defun delve--create-last-modified-query ()
  "Create a Query list item searching for the 10 last modified nodes."
  (delve--query-create :info "Last modified nodes"
                       :fn (apply-partially #'delve-query-last-modified
                                            delve-last-modified-limit)))

(defun delve--create-todo-query ()
  "Create a Query list item returning all nodes marked \"TODO\"."
  (delve--query-create :info "Nodes marked 'TODO'"
                       :fn (lambda ()
                             (delve-query-nodes-by-todo "TODO"))))

(defun delve--dashboard-queries ()
  "Return a list of all dashboard Query items."
  (let* ((tag-queries (--map (delve--create-tag-query (-list it)) delve-dashboard-tags))
         (non-tag-queries (-flatten (-map #'funcall delve-dashboard-queries))))
    (append tag-queries non-tag-queries)))

(defun delve--new-dashboard ()
  "Return a new Delve dashboard buffer."
  (with-temp-message "Setting up dashboard..."
    (let ((buf (delve--new-buffer delve-dashboard-name (delve--dashboard-queries))))
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
        (-difference (delve--storage-files)
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
  (replace-regexp-in-string (rx (one-or-more space)
                                "(" (+ (not ")")) ")"
                                string-end)
                            "\\1"
                            cand))

(defun delve--get-collection-buffer (buf-or-name)
  "Get a Delve buffer containing the collection BUF-OR-NAME.
BUF-OR-NAME must be a string or a buffer object.  If it is a
buffer object, return it unchanged.  Else check if the string is
the path to a Delve storage file and load it, or create a new
Delve buffer using the string as its name.  Always return the
newly created buffer object."
  (if (bufferp buf-or-name)
      buf-or-name
    (or (get-buffer buf-or-name)
        (if (delve--storage-p buf-or-name)
            (let ((buf (delve--read-storage-file buf-or-name)))
              (delve--set-storage-dir buf-or-name)
              buf)
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
NODES is a list of Org Roam nodes or a single node.  Move point
to the last item inserted.  For possible values for COLLECTION,
see `delve--get-collection-buffer'.  Return the collection
buffer."
  (delve-insert-items collection (-map #'delve--zettel-create (-list nodes))))

(defun delve-insert-nodes-by-id (collection ids)
  "Insert nodes with IDS in COLLECTION after current item and move point.
IDS can be either a single ID string or a list of IDs.  For
  possible values for COLLECTION, see
  `delve--get-collection-buffer'.  Return the collection buffer."
  (delve-insert-nodes collection (delve-query-nodes-by-id (-list ids))))

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

(cl-defstruct (delve-dual-cmp
               (:constructor delve-dual-cmp--create))
  "Structure holding two comparator functions for sorting in
ascending and descending order, and a description of the sorting
criterion for the user."
  comp-asc comp-desc
  criterion-name ascending-name descending-name)

;; FIXME Since the predicates for sorting are just the reverse from
;;       each other, maybe just use one and construct the other on
;;       on the fly (being nothing but '(not (fn ....)))
(defmacro delve--build-dual-cmp (name criterion-name
                                      sort-fn-asc ascending-name
                                      sort-fn-desc descending-name
                                      slot-fn
                                      &optional map-fn)
  "Define a dual comperator object NAME.
A dual comperator object holds two comparator functions.  NAME is
the name of the variable which holds the comperator object.

CRITERION-NAME is a description of the sorting criterion for the
user (e.g. \"title\"). It will be used for constructing a user
interface where the user can select this criterion.

SORT-FN-ASC and SORT-FN-DESC are predicates used for sorting in
ascending or in descending order, respectively. For ascending
order, the predicate should return non-nil if the first element
is \"less\" than the second, or nil if not; for descending order,
the reverse holds.

ASCENDING-NAME and DESCENDING-NAME give a user readable
description of the sorting result, e.g. \"from A to Z\".

SLOT-FN will be used to access the value, e.g.
`delve--zettel-tags'. Optionally pass the slot value through
MAP-FN before using it for sorting, e.g. `length'."
  (declare (indent 1))
  `(defvar ,name
     (delve-dual-cmp--create :comp-asc (delve--cmp-fn ,sort-fn-asc ,slot-fn ,map-fn)
                             :comp-desc (delve--cmp-fn ,sort-fn-desc ,slot-fn ,map-fn)
                             :criterion-name ,criterion-name
                             :ascending-name ,ascending-name
                             :descending-name ,descending-name)
     ,(concat "Structure holding a dual comporator to sort Zettel structs by " (downcase criterion-name))))

;; * Define the comparators for sorting

(delve--build-dual-cmp delve-2cmp--title
  "title"
  #'string< "from A to Z"
  #'string> "from Z to A"
  #'delve--zettel-title)

(delve--build-dual-cmp delve-2cmp--tag-n
  "number of tags"
  #'< "most tags first"
  #'> "no tags first"
  #'delve--zettel-tags
  #'length)

(delve--build-dual-cmp delve-2cmp--level
  "nesting level"
  #'< "deeply nested first"
  #'> "top nodes first"
  #'delve--zettel-level)

(delve--build-dual-cmp delve-2cmp--file-title
  "file title"
  #'string< "from A to Z"
  #'string> "from Z to A"
  #'delve--zettel-file-title)

(delve--build-dual-cmp delve-2cmp--file-mtime
  "modification time"
  (-compose #'not #'time-less-p) "last modified first"
  #'time-less-p             "last modified last"
  #'delve--zettel-file-mtime)

;; * Utilities to use comparators in transients

(defvar delve--all-2cmps-symbols
  '(delve-2cmp--title
    delve-2cmp--file-title
    delve-2cmp--level
    delve-2cmp--tag-n
    delve-2cmp--file-mtime)
  "*Internal* All comperator names, as symbols.")

(defvar delve--all-2cmps
  (-map #'eval delve--all-2cmps-symbols)
  "*Internal* All comparators for sorting.")

(defun delve--cmp-info (dual-cmp slot-name)
  "Return informative string about the comparator DUAL-CMP.
SLOT-NAME must be either \"comp-asc\" or \"comp-desc\",
designating the respective slot in the dual comperator
object. Return nil if one of the args is nil."
  (when (and dual-cmp slot-name)
    (format "by %s (%s)"
            ;; sorting criterion: by nesting level; by title; etc.
            (delve-dual-cmp-criterion-name dual-cmp)
            ;; comparator description:
            (if (equal slot-name "comp-desc")
                (delve-dual-cmp-descending-name dual-cmp)
              (delve-dual-cmp-ascending-name dual-cmp)))))

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
   (pretty-choices :initform (-map #'delve-dual-cmp-criterion-name delve--all-2cmps)))
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
  "Sort."
  [["First sorting criterion"
    ("s" "Sort by" "--sort1="
     :class delve--transient-cmp-switches
     :allow-nil nil)
    ("o" "Order"   "--order1="
     :class delve--transient-cmp-order
     :allow-nil nil)]]
   [["Second sorting criterion"
     ("S" "Sort by" "--sort2="  :class delve--transient-cmp-switches)
     ("O" "Order" "--order2="   :class delve--transient-cmp-order)]]
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

(defun delve--zettel-short-title (zettel)
  "Return a shortened title of ZETTEL.
Return title unchanged if it has less than 40 characters; else
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

(defun delve--marked-items-p ()
  "Check if current buffer has marked items or active regions."
  (delve--assert-buf nil "Command must be called in a Delve buffer")
  (or (lister-items-marked-p lister-local-ewoc)
      (use-region-p)))

(defun delve--current-item (types &optional ewoc pos)
  "Return item in current buffer if it belongs to TYPES.
Use current buffer unless EWOC and POS is given.  TYPES is a list
of types allowed for, or a single type symbol, or nil if any type
is acceptable.  POS can be any kind of position accepted by
`lister--parse-position' and defaults to `:point'."
  (let ((ewoc (or ewoc lister-local-ewoc)))
    (delve--assert-buf ewoc "Command must be called in a Delve buffer")
    (let ((item (lister-get-data-at ewoc (or pos :point))))
      (when (or (not types)
                (apply #'delve--type-p item (-list types)))
        item))))

(defun delve--current-item-or-error (&optional types ewoc pos)
  "Get the item bound to the current Lister node or throw an error.
Use the current buffer or the buffer of EWOC, if non-nil.  TYPES
is a type symbol or a list of type symbols.  If the item is not
of type TYPES, throw an error.  Use the position at point in EWOC
or POS, if supplied.  Skip any typechecking if TYPES is nil."
  (or (delve--current-item types ewoc pos)
      (error "The item at point is not of the right type for that command")))

;; TODO Refactor: delve--current-item-or-error nearly does the same
(defun delve--current-item-or-marked (&optional types)
  "Get either all marked items or the item at point, marking it.
Use the current buffer.  Always return a list.  TYPES is a type
symbol or a list of type symbols.  If one of the items is not of
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
  (let* ((template       (org-roam-node--process-display-format org-roam-node-display-template))
         (node-alist     (with-temp-message "Collecting nodes..."
                           (--map (org-roam-node-read--to-candidate it template)
                                  (if (listp nodes-or-node-fn)
                                      nodes-or-node-fn
                                    (funcall nodes-or-node-fn)))))
         (prompt         (concat " " (string-trim prompt) " "))
         (node-selected  (if node-alist
                             ;;(completing-read prompt node-alist)
                             (delve-crm-select prompt node-alist)
                           (user-error "No nodes to choose from"))))
    (--map (alist-get it node-alist nil nil #'string=)
           (-list node-selected))))

;;; * Key commands working with the "item at point"

;; Every key command in this subsection should have an optional prefix
;; argument even if it is not used, so that it can be used by
;; delve--key--dispatch.

(defun delve--key--add-tags (zettels &optional prefix)
  "Add tags to all marked ZETTELS or the zettel at point.
Optional argument PREFIX is currently not used."
  (interactive (list (delve--current-item-or-marked 'delve--zettel)))
  (ignore prefix)
  (if (not zettels)
      (user-error "Nothing to tag")
    (delve-edit--prompt-add-tags zettels)
    (delve--taint-and-refresh-marked-nodes lister-local-ewoc)
    (message "Edited %d nodes" (length zettels))))

(defun delve--key--remove-tags (zettels &optional prefix)
  "Remove tags from all marked ZETTELS or the zettel at point.
Optional argument PREFIX is currently not used."
  (interactive (list (delve--current-item-or-marked 'delve--zettel)))
  (ignore prefix)
  (if (not zettels)
      (user-error "Nothing to edit")
    (delve-edit--prompt-remove-tags zettels)
    (delve--taint-and-refresh-marked-nodes lister-local-ewoc)
    (message "Edited %d nodes" (length zettels))))

(defun delve--key--open-zettel (zettel &optional prefix)
  "Open the ZETTEL at point.
Optional argument PREFIX is currently not used."
  (interactive (list (delve--current-item-or-error 'delve--zettel)))
  (ignore prefix)
  (org-roam-node-open (delve--zettel-node zettel)))


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
  "Insert tokenized string S as a Delve object at point.
In Delve buffers, insert at point into the list.  Else insert
using the first export backend in `delve-export--yank-handlers'
for which the assert function returns non-nil.  If no such
handler is found, simply insert S as it is."
  (let* ((backends (delve-export--available-backends delve-export--yank-handlers))
         (objects (delve-store--parse-list (ignore-errors (read (substring-no-properties s))))))
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

(defun delve--key--yank (&optional arg)
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

;; * Insert backlinks and fromlinks in slot `info'

(defvar delve--backlink-format "Backlink to %s"
  "Format for insertion of backlinks in Zettel slot `info'.
\%s will be replaced by a button linking to the Zettel in the
list view.")

(defvar delve--fromlink-format "Link from %s"
  "Format for insertion of fromlinks in Zettel slot `info'.
\%s will be replaced by a button linking to the Zettel in the
list view.")

;; NOTE This could be generalized to set any slots while mapping
(defun delve--nodes-to-zettel (roam-nodes &optional info)
    "Return all Org ROAM-NODES as Delve zettel objects.
Optional fill the info slot of each object with the string INFO."
    (--map (let ((z (delve--zettel-create it)))
             (setf (delve--zettel-info z) info)
             z)
           roam-nodes))

(defun delve--zettel-create-link-info (format-string zettel)
  "Create a string with a button pointing to ZETTEL.
Replace '%s' in FORMAT-STRING with the button.  Use a shortened
representation of the ZETTEL object as a title for the button."
  (format format-string
          (delve--get-button (delve--zettel-short-title zettel)
            'action (lambda (_)
                      (if-let ((node (delve--find-zettel-by-id (delve--zettel-id zettel))))
                          (progn
                            (push-mark)
                            (lister-goto lister-local-ewoc node))
                        (user-error "Zettel not found in this buffer"))))))

(defun delve--key--backlinks (zettel &optional select-target)
  "Insert all backlinks to ZETTEL.
With SELECT-TARGET, select target collection for the link list,
else insert it as a sublist below point."
  (interactive (list (delve--current-item-or-error 'delve--zettel) current-prefix-arg))
  (let* ((nodes   (delve-query-backlinks-by-id (delve--zettel-id zettel)))
         (stub    (delve--zettel-create-link-info delve--backlink-format zettel))
         (zettels (and nodes (delve--nodes-to-zettel nodes stub))))
    (or (delve--insert-or-select zettels select-target)
        (message "No backlinks"))))

(defun delve--key--fromlinks (zettel &optional select-target)
  "Insert fromlinks of current ZETTEL.
With SELECT-TARGET, select target collection for the link list,
else insert it as a sublist below point."
  (interactive (list (delve--current-item-or-error 'delve--zettel) current-prefix-arg))
  (let* ((nodes   (delve-query-fromlinks-by-id (delve--zettel-id zettel)))
         (stub    (delve--zettel-create-link-info delve--fromlink-format zettel))
         (zettels (and nodes (delve--nodes-to-zettel nodes stub))))
    (or (delve--insert-or-select zettels select-target)
        (message "No tolinks to this zettel node"))))

(defun delve--key--links (zettel &optional select-target)
  "Insert all backlinks and fromlinks from ZETTEL.
With SELECT-TARGET, select target collection for the link list,
else insert it as a sublist below point."
  (interactive (list (delve--current-item-or-error 'delve--zettel) current-prefix-arg))
  (let* ((id       (delve--zettel-id zettel))
         (b-zettel (-some--> (delve-query-backlinks-by-id id)
                     (delve--nodes-to-zettel it (delve--zettel-create-link-info delve--backlink-format zettel))))
         (f-zettel (-some--> (delve-query-fromlinks-by-id id)
                     (delve--nodes-to-zettel it (delve--zettel-create-link-info delve--fromlink-format zettel)))))
    (or (delve--insert-or-select (append b-zettel f-zettel) select-target)
        (message "This zettel has no backlinks or fromlinks (you should change that)"))))

(defun delve--key--insert-backlink (zettel &optional select-target)
  "Select and insert backlinks from current ZETTEL.
With SELECT-TARGET, select target collection for the link list,
else insert it as a sublist below point."
  (interactive (list (delve--current-item-or-error 'delve--zettel) current-prefix-arg))
  (let* ((all-nodes  (delve-query-backlinks-by-id (delve--zettel-id zettel)))
         (nodes      (delve--select-nodes all-nodes "Insert backlinks:"))
         (zettels    (delve--nodes-to-zettel nodes (delve--zettel-create-link-info delve--backlink-format zettel))))
    (or (delve--insert-or-select zettels select-target)
        (user-error "No backlinks to insert"))))

(defun delve--key--insert-fromlink (zettel &optional select-target)
  "Select and insert fromlinks from current ZETTEL.
With SELECT-TARGET, select target collection for the link list,
else insert it as a sublist below point."
  (interactive (list (delve--current-item-or-error 'delve--zettel) current-prefix-arg))
  (let* ((all-nodes  (delve-query-backlinks-by-id (delve--zettel-id zettel)))
         (nodes      (delve--select-nodes all-nodes "Insert fromlinks:"))
         (zettels    (delve--nodes-to-zettel nodes (delve--zettel-create-link-info delve--fromlink-format zettel))))
    (or (delve--insert-or-select zettels select-target)
        (user-error "No fromlinks to insert"))))

(defun delve--key--insert-pile (pile &optional select-target)
  "Insert all zettels in PILE, removing it.
With SELECT-TARGET, select target for the results, else insert it
as a sublist below point."
  (interactive (list (delve--current-item-or-error '(delve--pile)) current-prefix-arg))
  (let ((pile-node (lister-get-node-at lister-local-ewoc :point)))
    (delve--insert-or-select (delve--pile-zettels pile) select-target)
    (unless select-target
      (lister-delete-at lister-local-ewoc pile-node))))

(defun delve--key--insert-query (query &optional select-target)
  "Insert results of QUERY.
With SELECT-TARGET, interatively select target for the results,
else insert it as a sublist below point."
  (interactive (list (delve--current-item-or-error '(delve--query)) current-prefix-arg))
  (or (delve--insert-or-select (-map #'delve--zettel-create
                                     (funcall (delve--query-fn query)))
                               select-target)
      (user-error "No matching items found")))

(defun delve--key--insert-query-or-pile (item &optional select-target)
  "Insert results from ITEM, either a query or a pile object.
With SELECT-TARGET, select target for the results, else insert it
as a sublist below point."
  (interactive (list (delve--current-item-or-error '(delve--query delve--pile)) current-prefix-arg))
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
  (interactive (list (delve--current-item-or-error 'delve--zettel) current-prefix-arg))
  (if prefix
      (delve--key--open-zettel zettel)
    (let ((preview (and (not (delve--zettel-preview zettel))
                        (or (delve--get-preview-contents zettel)
                            "No preview available"))))
      (setf (delve--zettel-preview zettel) preview)
      (lister-refresh-at lister-local-ewoc :point))))

(defun delve--key--roam (zettel &optional prefix)
  "Open the Org Roam buffer for ZETTEL.
Optional argument PREFIX is currently not used."
  (interactive (list (delve--current-item-or-error 'delve--zettel)))
  (ignore prefix)
  (org-roam-buffer-display-dedicated (delve--zettel-node zettel)))

(defun delve--key--tab (ewoc &optional prefix)
  "In EWOC, expand item at point or toggle subtree visibility.
With PREFIX, expand all hidden subtrees in the EWOC's buffer."
  (interactive (list lister-local-ewoc current-prefix-arg))
  (if (lister-sublist-below-p ewoc :point)
      (lister-mode-cycle-sublist ewoc :point prefix)
    (let ((item (delve--current-item-or-error)))
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

(defun delve--zettel-equal-id (z1 z2)
  "Check if Zettel Z1 and Z2 share the same Org Roam Node ID."
  (equal (delve--zettel-id z1) (delve--zettel-id z2)))

;; TODO This should actually remove items which
;;      do not belong to the queries anymore
;; REVIEW Problem: This would also delete all manually inserted items
;;  - Solution 1: Mark 'queried' items internally; problem here:
;;                 moving them around would cause great irritation
;;  - Solution 2: Instead of deleting them, mark them for deletion so
;;                that the user can unmark them if they want.
(defun delve--update-queries (ewoc)
  "Update all queries in the current EWOC buffer.
Return the count of the inserted nodes."
  (let ((count 0))
    (lister-dolist (ewoc query :first :last node)
      ;; act on query items with open sublists
      (when (and (delve--query-p query)
                 (lister-sublist-below-p ewoc node))
        ;; Redo the query and insert missing items
        (when-let* ((-compare-fn #'delve--zettel-equal-id)
                    (diff (-difference (delve--nodes-to-zettel (funcall (delve--query-fn query)))
                                       (-flatten (lister-get-sublist-below ewoc node)))))
          (lister-insert-sublist-below ewoc node diff)
          (setq count (+ count (length diff))))))
    count))

(defun delve--sync-items (ewoc nodes)
  "Update the Org Roam DB, then sync and update all NODES in EWOC.
Sync all NODES in the EWOC's buffer and remove those which have
no corresponding Org Roam Node. Return a plist (:synced :removed
:msgs) with the number of Zettels synced and removed, and a list
of warnings about unsaved buffers."
  (let* ((warnings nil)
         (zettel-nodes (--filter (delve--zettel-p (lister-node-get-data it)) nodes))
         (zettels (-map #'lister-node-get-data zettel-nodes)))
    ;; Update the Org Roam DB
    (cl-dolist (file (-uniq (-map #'delve--zettel-file zettels)))
      (when-let ((buf (get-file-buffer file)))
        (when (buffer-modified-p buf)
          (setq warnings (cons (format "Delve: Buffer visiting %s has been modified, DB might not be up to date" file) warnings))))
      (org-roam-db-update-file file))
    ;; Build an ID-indexed hash table for further examinations:
    (let ((hash (delve-store--create-node-table (-map #'delve--zettel-id zettels))))
      ;; Update all zettels, if possible.
      ;; Updated zettels are not 'out of sync' anymore.
      (--each zettels  (-let (((&hash (delve--zettel-id it) org-roam-node) hash))
                         (setf (delve--zettel-out-of-sync it) (not org-roam-node))
                         (when org-roam-node
                           (setf (delve--zettel-node it) org-roam-node)
                           (when (delve--zettel-preview it)
                             (setf (delve--zettel-preview it)
                                   (delve--get-preview-contents it))))))
      ;; Delete nodes with unlinked Zettels and update display:
      (-let (((unlinked update) (-separate #'delve--out-of-sync-p zettel-nodes)))
        (delve--save-outline ewoc
          (--each unlinked (delve--delete-item ewoc it)))
        (delve--refresh-nodes ewoc update)
        (list :synced (length update)
              :removed (length unlinked)
              :warnings warnings)))))

(defun delve--key--refresh (ewoc &optional prefix)
  "Sync all Zettels with the DB and re-insert query items.
With PREFIX, do not update the query items and force sync all
marked Zettel, or, if none is marked, the Zettel at point. Think
of PREFIX as a way focus on the marked Zettels only, ignoring any
queries and unmarked Zettels.

EWOC is the buffer's list object."
  (interactive (list lister-local-ewoc current-prefix-arg))
  ;; Determine the relevant nodes (plus type-checking of marked nodes):
  (let* ((pred (when (and prefix (delve--current-item-or-marked 'delve--zettel))
                  #'lister-node-marked-and-visible-p))
         (nodes (lister-collect-nodes ewoc :first :last pred)))
    ;; the actual sync:
    (-let* (((&plist :synced n-synced :removed n-removed :warnings warnings) (delve--sync-items ewoc nodes))
            (n-updates (if prefix 0 (delve--update-queries ewoc)))
            (info (concat (format "Synced %d items; inserted %d items; removed %d items"
                                  n-synced n-updates n-removed)
                          (when warnings (format ". There were warnings (see %s)" messages-buffer-name)))))
      (--each warnings (message it))
      (message (if (and (not warnings) (= 0 n-updates n-synced n-removed))
                   "Nothing to do"
                 info)))))

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
      (progn
        (delve-insert-items buf zettels)
        (message "Inserted %s zettels matching %s" (length zettels) matching-string))
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
  (let* ((tags (completing-read-multiple " Select nodes matching tag(s):"
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

;; * Compact View Minor Mode

(define-minor-mode delve-compact-view-mode
  "Minor mode for presenting Delve items in a more compact way."
  :lighter "▤"
  :group 'delve
  (delve--assert-buf)
  (setq delve-local-compact-view delve-compact-view-mode)
;;  (hl-line-mode (if delve-compact-view-mode +1 -1))
  (lister-save-current-node lister-local-ewoc
      (lister-refresh-list lister-local-ewoc)))

;; * Delete Items

(defun delve--ewoc-node-invalid-p (ewoc node)
  "Return t if NODE is not a valid node in EWOC."
  (or (null node)
      (and (not (marker-buffer (ewoc-location node)))
           (and (not (or (ewoc-next ewoc node)
                         (ewoc-prev ewoc node)))))))

(defmacro delve--save-outline (ewoc &rest body)
  "In EWOC, unhide all items, execute BODY and restore visibility.
Return the value returned by BODY."
  (declare (indent 1) (debug (sexp body)))
  (let ((nodes-var (gensym))
        (res-var (gensym)))
    `(let ((,nodes-var (lister-collect-nodes ,ewoc nil nil
                                             (-partial #'lister--outline-invisible-p ,ewoc))))
       (lister-outline-show-all ,ewoc)
       (let ((,res-var (progn ,@body)))
         (--each (-remove (-partial #'delve--ewoc-node-invalid-p ,ewoc) ,nodes-var)
           (lister--outline-hide-show ,ewoc it it t))
         ,res-var))))

(defun delve--delete-item (ewoc node)
  "In EWOC, delete NODE, taking care of indentation."
  (lister-with-sublist-below ewoc node beg end
    (lister-walk-nodes ewoc #'lister-move-item-left beg end))
  (lister-delete-at ewoc node))

(defun delve--key--multi-delete (ewoc)
  "Delete region, marked items or the single item at point.
If a region is active, delete all marked items and the items in
the region. Delete the item at point if no region is active and
if no items are marked.

Note that it is not possible to delete a full tree by just
deleting its root item. Indented items (\='branches' in a
tree-like structure) will be re-indented.

EWOC is the buffer's lister object."
  (interactive (list lister-local-ewoc))
  (let ((selection :marked))
    (delve--maybe-mark-region ewoc)
    (unless (lister-items-marked-p ewoc)
      (setq selection :at-point)
      (lister-mark-unmark-at ewoc :point t))
  (let* ((nodes (lister-collect-nodes ewoc nil nil #'lister-node-marked-and-visible-p))
         (n (length nodes))
         (item-s (if (= n 1) "item" "items")))
    (if (not (y-or-n-p (if (eq selection :marked)
                           (format  "Delete %d marked %s?" n item-s)
                         "Delete item at point?")))
        (user-error "Canceled")
      (delve--save-outline ewoc
          (--each nodes (delve--delete-item ewoc it)))
      (message "Deleted %d %s" n item-s)))))

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

(defun delve--prompt-new-storage-file ()
  "Ask for a file name for a Delve store."
  (delve--set-storage-dir)
  (let ((info "")          ;; info added to the prompt
        (res  nil)         ;; resulting file name
        (initial nil))     ;; when confirming fixed suffix
    (cl-labels ((dir-p (name) (when (file-exists-p name)
                                (file-directory-p name))))
      ;; FIXME Use a cl-loop construct instead
      (while (progn
               (setq res (read-file-name (format "%sDelve store file name: "
                                                 (propertize info 'face 'warning))
                                         delve--last-storage-dir nil nil initial)
                     initial nil
                     delve--last-storage-dir (delve--file-as-dir (file-name-directory res)))
               (if (dir-p res)
                   (setq info (format "Not a file name '%s'\n" (abbreviate-file-name res)))
                 (unless (delve--storage-file-name-p res)
                   (setq res (delve--fix-suffix res delve-storage-suffix)
                         initial (file-name-nondirectory res)
                         info (format "File must have suffix '%s', please confirm\n" delve-storage-suffix))))))
      res)))

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

;;; pTODO Write test
(defun delve--do-save-buffer (buf file-name)
  "Store the Delve list of BUF in FILE-NAME."
  ;; store list:
  (let ((l (lister-map (lister-get-ewoc buf)  #'delve-store--tokenize-object)))
    (unless (file-exists-p file-name)
      (make-empty-file file-name t))
    (delve-store--write file-name l)
    ;; link buffer to file:
    (delve--setup-file-buffer buf file-name)))

(defun delve--parse-list-from-file (l file-name)
  "Parse list of Delve objects L with error handling.
Pass FILE-NAME to inform the user if an error occurs."
  (condition-case nil
      (with-temp-message "Creating data objects..."
        (delve-store--parse-list l))
    (error (user-error "Could not parse file %s" file-name))))

;;; TODO Write test
(defun delve--read-storage-file (file-name)
  "Return a new Delve buffer read from FILE-NAME."
  (unless (file-exists-p file-name)
    (error "File not found %s" file-name))
  (let* ((l          (delve-store--read file-name))
         (delve-list (delve--parse-list-from-file l file-name))
         (buf-name   (format "Zettel imported from '%s'" (file-name-nondirectory file-name))))
    ;; link buffer to file:
    (delve--setup-file-buffer (delve--new-buffer buf-name delve-list) file-name)))

;; TODO Test if it works after introducing delve-storage-paths
(defun delve-save-buffer (buf &optional file-name)
  "Store BUF in its existing storage file or create a new one.
If FILE-NAME is not set, use the file name BUF is linked to.  If
BUF is not yet visiting any file, ask for a file name."
  (interactive (list (current-buffer)))
  (delve--assert-buf buf "Buffer to save must be in Delve mode")
  (let ((name  (or file-name
                   (buffer-local-value 'delve-local-storage-file buf)
                   (delve--prompt-new-storage-file))))
    (delve--do-save-buffer buf name))
  (with-current-buffer buf
    (message "Collection stored in file %s" delve-local-storage-file)))

;; TODO Test if it works after introducing delve-storage-paths
(defun delve-write-buffer (buf file-name)
  "Store BUF in FILE-NAME and associate it."
  (interactive (list (current-buffer) (delve--prompt-new-storage-file)))
  (delve-save-buffer buf file-name))

;; TODO Test if it works after introducing delve-storage-paths
(defun delve-find-storage-file ()
  "Open a Delve storage file or create a new Delve buffer.
If the user selects a non-storage file, pass to `find-file'."
  (interactive)
  (delve--set-storage-dir)
  (let* ((file-name   (expand-file-name (read-file-name "Find Delve storage or other file: " delve--last-storage-dir))))
    (pcase file-name
      ((pred delve--storage-p)
       (progn
         (switch-to-buffer (delve--read-storage-file file-name))
         ;; We set storage-dir here instead in the low level
         ;; functions, because else it would mess up the user's
         ;; workflow.
         (delve--set-storage-dir file-name)))
      ((pred delve--storage-file-name-p)
       (progn
         (switch-to-buffer (delve--new-buffer (file-name-base file-name)))
         ;; see above
         (delve--set-storage-dir file-name)))
      (_
        (find-file file-name)))))

;;; * Bookmark

;;;###autoload
(defun delve--bookmark-jump-handler (bookmark)
  "Open BOOKMARK pointing to a Delve collection file."
  (let* ((filename (bookmark-prop-get bookmark 'filename)))
    ;; either open existing buffer or create a new one
    (let ((buf (or (--first (equal (delve-get-storage-file it)
                                   filename)
                            (delve-buffer-list))
                   (delve--read-storage-file filename))))
      (switch-to-buffer buf)
      (delve--set-storage-dir filename))))

(defun delve--bookmark-record ()
  "Return a bookmark record for the current Delve buffer.
To enable special Delve bookmark handling, set the local value of
`bookmark-make-record-function' to this function."
  (unless (delve--buffer-p)
    (user-error "Non-Delve buffer not supported for Delve bookmarks"))
  (unless delve-local-storage-file
    (user-error "Only stored Delve collections can be bookmarked"))
  `(,delve-local-header-info
    (filename . ,delve-local-storage-file)
    (handler . ,#'delve--bookmark-jump-handler)))

;;; * Register Delve as Org Link Protocol

(require 'ol)

(defun delve-open-storage-file (path)
  "Visit Delve collection in PATH."
  (switch-to-buffer (delve--read-storage-file path))
  (delve--set-storage-dir path))

(defun delve--store-org-link ()
  "Return a link to the current buffer suitable for Org Mode."
  (when (and (memq major-mode '(delve-mode))
             delve-local-storage-file)
    (org-link-store-props
     :type "delve"
     :link (concat "delve:" delve-local-storage-file)
     :description delve-local-header-info)))

(defun delve--register-org-link ()
  "Register Org Links to Delve collections."
  (org-link-set-parameters "delve"
                           :follow #'delve-open-storage-file
                           :store  #'delve--store-org-link))

(defun delve--unregister-org-link ()
  "Unregister Org Links to Delve collections."
  (setq org-link-parameters
        (cl-delete "delve" org-link-parameters
                   :key #'car :test #'string=)))



;;; * Delve Major Mode

;; Help hl-line-mode to find the right range

(defun delve--get-hl-line-range ()
  "Return cons cell for highlighting a lister item in BUF."
  ;; TODO Report this to the emacs-devel list as a bug
  ;; Dirty Hack: hl-line-highlight is called before
  ;; pre-redisplay-functions, which is the hook used by
  ;; `cursor-intangible-mode'. So hl-line-highlight refers to a
  ;; position which will be changed afterwards if point is on an
  ;; intangible item. Thus we call here already this function which is
  ;; added to pre-redisplay-functions by `cursor-intangible-mode'.
  (when (and cursor-intangible-mode
             (get-buffer-window))
    (cursor-sensor--move-to-tangible (get-buffer-window)))
  (lister-with-node lister-local-ewoc :point node
    (when (lister--item-visible (ewoc-data node))
      (cons (marker-position (lister--item-beg (ewoc-data node)))
            (marker-position (lister--item-end (ewoc-data node)))))))

;; (defun delve--debug ()
;;   (message "Overlay %S;\nPoint: %d Beg-end: %S"
;;            hl-line-overlay (point)
;;            (delve--get-hl-line-range)))

;; * Transient Commands

(transient-define-prefix delve--node-transient-key ()
  "Transient for doing stuff with node(s)."
  [["Select and insert"
    ("n" "New node(s)"                      delve--key--insert-node)
    ("T" "New node(s) having #tag"          delve--key--insert-node-by-tags)]
   ["Insert all"
    ("b" "All backlinks of node at point"   delve--key--insert-backlink)
    ("f" "All links from current node"      delve--key--insert-fromlink)
    ("t" "All links matching tag(s)"        delve--key--insert-tagged)
    ("i" delve--key--insert-query-or-pile
     :description (lambda ()
                    (pcase (delve--current-item '(delve--query delve--pile))
                      ((cl-type delve--query) "Results from query at point")
                      ((cl-type delve--pile)  "Nodes from current pile, dissolving it")
                      (_ "Cannot be used here")))
     :if (lambda () (delve--current-item '(delve--query delve--pile)))
     )]]
  [["Edit"
    ("+" delve--key--add-tags
     :description (lambda ()
                        (concat "Add tag(s)"
                                (when (delve--marked-items-p) " in region"))))
    ("-" delve--key--remove-tags
     :description (lambda ()
                    (concat "Remove tag(s)"
                            (when (delve--marked-items-p) " in region"))))]
   ["Marked nodes"
    :if (lambda () (delve--marked-items-p))
    ("c" delve--key--collect-into-buffer
     :description "Add marked items to (new) collection")
    ("p" delve--key--collect-into-pile
     :description "Move marked items into a pile")
    ("<DEL>" delve--key--multi-delete
     :description "Delete all marked items")
    ("U" lister-mode-unmark-all
     :description "Unmark all"
     :if-non-nil lister-mode)]
   ]
  [["Visit"
    :if (lambda () (delve--current-item 'delve--zettel))
    ("o" "Open Org Roam file with this node" delve--key--open-zettel)]
   ["Structure"
    ("h" "Insert heading" delve--key--insert-heading)]]
  [["Quit"
    ("q" "Quit" transient-quit-one)]])

;; * Delve Keymap

(defvar delve-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Buffer as a whole:
    (define-key map (kbd "q")                        #'bury-buffer)
    (define-key map [remap save-buffer]              #'delve-save-buffer)
    (define-key map [remap write-file]               #'delve-write-buffer)
    (define-key map [remap find-file]                #'delve-find-storage-file)
    (define-key map [remap rename-buffer]            #'delve--key--edit-title)
    (define-key map (kbd "g")                        #'delve--key--refresh)
    (define-key map (kbd "v")                        #'delve-compact-view-mode)
    ;; Any item:
    (define-key map (kbd "<delete>")                 #'delve--key--multi-delete)
    (define-key map [remap yank]                     #'delve--key--yank)
    ;; Insert node(s):
    (define-key map (kbd "n")                        #'delve--node-transient-key)
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
    (define-key map (kbd "T")                        #'delve--key--insert-node-by-tags)
    ;; Sorting / Reordering:
    (define-key map (kbd "s")                        #'delve--key--sort)
    ;; Remote Editing:
    (define-key map (kbd "+")                        #'delve--key--add-tags)
    (define-key map (kbd "-")                        #'delve--key--remove-tags)
    map)
  "Key map for `delve-mode'.")

(define-derived-mode delve-mode
  fundamental-mode "Delve"
  "Major mode for browsing your Org Roam zettelkasten."
  (lister-setup	(current-buffer) #'delve-mapper  #'delve--header-function)
  (add-to-invisibility-spec '(org-link))
  (lister-mode)
  (setq-local hl-line-range-function #'delve--get-hl-line-range)
  (setq-local bookmark-make-record-function 'delve--bookmark-record)
  (setq-local lister-mark-face-or-property 'delve-mark-face)
  (setq-local filter-buffer-substring-function #'delve--tokenize-filter))

;;; * Main Entry Point

;;;###autoload
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
