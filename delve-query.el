;;; delve-query.el --- Delve queries for the Org Roam database  -*- lexical-binding: t; -*-

;; Copyright (C) 2021

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

;; Provides functions to query the Org Roam DB.

;;; Code:

;;; * Dependencies

(require 'subr-x)
(require 'cl-lib)
(require 'seq)
(require 'dash)
(require 'benchmark)
(require 'org-roam-db)
(require 'org-roam-node)
(require 'emacsql-compiler)


;;; * Framework for 'save' and 'verbose' querying

(defconst delve-query-db-version 18
  "Org roam DB version on which delve query relies.
Must match `org-roam-db-version'.")

(defvar delve-query-catch-db-errors t
  "If set, catch and log database errors.")

(defvar delve-query-log-queries t
  "If set, log all SQL queries issued by Delve.")

(defvar delve-query-log-buffer "*Delve DB Interactions*"
  "Buffer name for logging Org Roam queries.")

(defun delve-query-log-buffer ()
  "Get or create a log buffer."
  (or (get-buffer delve-query-log-buffer)
      (with-current-buffer (get-buffer-create delve-query-log-buffer)
        (special-mode)
        (current-buffer))))

(defun delve-query-log (&rest strings)
  "Insert STRINGS in the log buffer.
NIL values are ignored.  Nested lists are flattened."
  (with-current-buffer (delve-query-log-buffer)
    (let* ((inhibit-read-only t)
           (date-string (format-time-string "%D %T")))
      (goto-char (point-max))
      ;; we use flatten-tree to remove nil values:
      (cl-dolist (s (flatten-tree strings))
        (insert (propertize date-string 'face 'compilation-line-number) " " s "\n")))))

(defun delve-query (sql &rest args)
  "Call Org Roam SQL query (optionally using ARGS) in a safe way.
If `delve-query-catch-db-errors' is set, catch all errors, log them
and return nil."
  ;; always check DB version
  (unless (eq org-roam-db-version delve-query-db-version)
    (let ((msg (if (> org-roam-db-version delve-query-db-version)
                   "ahead"
                 "outdated")))
      (error "Delve fatal error: Org Roam DB version %d is %s, expected %d"
             org-roam-db-version msg delve-query-db-version)))
  ;; now do the query:
  (condition-case err
      (let (res time)
        (when delve-query-log-queries
          (delve-query-log (format "%s" sql)
                           (when args (format " -- Args=%s" args))))
        (setq time
              (benchmark-run
                  (setq res
                        (apply #'org-roam-db-query
                               ;; emacsql calls the string sql as
                               ;; first argument, so escape all %
                               ;; signs if passing it:
                               (if (stringp sql) (emacsql-escape-format sql) sql)
                               (flatten-tree args)))))
        (when delve-query-log-queries
          (delve-query-log (format " -- query returns %d items in %.2f seconds."
                                 (length res)
                                 (car time))))
        res)
    (error (if (not delve-query-catch-db-errors)
               (signal (car err) (cdr err))
             (delve-query-log (format "delve-query error: %s" (error-message-string err)))))))

;;; * Some queries

(defvar delve-query--super-query
  "SELECT id, file, filetitle, \"level\", todo, pos, priority,
           scheduled, deadline , title, properties, olp, atime,
           mtime, '(' || group_concat(tags, ' ') || ')' as tags,
           aliases, refs FROM
           -- outer from clause
           (
           SELECT  id,  file, filetitle, \"level\", todo,  pos, priority,  scheduled, deadline ,
             title, properties, olp, atime,  mtime, tags,
             '(' || group_concat(aliases, ' ') || ')' as aliases,
             refs
           FROM
           -- inner from clause
             (
             SELECT  nodes.id as id,  nodes.file as file,  nodes.\"level\" as \"level\",
               nodes.todo as todo,   nodes.pos as pos,  nodes.priority as priority,
               nodes.scheduled as scheduled,  nodes.deadline as deadline,  nodes.title as title,
               nodes.properties as properties,  nodes.olp as olp,  files.atime as atime,
               files.title as filetitle,
               files.mtime as mtime,  tags.tag as tags,    aliases.alias as aliases,
               '(' || group_concat(RTRIM (refs.\"type\", '\"') || ':' || LTRIM(refs.ref, '\"'), ' ') || ')' as refs
             FROM nodes
             LEFT JOIN files ON files.file = nodes.file
             LEFT JOIN tags ON tags.node_id = nodes.id
             LEFT JOIN aliases ON aliases.node_id = nodes.id
             LEFT JOIN refs ON refs.node_id = nodes.id
             GROUP BY nodes.id, tags.tag, aliases.alias )
             -- end inner from clause
           GROUP BY id, tags )
           --- end outer from clause
         GROUP BY id\n"
  "Query which returns all nodes with all fields.")

(defun delve-query-node-list ()
  "Return all nodes."
  (org-roam-node-list))

(defun delve-query-do-super-query (query)
  "Call one big SQL QUERY and return results as Org Roam node structs.
QUERY must be `delve-query--super-query' or a subset.  See the
query `delve-query--super-query' for allowed fields."
  (cl-loop for row in (delve-query query)
           append (pcase-let* ((`(,id ,file ,file-title ,level ,todo ,pos ,priority ,scheduled ,deadline
                                      ,title ,properties ,olp ,atime ,mtime ,tags ,aliases ,refs)
                                row)
                               (all-titles (cons title aliases)))
                    (mapcar (lambda (temp-title)
                              (org-roam-node-create :id id
                                                    :file file
                                                    :file-title file-title
                                                    :file-atime atime
                                                    :file-mtime mtime
                                                    :level level
                                                    :point pos
                                                    :todo todo
                                                    :priority priority
                                                    :scheduled scheduled
                                                    :deadline deadline
                                                    :title temp-title
                                                    :properties properties
                                                    :olp olp
                                                    :tags tags
                                                    :refs refs))
                            all-titles))))

(defun delve-query--quote-string (string &optional add-wildcards add-double-quotes)
  "Return STRING as a quoted string to be used in SQL queries.
Wrap STRING in single quotes.  If ADD-DOUBLE-QUOTES is non-nil,
wrap STRING first in double quotes, then add wildcards, then add
single quotes.  Use ADD-WILDCARDS to add '%' to the left and the
right for use with the SQL Like operator.  In this case, also
escape any SQL wildcard characters in STRING using a regular
slash (which has to be added to the SQL clause via the ESCAPE
statement)."
  (->> string
       ;; Welcome to the quoting hell!
       ;; first we double all single quotes since the result will be
       ;; enclosed in single quotes
       (replace-regexp-in-string "'" "''")
       ;; now escape all double quotes, since this is how they are
       ;; stored in the DB, e.g. "\"quoted\"".
       (string-replace "\"" "\\\"")
       ;; if this will be used in a LIKE operator, we have to escape
       ;; all existing percent signs and underscores. We can't use a
       ;; backslash as escape character, since it is also used by the
       ;; LISP reader; so we use a regular slash. Don't forget to add
       ;; the ESCAPE clause to your query!
       (if add-wildcards
           (->> string
                (string-replace "/" "//")
                (string-replace "%" "/%")
                (string-replace "_" "/_")))
       ;; add double quotes
       (format (if add-double-quotes "\"%s\"" "%s"))
       ;; finally enclose the result in single quotes,
       ;; optionally wrapping it in wildcards.
       (format (if add-wildcards "'%%%s%%'" "'%s'"))))

(defun delve-query--quote-strings (strings sep add-wildcards)
  "Concatenate all STRINGS using SEP, wrapping each in double quotes.
Additionally wrap the double quoted string in percent signs (SQL
wildcards) when ADD-WILDCARDS is non-nil.

As a rule of thumb, always add wildcards when using the LIKE
operator in your SQL query, else not."
  (string-join (--map (delve-query--quote-string it add-wildcards t) strings)
               sep))

(defun delve-query-nodes-by-tags (tag-list)
  "Return all nodes with tags TAG-LIST."
  (when tag-list
    (delve-query-do-super-query
     (concat "SELECT * FROM ( " delve-query--super-query " ) "
             (format "WHERE tags LIKE %s ESCAPE '/' ORDER BY title"
                     ;; Column 'TAGS' is a list of items '("a"
                     ;; "b""c")', so enclose the tag in double quotes
                     ;; to ensure an exact match and add then the
                     ;; wildcard
                     (delve-query--quote-strings tag-list " AND TAGS LIKE " t))))))

(defun delve-query-tags (&optional ids)
  "Return all tags as a sorted list of strings.
Optionally restrict to those nodes with an id in IDS."
  (let* ((base-query [:select :distinct [tag] :from tags])
         (query      (if (null ids)
                         base-query
                       (vconcat base-query `[:where (in node_id ,(seq-into ids 'vector))]))))
    (seq-sort #'string< (seq-uniq (mapcar #'car (delve-query query))))))

(defun delve-query-nodes-by-todo (todo-state)
  "Return all nodes having TODO-STATE, sorted by title."
  (delve-query-do-super-query
   (concat delve-query--super-query
           (format "HAVING todo=%s ORDER BY title"
                   ;; Column 'TODO' is a list of items, so wrap
                   ;; todo-state in double quotes to ensure an exact match
                   (delve-query--quote-string todo-state nil t)))))

(defun delve-query-nodes-by-id (id-list)
  "Return all nodes in ID-LIST sorted by the node's title."
  (let ((nodes (with-temp-message (format "Querying database for %d nodes..." (length id-list))
                 (delve-query-do-super-query
                  (concat delve-query--super-query
                          (format "HAVING id IN (%s) ORDER BY title"
                                  ;; No need for SQL wildcards here;
                                  ;; simple quoting is enough.
                                  (delve-query--quote-strings id-list ", " nil)))))))
    (unless (eq (length nodes) (length id-list))
      ;; make sure inequality is not due to aliased nodes with same ID
      (when (-difference (-uniq (mapcar #'org-roam-node-id nodes))
                         (-uniq id-list))
          (message "delve: Could not get all requested IDs, maybe DB is out of sync?")))
    nodes))

(defun delve-query-node-by-id (id)
  "Return node with ID."
  (car (delve-query-nodes-by-id (list id))))

(defun delve-query--ids-linking-to (id)
  "Get all ids linking to ID (backlinks)."
  (flatten-tree (delve-query [:select [ source ]
                                      :from links
                                      :where (= dest $s1)
                                      :and (= type "id")]
                             id)))

(defun delve-query--ids-linking-from (id)
  "Get all ids linking from node ID (fromlinks)."
  (flatten-tree (delve-query [:select [ dest ]
                                      :from links
                                      :where (= source $s1)
                                      :and (= type "id")]
                             id)))

(defun delve-query-backlinks-by-id (id)
  "Get all nodes linking to ID."
  (let ((backlinks (delve-query--ids-linking-to id)))
    (delve-query-nodes-by-id (flatten-tree backlinks))))

(defun delve-query-fromlinks-by-id (id)
  "Get all nodes linking from ID."
  (let ((tolinks (delve-query--ids-linking-from id)))
    (delve-query-nodes-by-id (flatten-tree tolinks))))

(defun delve-query-unlinked ()
  "Get all nodes with no backlinks or tolinks."
  (let* ((ids (delve-query "SELECT id FROM nodes WHERE id NOT IN
  (SELECT source AS id FROM links WHERE type='\"id\"'
   UNION SELECT dest AS id FROM links WHERE type='\"id\"')")))
    (delve-query-nodes-by-id (flatten-tree ids))))

(defun delve-query-last-modified (&optional limit)
  "Get LIMIT last modified Org Roam nodes.
If LIMIT is unspecified, return the last 10 modified nodes."
  (-take (or limit 10)
         (-sort (-on (-compose #'not #'time-less-p) #'org-roam-node-file-mtime)
                (delve-query-node-list))))

(defun delve-query-by-title (title)
  "Get all nodes matching TITLE.
TITLE will be passed to an SQL query using `LIKE'."
  (delve-query-do-super-query
   (concat delve-query--super-query
           (format "HAVING title LIKE %s ESCAPE '/' ORDER BY title"
                   ;; Field 'title' is not a list (like 'tags'), so we
                   ;; don't need no double quotes here.
                   (delve-query--quote-string title t nil)))))

(provide 'delve-query)
;;; delve-query.el ends here
