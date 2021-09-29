;;; delve-query.el --- Delve queries for the org roam database  -*- lexical-binding: t; -*-

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

;; Provides functions to query the org roam db.

;;; Code:

;;; * Dependencies

;; TODO Add delve-query-backlinks-by-ids
;; TODO Add delve-query-tolinks-by-ids
;; NOTE Backlinks-Query in der alten DELVE Version suchen!

(require 'subr-x)
(require 'cl-lib)
(require 'seq)
(require 'benchmark)
(require 'org-roam-db)
(require 'org-roam-node)
(require 'emacsql-compiler)

;;; * Framework for 'save' and 'verbose' querying

(defconst delve-query-db-version 17
  "Org roam DB version on which delve query relies.
Must match `org-roam-db-version'.")

(defvar delve-query-catch-db-errors t
  "If set, catch and log database errors.")

(defvar delve-query-log-queries t
  "If set, log all SQL queries issued by Delve.")

(defvar delve-query-log-buffer "*Delve DB Interactions*"
  "Buffer name for logging org roam queries.")

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
  "Call org roam SQL query (optionally using ARGS) in a safe way.
If `delve-query-catch-db-errors' is set, catch all errors, log them
and return nil."
  ;; always check DB version
  (unless (eq org-roam-db-version delve-query-db-version)
    (let ((msg (if (> org-roam-db-version delve-query-db-version)
                   "ahead"
                 "outdated")))
      (error "Delve fatal error: org roam db version %d is %s, expected %d"
             org-roam-db-version msg delve-query-db-version)))
  ;; now do the query:
  (condition-case err
      (let (res time)
        (when delve-query-log-queries
          (delve-query-log (format "%s" sql)
                           (when args (format " -- Args=%s" args))))
        (setq time
              (benchmark-run (setq res (apply #'org-roam-db-query sql (flatten-tree args)))))
        (when delve-query-log-queries
          (delve-query-log (format " -- query returns %d items in %.2f seconds."
                                 (length res)
                                 (car time))))
        res)
    (error (if (not delve-query-catch-db-errors)
               (signal (car err) (cdr err))
             (delve-query-log (error-message-string err))))))

(defun delve-query-quote-string (s)
  "Quote string S for use as an emacsSQL argument."
  (concat "\"" s "\""))

;;; * Some queries

(defvar delve-query--super-query
  "SELECT id, file, \"level\", todo, pos, priority,
           scheduled, deadline , title, properties, olp, atime,
           mtime, '(' || group_concat(tags, ' ') || ')' as tags,
           aliases, refs FROM
           -- outer from clause
           (
           SELECT  id,  file, \"level\", todo,  pos, priority,  scheduled, deadline ,
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
  "Call one big SQL QUERY and return results as nodes.
QUERY must be `delve-query--super-query' or a subset.  See the
query `delve-query--super-query' for allowed fields."
  (cl-loop for row in (delve-query query)
           append (pcase-let* ((`(,id ,file ,level ,todo ,pos ,priority ,scheduled ,deadline
                                      ,title ,properties ,olp ,atime ,mtime ,tags ,aliases ,refs)
                                row)
                               (all-titles (cons title aliases)))
                    (mapcar (lambda (temp-title)
                              (org-roam-node-create :id id
                                                    :file file
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

(defun delve-query--scalar-string (string)
  "Return STRING as a quoted scalar string."
  (thread-first string
    (emacsql-quote-identifier)
    (emacsql-quote-scalar)))

(defun delve-query--scalar-strings (strings)
  "Return STRINGS as a string with quoted scalar values."
  (string-join (mapcar #'delve-query--scalar-string
                       strings)
               ", "))

(defun delve-query-nodes-by-tags (tag-list)
  "Return all nodes with tags TAG-LIST."
  (delve-query-do-super-query
   (concat "SELECT * FROM ( " delve-query--super-query " ) "
           (format "WHERE tags LIKE %s"
                   (string-join (mapcar (lambda (s)
                                          (thread-last s
                                            (emacsql-quote-identifier)
                                            ;; emacsql-parse passes SQL to
                                            ;; #'format, so double % to avoid
                                            ;; interpretation as format char
                                            (format "%%%%%s%%%%")
                                            (emacsql-quote-scalar)))
                                        tag-list)
                                " AND tags LIKE ")))))

;; (delve-query-nodes-by-tags '("Referenz" "gedanke"))

(defun delve-query-tags ()
  "Return all tags as a sorted list of strings."
  (seq-sort #'string< (seq-uniq (mapcar #'car (delve-query [:select :distinct [tag] :from tags])))))

(defun delve-query-nodes-by-id (id-list)
  "Return all nodes in ID-LIST."
  (delve-query-do-super-query
   (concat delve-query--super-query
           (format "HAVING id IN (%s)" (delve-query--scalar-strings id-list)))))

(defun delve-query-node-by-id (id)
  "Return node with ID."
  (car (delve-query-nodes-by-id (list id))))

;; (delve-query-nodes-by-id '("343cf09a-c197-4878-a16b-54215b525b17" "996632c7-5480-47ae-b885-485296267220"))


(provide 'delve-query)
;;; delve-query.el  ends here
