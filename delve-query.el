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

;; DONE Check v2 database structure with dbbrowser
;; TODO Add version check for the "first" query call; store result in global variable
;; TODO Write delve.el for a quick helper which displays query results
;; TODO Copy the all-query-gets-them-all-query from org-roam
(require 'subr-x)
(require 'cl-lib)
(require 'seq)
(require 'benchmark)
(require 'org-roam)

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

;;; * Some helping functions

;; ;; This is a copy of org-roam--tags-table, copied here since it is not
;; ;; a public function
;; (defun delve-query--get-tags-table ()
;;   "Return a hash table of node ID to list of tags."
;;   (let ((ht (make-hash-table :test #'equal)))
;;     (pcase-dolist (`(,node-id ,tag) (org-roam-db-query [:select [node-id tag] :from tags]))
;;       (puthash node-id (cons tag (gethash node-id ht)) ht))
;;     ht))

;; ;;; * Plain queries returning simple strings

;; (defun delve-query-all-tags ()
;;   "Return an alphabetically sorted list of all node tags."
;;   (mapcar #'car
;;           (delve-query [:select :distinct [tag] :from tags :order :by (asc tag)])))

;; ;;; * Queries each returning a list of nodes

;; ;; General function to query the 'nodes' table and convert the results
;; ;; into a list of nodes.

;; (defun delve-query--sql-populate-nodes (sql-addition &rest args)
;;   "Build a query using SQL-ADDITION and return results as nodes.
;; Run a query which combines a predefined SQL query with an
;; SQL-ADDITION, optionally also passing ARGS to the SQL
;; expression.  Convert the results of the query into a list of
;; nodes.  Only fill the slots `file', `id', `level', `tag', `olp',
;; `pos' and `title'.

;; The predefined query selects fields from the table 'nodes'.  The
;; additional query should add a constraint, e.g. a WHERE clause."
;;   (let ((tag-table (delve-query--get-tags-table))
;;         (query (vconcat [:select [file id title level olp pos]
;;                          :from nodes]
;;                         sql-addition)))
;;     (cl-loop for row in (apply #'delve-query query args)
;;              collect (pcase-let ((`(,file ,id ,title ,level ,olp ,pos) row))
;;                        (org-roam-node-create :file file
;;                                              :id id
;;                                              :olp olp
;;                                              :point pos
;;                                              :tags (gethash id tag-table)
;;                                              :level level
;;                                              :title title)))))

;; (defun delve-query-nodes-matching-tag (tag)
;;   "Return partially populated nodes matching TAG.
;; TAG must be a string.

;; Does not fill all slots of the nodes; see
;; `delve-query--sql-populate-nodes' for a detailed list."
;;   (delve-query--sql-populate-nodes [:where
;;                                   :exists [:select [tag node_id]
;;                                                    :from tags
;;                                                    :where (= tag $r1)
;;                                                    :and (= nodes:id node_id)]]
;;                                  (delve-query-quote-string tag)))

;; ;; FIXME Die "IN" Abfrage führt dazu, dass mehrfache Backlinks in
;; ;; einem Node nur als ein Eintrag geführt werden. Ist das ein Problem?
;; ;; Lässt sich das vermeiden?
;; (defun delve-query-backlinks-to-node (node)
;;   "Return partially populated nodes pointing to NODE.
;; NODE must be node struct.

;; If one node contains several backlinks to NODE, only return one
;; candidate for that node.  This behaviour differs from the
;; backlinks section created by `org-roam-buffer', which is
;; populated with all backlinks whatsovever.

;; Does not fill all slots of the nodes; see
;; `delve-query--sql-populate-nodes' for a detailed list."
;;   (delve-query--sql-populate-nodes [ :WHERE nodes:id :IN
;;                                           [:SELECT source :FROM links
;;                                                    :WHERE (= links:dest $s1)
;;                                                    :AND (= type "id")]]
;;                                  (org-roam-node-id node)))


(provide 'delve-query)
;;; delve-query.el  ends here
