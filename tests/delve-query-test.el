;;; delve-query-test.el --- Test delve query functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2021

;; Author:  <joerg@joergvolbers.de>
;; Keywords: internal

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

;; Tests the interaction with the database.

;;; Code:
(require 'buttercup)
(require 'delve-test-db-utils)
(require 'delve-query)
(require 'delve-store)



(xdescribe "Test if DB is in sync"
  (before-all
    (delve-test-setup-db))
  (after-all
    (delve-test-teardown-db))

  (it "org roam db has all IDs"
    (let* ((db-ids (cl-loop for node-id in (org-roam-db-query [:select id :from nodes])
                            append node-id))
           (file-ids (delve-test-collect-ids)))
      (expect db-ids :to-have-same-items-as file-ids))))

(xdescribe "Test DB Queries"
  (before-all
    (delve-test-setup-db))
  (after-all
    (delve-test-teardown-db))

  (describe "delve-query-node-list"
    (it "returns all nodes"
      (let ((nodes (delve-query-node-list)))
        (expect (mapcar #'org-roam-node-id nodes)
                :to-have-same-items-as
                (delve-test-collect-ids)))))

  (describe "delve-query-nodes-by-tags"
    (it "returns all nodes with a single tag"
      (let ((nodes (delve-query-nodes-by-tags '("meat"))))
        (expect (mapcar #'org-roam-node-id nodes)
                :to-have-same-items-as
                '("41ce628a-9631-4f51-92a3-1476ac9d4a61"
                  "54713c90-13da-4ea9-ab97-b056e4f47535"
                  "5fa37556-7c0f-4e7f-ba94-00dfb3388731"
                  "e9d02eb6-22e1-4549-890d-6f1d8d4ec744"))))
    (it "returns all nodes matching two tags (boolean AND)"
      (let ((nodes (delve-query-nodes-by-tags '("meat" "vegetarian"))))
        (expect (mapcar #'org-roam-node-id nodes)
                :to-have-same-items-as
                '("e9d02eb6-22e1-4549-890d-6f1d8d4ec744"))))
    (it "returns nil if called with empty list"
      (let ((nodes (delve-query-nodes-by-tags nil)))
        (expect nodes :to-be nil))))

  (describe "delve-query-tags"
    (it "returns all tags"
      (let ((tags (delve-query-tags)))
        (expect tags :to-have-same-items-as
                (delve-test-collect-tags))))
    (it "returns all tags sorted"
      (let ((tags (delve-query-tags)))
        (expect tags :to-equal
                (sort (delve-test-collect-tags) #'string<)))))

  (describe "delve-query-nodes-by-id"
    (it "returns nodes"
      (let ((ids '("b77a4837-71d6-495e-98f1-b576464aacc1"
                   "92a06447-2400-4c33-948c-c76fecda5ad2")))
        (expect (mapcar #'org-roam-node-title
                 (delve-query-nodes-by-id ids))
                :to-have-same-items-as
                '("Big note sub-heading"
                  "Spinach")))))
  (describe "delve-query-node-by-id"
    (it "returns the node"
      (let ((id "b77a4837-71d6-495e-98f1-b576464aacc1"))
        (expect (org-roam-node-title
                 (delve-query-node-by-id id)))
                :to-equal
                "Big note sub-heading"))))


(provide 'delve-query-test)
;;; delve-query-test.el ends here
