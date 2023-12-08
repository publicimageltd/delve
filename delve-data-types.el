;;; delve-data-types.el --- Data Types for the Delve Program  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023

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

;; Basic data types which are shared by many modules.

;;; Code:
(require 'cl-lib)
(require 'org-roam-node)

;;; * Data Types

;; Each list item has to be derived from this unspecific structure:
(cl-defstruct (delve--item (:constructor delve--item-create))
  "A generic single delve list item.")

(cl-defstruct (delve--note (:include delve--item)
                           (:constructor delve--note-create))
  "A note item for free text."
  text)

(cl-defstruct (delve--heading (:include delve--note)
                              (:constructor delve--heading-create))
  "A heading.")

(cl-defstruct (delve--info (:include delve--note)
                           (:constructor delve--info-create))
  "A text item for information to the user.")

(cl-defstruct (delve--zettel
               (:include delve--item)
               (:constructor delve--zettel-create (node)))
  "A Zettel item storing an Org Roam node."
  node preview out-of-sync info)

;; Some shortcuts to the node element of a zettel:
(defmacro delve-data-types--zettel--accessor-fn (name slot-name)
  "Define an accessor function for the node in a Zettel item.
SLOT-NAME must be the name of a slot of an org-roam-node.  Give
  the function the name NAME."
  `(defun ,name (z)
     ,(format "Access the slot %s of the node object stored in a Zettel item." slot-name)
     (cl-struct-slot-value 'org-roam-node ,slot-name (delve--zettel-node z))))

(delve-data-types--zettel--accessor-fn delve--zettel-title   'title)
(delve-data-types--zettel--accessor-fn delve--zettel-olp     'olp)
(delve-data-types--zettel--accessor-fn delve--zettel-properties 'properties)
(delve-data-types--zettel--accessor-fn delve--zettel-point   'point)
(delve-data-types--zettel--accessor-fn delve--zettel-id      'id)
(delve-data-types--zettel--accessor-fn delve--zettel-file    'file)
(delve-data-types--zettel--accessor-fn delve--zettel-tags    'tags)
(delve-data-types--zettel--accessor-fn delve--zettel-level   'level)
(delve-data-types--zettel--accessor-fn delve--zettel-aliases 'aliases)
(delve-data-types--zettel--accessor-fn delve--zettel-filetitle   'file-title)
(delve-data-types--zettel--accessor-fn delve--zettel-mtime   'file-mtime)
(delve-data-types--zettel--accessor-fn delve--zettel-atime   'file-atime)

(cl-defstruct (delve--pile
            (:include delve--item)
            (:constructor delve--pile-create))
  "A pile (list) of Zettels."
  name zettels)

(cl-defstruct (delve--query
            (:include delve--item)
            (:constructor delve--query-create))
  "A query returning Delve objects."
  info fn)

(provide 'delve-data-types)
;;; delve-data-types.el ends here
