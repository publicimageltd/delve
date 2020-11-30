;;; delve-data-types.el --- data types for the delve tool  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  

;; Author:  <joerg@joergvolbers.de>
;; Keywords: 

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

;; Defines same basic types for the delve zettelkasten explorer.

;;; Code:

(require 'cl-lib)

;;; * Item data types

(cl-defstruct (delve-tag (:constructor delve-make-tag))
  tag
  count)

(cl-defstruct (delve-zettel (:constructor delve-make-zettel))
  title
  file
  tags
  mtime
  atime
  outline
  backlinks
  tolinks)

(cl-defstruct (delve-page
	       (:constructor delve-make-page)
	       (:include delve-zettel)))

(cl-defstruct (delve-tolink 
	       (:constructor delve-make-tolink)
	       (:include delve-zettel)))

(cl-defstruct (delve-backlink
	       (:constructor delve-make-backlink)
	       (:include delve-zettel)))



;;; Searches

 (cl-defstruct (delve-generic-search (:constructor delve-make-generic-search))
  name
  with-clause
  constraint
  args
  postprocess
  result-makefn)

(cl-defstruct (delve-page-search
	       (:constructor delve-make-page-search)
	       (:include delve-generic-search
			 (result-makefn 'delve-make-page))))

(provide 'delve-data-types)
;;; delve-data-types.el ends here
