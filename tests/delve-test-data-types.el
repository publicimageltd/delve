;;; delve-data-types-test.el --- test for delve-data-types.el  -*- lexical-binding: t; -*-

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

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Tests for delve-data-types.el

;;; Code:

(require 'buttercup)
(require 'delve-data-types)

(describe "Core Data Types:"
  (it "Distinguish data items by their type"
    (let* ((page     (delve-make-page))
	   (tolink   (delve-make-tolink))
	   (backlink (delve-make-backlink)))
      (expect (type-of page)     :to-be 'delve-page)
      (expect (type-of tolink)   :to-be 'delve-tolink)
      (expect (type-of backlink) :to-be 'delve-backlink)))
  (it "Check for result type of searches."
    (let* ((page-search (delve-make-page-search))) 
      (expect (delve-page-search-result-makefn page-search)
	      :to-be 'delve-make-page))))

(provide 'delve-data-types-test)
;;; delve-data-types-test.el ends here
