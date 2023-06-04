;;; delve-edit-test.el --- Tests for delve-tests.el  -*- lexical-binding: t; -*-
;; Copyright (C) 2023

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

;; Tests for remote editing Org files.

;;; Code:
(require 'buttercup)
(require 'delve-test-db-utils)
(require 'delve-edit)

;; Test the org-roam-functions
;; org-roam-tag-{remove,add} directly in-buffer
;; (delve-edit-with-file nil ...)



(provide 'delve-edit-test)
;;; delve-edit-test.el ends here
