;;; delve-test-utils.el ---  Helpers for testing delve -*- lexical-binding: t; -*-

;; Copyright (C) 2021  

;; Author:  Jörg Volbers <joerg@joergvolbers.de>
;; Keywords: delve

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

;; Most of the tests is an extension and a modified copy from
;; https://github.com/d12frosted/vulpea. Thanks to Boris Buliga for
;; that.

;;; Code:

(require 'buttercup)
(require 'org)
(require 'org-roam)

;; * Locations

(defun delve-test-orig-notes-dir ()
  "Directory containing test note files."
  (expand-file-name "note-files"))

(defun delve-test-temp-notes-dir ()
  "Return a directory name for a temporary collection of org roam notes."
  (expand-file-name (make-temp-name "note-files-")
		    temporary-file-directory))

(defun delve-test-setup-db ()
  "Provide a temporary org roam db to work with."
  (let* ((original-dir (delve-test-orig-notes-dir))
	 (new-dir      (delve-test-temp-notes-dir)))
    (copy-directory original-dir new-dir nil nil t)
    (setq org-roam-directory new-dir)
    (setq org-roam-db-location (concat
				(file-name-as-directory (delve-test-temp-notes-dir))
				"org-roam.db"))
    (org-roam-mode +1)
    (org-roam-db-build-cache)))

(defun delve-test-teardown-db ()
  "Delete the temporary org roam db."
  (org-roam-mode -1)
  (org-roam-db--close org-roam-db-location)
  (delete-file org-roam-db-location))
    
(provide 'delve-test-utils)
;;; delve-test-utils.el ends here
