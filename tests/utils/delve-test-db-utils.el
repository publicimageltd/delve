;;; delve-test-db-utils.el ---  Helpers for testing delve -*- lexical-binding: t; -*-

;; Copyright (C) 2021  

;; Author:  Jörg Volbers <joerg@joergvolbers.de>

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

;; Provides utilities for testing interaction with an org roam
;; database. Adopted from https://github.com/d12frosted/vulpea.
;;
;; For these functions to work, a separate directory with org roam
;; note files has to be provided. It has to be accessed from this
;; file's location. See the variable "delve-note-files-directory".

;;; Code:

(require 'buttercup)
(require 'org)
(require 'org-roam)
(require 'org-id)

;; * Locations

(defvar delve-test-environment nil
  "Whether the test environment exists.")

(defvar delve-note-files-directory "note-files"
  "Name of the directory for the note files to create the db.")

(defun delve-test-orig-notes-dir ()
  "Return directory containing test note files."
  (or
   (locate-file delve-note-files-directory
		(list default-directory) nil
		(lambda (f) (if (file-directory-p f) 'dir-ok)))
   (locate-file delve-note-files-directory
		load-path nil
		(lambda (f) (if (file-directory-p f) 'dir-ok)))
   (user-error "Could not create test data base; canceled")))

(defun delve-test-temp-notes-dir ()
  "Create a new directory name for a collection of org roam notes."
  (expand-file-name (make-temp-name "note-files-")
		    temporary-file-directory))

(defun delve-test-get-file (file-name)
  "Get file FILE NAME from the test data base.
This requires `delve-test-setup-db' to have been called."
  (if delve-test-environment
      (concat
       (file-name-as-directory org-roam-directory)
       file-name)
    (user-error "delve-test-get-file has to be called within a test environment.")))

(defun delve-test-all-org-files ()
  "Return all org file names from the test environment."
  (if delve-test-environment
      (directory-files org-roam-directory nil "\\.org$")
    (user-error "delve-test-all-org-files has to be called within a test environment.")))

(defun delve-test-setup-db ()
  "Provide a temporary org roam db to work with."
  (let* ((original-dir (delve-test-orig-notes-dir))
	 (new-dir      (delve-test-temp-notes-dir))
	 (inhibit-message t))
    (copy-directory original-dir new-dir nil nil t)
    (setq delve-test-environment t)
    (setq org-roam-directory new-dir)
    (setq org-roam-db-location (concat
				(file-name-as-directory new-dir)
				"org-roam.db"))
    (setq org-roam-db-update-method 'immediate)
    (org-id-update-id-locations (delve-test-all-org-files))    
    (org-roam-mode +1)
    (org-roam-db-build-cache)
    (sleep-for 2)))

(defun delve-test-teardown-db (&optional dont-move)
  "Delete the temporary org roam db."
  (unless delve-test-environment
    (user-error "nothing to tear down; no test environment given"))
  (org-roam-mode -1)
  (unless dont-move
    (let ((new-dir (expand-file-name "note-files-last-test-run"
				     temporary-file-directory)))
      (message "Moving database %s to %s...."
	       org-roam-directory new-dir)
      (when (file-exists-p new-dir)
	(delete-directory new-dir t))
      (rename-file org-roam-directory new-dir t)))
  (setq delve-test-environment nil))

;; SQL statements are long, so give them some space:
(setq buttercup-stack-frame-style 'pretty)

(provide 'delve-test-db-utils)
;;; delve-test-db-utils.el ends here
