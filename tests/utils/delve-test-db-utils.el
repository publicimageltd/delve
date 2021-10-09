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
;; database.  Adopted from https://github.com/d12frosted/vulpea.
;;
;; For these functions to work, a separate directory with org roam
;; note files has to be provided.  It has to be accessible from this
;; file's location.  The path is set in the variable
;; "delve-note-files-directory".

;;; Code:

;;(require 'buttercup)
(require 'org)
(require 'org-roam)
(require 'org-id)

;; * Creating the Test Environment

(defvar delve-test-environment nil
  "Whether the test environment exists.")

(defvar delve-note-files-directory "note-files"
  "Name of the directory for the note files to create the db.
The path has to be relative to the directory from where this file
is called or required, or the `load-path'.")

(defun delve-test-orig-notes-dir ()
  "Return directory containing test note files."
  (cl-labels ((is-dir (f)
                      (when (file-directory-p f) 'dir-ok)))
    (or
     ;; first check from the calling directory:
     (locate-file delve-note-files-directory
		  (list default-directory) nil  #'is-dir)
     ;; now check the load path:
     (locate-file delve-note-files-directory
		  load-path nil #'is-dir)
     (user-error "Could not create data base; canceled"))))

(defun delve-test-temp-notes-dir ()
  "Create a new directory name for a collection of org roam notes."
  (expand-file-name (make-temp-name "note-files-")
		    temporary-file-directory))

(defun delve-test-get-file (file-name)
  "Get file FILE-NAME from the test data base.
This requires `delve-test-setup-db' to have been called."
  (if delve-test-environment
      (concat (file-name-as-directory org-roam-directory)
              file-name)
    (error "Function delve-test-get-file has to be called within a test environment")))

(defun delve-test-all-org-files ()
  "Return all org file names from the test environment."
  (if delve-test-environment
      (directory-files org-roam-directory t "\\.org$")
    (user-error "Function delve-test-all-org-files has to be called within a test environment")))

(defun delve-test--collect-from-file (file)
  "Collect all IDs in FILE without using org roam."
  (let* ((kill-it (not (find-buffer-visiting file)))
         (buf (find-file-noselect file))
         (ids (with-current-buffer buf
                (let ((info (org-element-parse-buffer)))
                  (org-element-map info 'node-property
                    (lambda (elt)
                      (when (equal "ID" (org-element-property :key elt))
                        (org-element-property :value elt))))))))
    (when kill-it
      (kill-buffer buf))
    ids))
    

(defun delve-test-collect-ids (files)
  "Collect all IDs from FILES without using org roam."
  (cl-loop for file in files
           append (delve-test--collect-from-file file)))

;; Evaluate the inner sexp to test if IDs are gathered correctly:
;;(defun delve-test--collect-by-hand ()
  ;; (setq delve-test-ids
  ;;       (delve-test-collect-ids
  ;;        (directory-files (concat default-directory "../note-files/") t "\\.org$"))))

(defun delve-test-setup-db ()
  "Provide a temporary org roam db to work with."
  (let* ((original-dir (delve-test-orig-notes-dir))
	 (new-dir      (delve-test-temp-notes-dir))
	 (inhibit-message t)
         (org-id-extra-files nil)
         (org-id-files nil))
    ;;
    (copy-directory original-dir new-dir nil nil t)
    (setq delve-test-environment t)
    (setq org-roam-directory new-dir)
    (setq org-roam-db-location (concat
				(file-name-as-directory new-dir)
				"org-roam.db"))
    ;; 
    (org-roam-db-sync)))

(defun delve-test-move-temp-db (target)
  "Move the DB from the test environment to directory TARGET."
  (unless delve-test-environment
    (error "Nothing to copy, no test environment"))
  (with-temp-message (format "Moving temporary database to %s" target)
    (when (file-exists-p target)
      (delete-directory target t))
    (rename-file org-roam-directory target t)))

(defun delve-test-teardown-db (&optional dont-backup)
  "Close the temporary org roam db and make a backup copy.
Do not make a backup of the database if DONT-BACKUP is non-nil."
  (unless delve-test-environment
    (user-error "Nothing to tear down; no test environment given"))
  (unless dont-backup
    (let ((new-dir (expand-file-name
                    (format-time-string "note-files-db-at-%H_%M_%S")
		    temporary-file-directory)))
      (delve-test-move-temp-db new-dir)))
  (setq delve-test-environment nil))

(provide 'delve-test-db-utils)
;;; delve-test-db-utils.el ends here
