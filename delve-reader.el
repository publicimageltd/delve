;;; delve-reader.el --- file i/o for the Delve tool  -*- lexical-binding: t; -*-

;; Copyright (C) 2021

;; Author:  <joerg@joergvolbers.de>
;; Keywords: files

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

;; Library for reading and writing list data in files.

;;; Code:

(require 'cl-lib)

(defun delve-reader-write (filename lisp-object &optional header)
  "Write LISP-OBJECT to FILENAME.
Add string HEADER as a comment line to the top of the file.
Return LISP-OBJECT."
  (let* ((coding-system-for-write 'binary)
         (print-level nil)
         (print-length nil)
         (content (concat
                   (format ";;; file created: %s at %s h"
                           (format-time-string "%x")
                           (format-time-string "%H:%M"))
                   "\n\n"
                   (when header
                     (concat header "\n\n"))
                   (pp-to-string lisp-object))))
    (with-temp-file filename
      (set-buffer-multibyte nil)
      (encode-coding-string content 'utf-8 nil (current-buffer)))
  lisp-object))

(defun delve-reader-read (file-name)
  "Read FILE-NAME as Lisp expression and return it."
  (if (file-exists-p file-name)
      (with-temp-buffer
        (insert-file-contents-literally file-name)
        (condition-case err
            (car (read-from-string (decode-coding-region (point-min) (point-max) 'utf-8 t)))
          ((end-of-file scan-error invalid-read-syntax circular-list)
           (error "File '%s' could not be read, returned error '%s'"
                  file-name
                  (error-message-string err)))
          (error (error "Error reading data base: %s" (error-message-string err)))))
    (error "File not found: %s" file-name)))

(provide 'delve-reader)
;;; delve-reader.el ends here
