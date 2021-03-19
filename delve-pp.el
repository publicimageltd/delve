;;; delve-pp.el --- Pretty printer for delve         -*- lexical-binding: t; -*-

;; Copyright (C) 2021

;; This file is part of Delve.

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

;; Utilities for pretty printing delve objects.

;;; Code:

(require 'cl-lib)

;; * Global Variables

(defvar delve-pp-inhibit-faces nil
  "If set, never add any faces when using the pretty printer.")

(defvar delve-pp-invalid-scheme-error-string "invalid pp-scheme: '%s'"
  "Error string to be returned when a pp scheme is invalid.
If set to nil, return nil instead.")

;; * Modify a String

;; -- The modifying functions

(defun delve-pp-mod:format (s format-spec)
  "Pass S to `format' using FORMAT-SPEC."
  (format format-spec s))

(defun delve-pp-mod:width (s width)
  "Pad or truncate S so that it fits WIDTH."
  (let* ((pad (- width (string-width s))))
    (if (<= pad 0)
	(setq s (substring s 0 width))
      (setq s (concat s (make-string pad ?\s))))))

(defun delve-pp-mod:set-face (s face-or-spec)
  "Set FACE-OR-SPEC as the face property of S."
  (propertize s 'face face-or-spec))

(defun delve-pp-mod:add-face (s face-or-spec)
  "Add FACE-OR-SPEC to the face properties of S."
  (with-temp-buffer
    (insert s)
    (add-face-text-property (point-min) (point-max) face-or-spec t)
    (buffer-string)))

(defun delve-pp-apply-mods (s mod arg)
  "Return S modified by applying MOD using ARG.
If MOD is not defined, return S unmodified.

The following mods are currently defined:

 (:width <n>)                    ;; restrict or pad output to <n> characters
 (:set-face <facename or spec>))     ;; return string with this face
 (:add-face <facename or spec>) ;; add face to the end of the string properties
 (:format  \"format-string\")    ;; pass nonempty value to format

If the global variable `delve-pp-inhibit-faces' is set to t, the
face mods will be ignored."
  (pcase (list mod arg)
    (`(:format   ,format-spec)     (delve-pp-mod:format s format-spec))
    (`(:width    ,width)           (delve-pp-mod:width  s width))
    ((and `(:set-face ,face-or-spec) (guard (null delve-pp-inhibit-faces)))
     (delve-pp-mod:set-face s face-or-spec))
    ((and `(:add-face ,face-or-spec) (guard (null delve-pp-inhibit-faces)))
     (delve-pp-mod:add-face s face-or-spec))
    (_ s)))

;; * Pretty Print an Item

(defun delve-pp-item (object pprinter mods)
  "Convert OBJECT to a string by passing it to PPRINTER and applying MODS.

If PPRINTER is a function, OBJECT is passed as an argument to
PPRINTER.

If PPRINTER is not a function, it is passed as a value to
`format'. The format string defaults to \"%s\" unless one of the
MODS specifies another format using the keyword `:format'.

MODS modify the resulting string. It can be either nil, leaving
the string untouched, or a property list, which is processed in
order. For a list of available mods, see `delve-pp-apply-mods'."
  (let (s format-spec)
    ;; first create the string to be modified
    (if (functionp pprinter)
	(setq s (funcall pprinter object))
      (setq format-spec (plist-get mods :format))
      (setq s (format (or format-spec "%s") pprinter)))
    ;; then apply the mods
    (when s
      (cl-loop for i below (length mods) by 2
	       do (let ((mod (elt mods i))
			(arg (elt mods (1+ i))))
		    (unless (and format-spec (eq mod :format))
		      (setq s (delve-pp-apply-mods s mod arg))))))
      ;; finally pass the result
    s))

(cl-defun delve-pp-line (object pp-schemes &optional (separator " "))
  "Return a pretty printed representation of OBJECT.

PP-SCHEMES is a list. Each item of this list can either be a
string, which is used as-is; or a pretty printer function
returning a string, to which the object is passed; or a list with
a pretty printer function and some properties determining how to
further modify its results.

This property list can be either an additional cons cell (like in
\(fn .\(:prop arg\)\) or a continuation of the list, like in \(fn
:prop arg\). See `delve-pp-apply-mods' for the list of available
mods.

The resulting string is created by joining all these results
using SEPARATOR, ignoring nil values. Returns an empty string if
all items returned nil values.

If a scheme is structurally invalid, include a descriptive error
message in the result. This output is formed by passing
`delve-pp-invalid-scheme-error-string' to `format'. Setting the
variable to nil will inhibit any feedback on invalid schemes.
Note that no check is being done on the values passed."
  (string-join
   (cl-remove-if #'null
		 (mapcar (lambda (it)
			   (pcase it
			     ((pred stringp)   it)
			     ((pred functionp) (delve-pp-item object it nil))
			     (`(,fn)           (delve-pp-item object fn nil))
			     (`(,fn ,mods)     (if (listp mods)
						   (delve-pp-item object fn mods)
						 (when delve-pp-invalid-scheme-error-string
						   (format delve-pp-invalid-scheme-error-string it))))
			     (`(,fn . ,mods)   (delve-pp-item object fn mods))
			     (_                (when delve-pp-invalid-scheme-error-string
					       (format delve-pp-invalid-scheme-error-string it)))))
			 pp-schemes))
   separator))

(provide 'delve-pp)
;;; delve-pp.el ends here
