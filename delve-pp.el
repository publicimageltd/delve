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

;; Utilities for pretty printing objects, or aspects of it, using
;; pretty printer functions.
;;
;; Common usage: Define pretty printer functions which return a string
;; representation for an object (e.g. for a struct).  Then, create a
;; scheme which works like a vamped up concat, defining fields which
;; can be modified before joining the results in one single string.
;;
;; Examples:
;;
;; (delve-pp-fields 1 '(number-to-string)) ;; pprinter as function
;;   =>"1"
;;
;; (delve-pp-fields 1 '(number-to-string " = " "%0d")) ;; .. and strings
;;   => "1 = 1"
;;
;; (delve-pp-fields nil '("hallo" ignore "du")) ;; nil values will be ignored
;;   => "hallo du"
;;
;; (delve-pp-fields nil '("hallo" (not :format "%s") "du"))
;;   => "hallo not du"
;;
;; (delve-pp-fields nil '("value" "=" "%s"))
;;   => "value = nil"
;;
;; (delve-pp-fields nil '("value" "=" ("%s" (:width 1))))
;;   => "value = n" (truncated)
;;
;; The following mods are currently defined:
;;
;; (:width <n>)                    ;; restrict field or pad output to <n> characters
;; (:set-face <facename or spec>)) ;; propertize string with this face
;; (:add-face <facename or spec>)  ;; add face to the end of possibly existing string properties
;; (:format  \"format-string\")    ;; pass nonempty return value of pprinter to format

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

(defun delve-pp--format (s format-spec)
  "Pass S to `format' using FORMAT-SPEC."
  (format format-spec s))

(defun delve-pp--set-width (s width)
  "Pad or truncate S so that it fits WIDTH."
  (let* ((pad (- width (string-width s))))
    (if (<= pad 0)
        (setq s (substring s 0 width))
      (setq s (concat s (make-string pad ?\s))))))

(defun delve-pp--set-face (s face-or-spec)
  "Set FACE-OR-SPEC as the face property of S."
  (propertize s 'face face-or-spec))

(defun delve-pp--add-face (s face-or-spec)
  "Add FACE-OR-SPEC to the face properties of S."
  (with-temp-buffer
    (insert s)
    (add-face-text-property (point-min) (point-max) face-or-spec t)
    (buffer-string)))

(defun delve-pp-apply-mods (s mod arg)
  "Return S modified by applying MOD using ARG.
If MOD is not defined, return S unmodified.

The following mods are currently defined:

 (:width <n>) ;; restrict field or pad output to <n> characters
 (:set-face <facename or spec>)) ;; return string with this face
 (:add-face <facename or spec>)  ;; add face to the end of the string properties
 (:format  \"format-string\")    ;; pass nonempty value to format

If the global variable `delve-pp-inhibit-faces' is set to t, the
face mods will be ignored."
  (pcase (list mod arg)
    (`(:format   ,format-spec)
     (delve-pp--format s format-spec))
    (`(:width    ,width)
     (delve-pp--set-width  s width))
    ((and `(:set-face ,face-or-spec)
          (guard (null delve-pp-inhibit-faces)))
     (delve-pp--set-face s face-or-spec))
    ((and `(:add-face ,face-or-spec)
          (guard (null delve-pp-inhibit-faces)))
     (delve-pp--add-face s face-or-spec))
    (_ s)))

;; * Pretty Print an Object

(defun delve-pp-field (object pprinter mods)
  "Turn OBJECT to a string by passing it to PPRINTER and applying MODS.

If PPRINTER is a function, OBJECT is passed as to as its sole
argument.

If PPRINTER is a string, it is used a value to `format'
OBJECT.

If PPRINTER is nil, use the default format string \"%s\" unless a
`:format' modifier is present.

MODS modify the resulting string. It can be either nil, leaving
the string untouched, or a property list, which is processed in
order. In this property list, the property designates a
modification, and the value is an argument to this intended
modification. For example, `(:set-face f)' means to propertize
the field with the face f. For a list of available mods, see
`delve-pp-apply-mods'."
  (let (s format-spec)
    ;; first create the string to be modified
    (if (functionp pprinter)
        (setq s (funcall pprinter object))
      (setq format-spec (plist-get mods :format))
      (setq s (format (or format-spec pprinter "%s") object)))
    ;; then apply the mods
    (when s
      (cl-loop for i below (length mods) by 2
               do (let ((mod (elt mods i))
                        (arg (elt mods (1+ i))))
                    (unless (and format-spec (eq mod :format))
                      (setq s (delve-pp-apply-mods s mod arg))))))
      ;; finally pass the result
    s))

(cl-defun delve-pp-fields (object pp-schemes &optional (separator " "))
  "Return a pretty printed multi-field representation of OBJECT.

PP-SCHEMES is a list of elements, each designating a 'field' to
represent the object.  Create the resulting string by joining all
these results using SEPARATOR, discarding nil values.  Return an
empty string if all fields returned nil values.

The element of PP-SCHEMES can either be a string, which is simply
used as-is and thus represents a constant value.  If it is a
function, it is treated as a pretty printing function which,
passed the object to it, returns a string presenting that object.
Finally, the element can be a list consisting of a pretty
printing function and an additional list with some further
specifications which determine how to modify the results from the
function (so-called 'mods').  In that latter case, a string can
also be used instead of a function name, again making it a
constant.

For syntactic sugar, this mod list can be either added as an
explicit additional cons cell like in \(accessor-fn .\(:prop
arg\)\).  Alternatively, it can be a simple continuation of the
list, like in \(accessor-fn :prop arg\).  See
`delve-pp-apply-mods' for the list of available mods.

If a scheme is structurally invalid, include a descriptive error
message in the result.  This output is formed by passing
`delve-pp-invalid-scheme-error-string' to `format'.  Setting the
variable to nil will inhibit any feedback on invalid schemes.
Note that no check is being done on the values passed."
  (string-join
   (cl-remove-if #'null
                 (mapcar (lambda (it)
                           (pcase it
                             ((pred stringp)   (delve-pp-field object it nil)) ;;it)
                             ((pred functionp) (delve-pp-field object it nil))
                             (`(,fn)           (delve-pp-field object fn nil))
                             (`(,fn ,mods)     (if (listp mods)
                                                   (delve-pp-field object fn mods)
                                                 (when delve-pp-invalid-scheme-error-string
                                                   (format delve-pp-invalid-scheme-error-string it))))
                             (`(,fn . ,mods)   (delve-pp-field object fn mods))
                             (_                (when delve-pp-invalid-scheme-error-string
                                               (format delve-pp-invalid-scheme-error-string it)))))
                         pp-schemes))
   separator))

(provide 'delve-pp)
;;; delve-pp.el  ends here
