;;; delve-export.el --- Export capabilities for Delve  -*- lexical-binding: t; -*-

;; Copyright (C) 2021

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

;; Export capabilities for Delve.

;;; Code:
(require 'delve-data-types)

(cl-defstruct (delve-export-backend (:constructor delve-export-backend-create))
  "A backend for exporting Delve objects.
Slot NAME is a name (symbol) for the backend.

OPTIONS holds a property list of options, which are passed to
each exporting function.

Slots HEADER, FOOTER and SEPARATOR are either strings, which are
inserted as is, or functions returning a value to be
inserted. The functions are called with a list of options.

SEPARATOR can be nil, a string or a function with two
arguments (object and options) returning a string. If SEPARATOR is
non-nil, it will be inserted between each item (including
the header and the footer).

Slot PRINTERS is an alist, associating each Delve object type
with a printer function.  The printer function is called with the
object and a list of options. Those options also include the
computed value of the other slots, e.g. the final value of the
slot separator is associated with the property `:separator'."
  assert name printers header footer separator options)

;; This is the main workhorse for exporting.  Its design is rather
;; inconsequential: The printer return string values, but recursion is
;; impossible since everything is inserted directly (instead of
;; accumulating it).
(defun delve-export--insert-item (backend object options)
  "Print OBJECT in current buffer using the printer defined in BACKEND.
Insert the results of calling the printer function with two
arguments, the Delve OBJECT to be printed and a property list of
OPTIONS.  If the options property `:separator' is non-nil,
additionally also insert its associated value between items.

Do nothing if no printer is found or if the printer function
returns nil."
  (when-let ((printer (alist-get (type-of object)
                                 (delve-export-backend-printers backend))))
    (when-let ((item (funcall printer object options)))
      (insert item)
      (when-let ((newline (plist-get options :separator)))
        (unless (and (plist-get options :last)
                     (not (plist-get options :header))
                     (not (eq (type-of object) 'delve--pile)))
          (insert newline))))))

(defun delve-export--get-string-or-fn-value (value &rest args)
  "Return VALUE if it is a string or call it with ARGS."
  (when value
    (pcase value
      ((pred stringp)    value)
      ((or (pred functionp)
           (and (pred consp)
                (app car 'closure)))
       (apply value args))
      (_ (error "Cannot determine object type of %S" value)))))

(defun delve-export--get-slot-option (instance slot key options)
  "For an INSTANCE of a struct, determine the value of SLOT.
Get the final value of SLOT by returning the first non-nil result
of checking first the property KEY in OPTIONS, then the SLOT in
INSTANCE.  Return nil if no value is found.  If the value is a
string, return it directly.  Else if it is a function name or a
function object, return the result of calling that function with
the property list OPTIONS as its sole argument."
  (delve-export--get-string-or-fn-value
   (or (plist-get options key)
       (cl-struct-slot-value 'delve-export-backend slot instance))
   options))

(defun delve-export--insert (buf backend delve-objects
                                 &optional extra-options)
  "Insert DELVE-OBJECTS into BUF using BACKEND.
Use the options defined in BACKEND.  Optionally also use
EXTRA-OPTIONS, which override the backend options.  You can also
pass overriding values for the backend slots."
  (with-current-buffer buf
    (let* ((n       (length delve-objects))
           (options (append extra-options
                            (list :n-total n)
                            (delve-export-backend-options backend))))
      ;; test slot or option :assert
      (if (delve-export--get-slot-option backend 'assert :assert options)
          ;; assertion succeeded:
          (let* ((header  (delve-export--get-slot-option backend 'header    :header    options))
                 (footer  (delve-export--get-slot-option backend 'footer    :footer    options))
                 (sep     (delve-export--get-slot-option backend 'separator :separator options))
                 (options (append (list
                                   :separator sep
                                   :header header
                                   :footer footer)
                                  options)))
            ;;
            (when header (insert (concat header sep)))
            (when delve-objects
              (let ((last-n (1- n)))
                (--each-indexed delve-objects
                  (delve-export--insert-item backend it
                                             (-> options
                                                 (plist-put :index it-index)
                                                 (plist-put :last  (eq it-index last-n)))))))
            (when footer (insert (concat footer sep))))
        ;; assertion failed:
        (error "Backend %s: assertion failed, cannot export" (delve-export-backend-name backend))))))

;; * Export to Org Links

(defun delve-export--zettel-to-link (z &optional args)
  "Return zettel Z as an Org link pointing to its headline.
Optional argument ARGS is ignored."
  (ignore args)
  (org-link-make-string (concat "id:" (delve--zettel-id z))
                        (delve--zettel-title z)))

(defvar delve-export--backend-for-yanking
  (delve-export-backend-create
   :assert (lambda (_) (derived-mode-p 'org-mode))
   :name 'yank-into-org
   :header nil
   :footer (lambda (o) (when (> (plist-get o :n-total) 1) ""))
   :options nil
   :separator "\n"
   :printers `((delve--pile    . ,(lambda (p o)
                                    (let ((sep (plist-get o :separator)))
                                      (concat "* " (delve--pile-name p) sep
                                              (string-join
                                               (mapcar #'delve-export--zettel-to-link
                                                       (delve--pile-zettels p))
                                               sep)))))
               (delve--note    . ,(lambda (n _) (delve--note-text n)))
               (delve--heading . ,(lambda (h _) (concat "* " (delve--heading-text h))))
               (delve--info    . ,(lambda (i _) (delve--info-text i)))
               (delve--zettel  . ,#'delve-export--zettel-to-link)))
    "Backend for exporting Delve items to simple Org mode links.")

(provide 'delve-export)
;;; delve-export.el ends here
