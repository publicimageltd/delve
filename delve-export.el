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

;; TODO We don't need to pass around "the whole list of objects";
;; change that
(cl-defstruct (delve-export-backend (:constructor delve-export-backend-create))
  "A backend for exporting Delve objects.
Slot NAME is a name (symbol) for the backend.

OPTIONS holds a property list of options, which are passed to each exporting function.

Slots HEADER, FOOTER and NEWLINE are either strings, which are
inserted as is, or functions returning a value to be
inserted. The functions are called with two arguments, the whole
list of objects to be exported and an extra list of options.

SEPARATOR can be nil, a string or a function with two
arguments (object and options) returning a string. If SEPARATOR is
non-nil, it will be inserted between each item (including
the header and the footer).

Slot PRINTERS is an alist, associating each Delve object
type with a printer function (which is called with the object and
a list of options)."
  name printers header footer separator options)

;; TODO Allow recursion (for piles)
;; Current implementation can't recurse because of the argument
;; BACKEND. Maybe recurse if return value is a list (instead of a
;; string or nil)? Then recurse over the list.

(defun delve-export--insert-item (backend object options)
  "Print OBJECT using the printer defined in BACKEND.
The printer function returns a string representation of the item
to be inserted the current buffer.  It is called with two
arguments, the Delve OBJECT and a property list of OPTIONS.
After printing the item, insert the value of the options property
:separator if it is non-nil.  Do not add a newline for the last
item if there is no header.

Do nothing if no printer is found or if the printer function
returns nil."
  (when-let ((printer (alist-get (type-of object)
                                 (delve-export-backend-printers backend))))
    (when-let ((item (funcall printer object options)))
      (insert (format "%S" (plist-get options :index)) item)
      (when-let ((newline (plist-get options :separator)))
        (unless (and (plist-get options :last)
                     (not (plist-get options :header)))
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

(defun delve-export--get-slot-option (backend slot key delve-objects options)
  "Determine the value for SLOT.
Get the value by first checking the KEY in OPTIONS, and if that
value is not determined, use the value of SLOT in BACKEND.  In
both cases, treat the value as a multi-type value (string or a
function with the two arguments DELVE-OBJECTS and OPTIONS)."
  (delve-export--get-string-or-fn-value
   (or (plist-get options key)
       (cl-struct-slot-value 'delve-export-backend slot backend))
   delve-objects
   options))

(defun delve-export--insert (buf backend delve-objects
                                 &optional extra-options)
  "Insert DELVE-OBJECTS into BUF using BACKEND.
Use the options defined in BACKEND.  Optionally also use
EXTRA-OPTIONS, which override the backend options.  You can also
pass overriding values for the header and footer (using the
keywords :header or :footer)."
  (with-current-buffer buf
    (let* ((options (append extra-options (delve-export-backend-options backend)))
           (header  (delve-export--get-slot-option backend 'header :header
                                                   delve-objects options))
           (footer  (delve-export--get-slot-option backend 'footer :footer
                                                   delve-objects options))
           (newline (delve-export--get-slot-option backend 'newline :separator
                                                   delve-objects options))
           (options (append (list
                             :n-total (length delve-objects)
                             :separator newline
                             :header header
                             :footer footer)
                            options)))
      ;;
      (when header (insert (concat header newline)))
      (when delve-objects
        (let ((last-n (1- (length delve-objects))))
          (--each-indexed delve-objects
            (delve-export--insert-item backend it
                                       (-> options
                                           (plist-put :index it-index)
                                           (plist-put :last  (eq it-index last-n)))))))
      (when footer (insert (concat footer newline))))))

;; * Export to Org Links

(defvar delve-export--backend-for-yanking
  (delve-export-backend-create
   :name 'yank-into-org
   :header nil
   :footer nil
   :options nil
   :separator "\n"
   :printers `((delve--note    . ,(lambda (n _) (delve--note-text n)))
               (delve--heading . ,(lambda (h _) (concat "*" (delve--heading-text h))))
               (delve--info    . ,(lambda (i _) (delve--info-text i)))
               (delve--zettel  . ,(lambda (z _)
                                    (org-link-make-string
                                     (delve--zettel-file z)
                                     (delve--zettel-title z))))))
    "Backend for exporting Delve items to simple Org mode links.")

(provide 'delve-export)
;;; delve-export.el ends here
