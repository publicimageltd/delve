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

;;; * Define backend type and associated functions

(cl-defstruct (delve-export-backend (:constructor delve-export-backend-create))
  "A backend for exporting Delve objects.
Slot NAME is a name (symbol) for the backend.

OPTIONS holds a property list of options, which are passed to
each exporting function in addition to the values of the backend
instance.

Slots HEADER, FOOTER and SEPARATOR are either strings, which are
inserted as is, or functions returning a value to be
inserted. The functions are called with a list of options.

SEPARATOR can be nil, a string or a function with two
arguments (object and options) returning a string. If SEPARATOR is
non-nil, it will be inserted between each item (including
the header and the footer).

Slot PRINTERS is an alist, associating each Delve object type
with a printer function.  Each printer function is called with
the object and a property list of options. Apart from extra
options which can be passed programmatically and the values from
the slot OPTIONS, this property list also contains a complete
copy of the backend's slots, each slot name corresponding to a
keyword.  E.g. the property `:separator' contains the value of
the slot `separator'.  Special slots which accept both a
value and a function are finalized before passing them to the
printer function."
  assert name printers header footer separator options)

(defun delve-export--struct-to-plist (instance &optional exclude)
  "Return all slot-value-pairs of struct INSTANCE as a plist.
Exlude all slots from EXCLUDE."
  (when instance
    (let* ((type  (type-of instance))
           (slots (-difference (cl-struct-slot-info type) exclude))
           (res   nil))
      (pcase-dolist (`(,slot . _) (cdr slots))
        (setq res (plist-put res
                             (intern (format ":%s" slot))
                             (cl-struct-slot-value type slot instance))))
      res)))

(defun delve-export--merge-plist (plist1 plist2)
  "Merge PLIST2 into PLIST1, overwriting the latter's values."
  (unless (eq 0 (mod (length plist2) 2))
    (error "Malformed property list: %S" plist2))
  (let ((res (copy-sequence plist1)))
    (while plist2
      (pcase-let ((`(,key ,val _) plist2))
        (setq res (plist-put res key val)
              plist2 (cdr (cdr plist2)))))
    res))

(defun delve-export--merge-plists (plist1 &rest plists)
  "Merge all PLISTS into PLIST1, overriding PLIST1's values."
  (-reduce-from #'delve-export--merge-plist plist1 plists))

(defun delve-export--value-or-fn (value &rest args)
  "Return VALUE unchanged or call it as a function with ARGS."
  (when value
    (pcase value
      ((or (pred functionp)
           (and (pred consp)
                (app car 'closure)))
       (apply value args))
      (_    value))))

(defun delve-export--process-special-values (options &rest keys)
  "Return OPTIONS with the values for KEYS processed in a special way.
Leave the associated values unchanged unless they hold a function
object or a symbol pointing to a function.  In that latter case,
replace the value with the result of calling this function with
OPTIONS as its argument."
  (--reduce-from (plist-put acc it
                            (delve-export--value-or-fn (plist-get options it) acc))
                 options
                 keys))

;; This is the main workhorse for exporting.

;; FIXME Current design is rather inconsequential: The printer return
;; string values, but recursion is impossible since everything is
;; inserted directly (instead of accumulating it).
(defun delve-export--insert-item (object options)
  "Print OBJECT in current buffer using the printer defined in OPTIONS.
Insert the results of calling the printer function with two
arguments, the Delve OBJECT to be printed and a property list of
OPTIONS.  If the options property `:separator' is non-nil,
additionally also insert its associated value between items.

Do nothing if no printer is found or if the printer function
returns nil."
  (when-let ((printer (alist-get (type-of object)
                                 (plist-get options :printers))))
    (when-let ((item (funcall printer object options)))
      (insert item)
      (when-let ((newline (plist-get options :separator)))
        (unless (and (plist-get options :last)
                     (not (plist-get options :header))
                     (not (eq (type-of object) 'delve--pile)))
          (insert newline))))))
  
(defun delve-export--insert (buf backend delve-objects
                                 &optional extra-options)
  "Insert DELVE-OBJECTS into BUF using BACKEND.
Use the options and slot values defined in BACKEND.  Optionally
also use EXTRA-OPTIONS, which override the backend options.  If
BACKEND is nil, assume that EXTRA-OPTIONS has all slot values."
  (with-current-buffer buf
    (let* ((n       (length delve-objects))
           ;; merge everything into a big plist:
           (options  (delve-export--merge-plists
                      (delve-export--struct-to-plist backend)
                      (when backend (delve-export-backend-options backend))
                      extra-options
                      (list :n-total n))))
      (if (not (delve-export--value-or-fn (plist-get options :assert) options))
          (error "Backend %s: assertion failed, cannot export" (delve-export-backend-name backend))

        ;; process special slots where fns might produce the final value:
        (let* ((options (delve-export--process-special-values options :header :footer :separator))
               (header  (plist-get options :header))
               (footer  (plist-get options :footer))
               (sep     (plist-get options :separator)))
          
            ;; print it:
            (when header (insert (concat header sep)))
            (when delve-objects
              (let ((last-n (1- n)))
                (--each-indexed delve-objects
                  (delve-export--insert-item it
                                             (delve-export--merge-plists options
                                                                         (list :index it-index
                                                                               :last (eq it-index last-n)))))))
            (when footer (insert (concat footer sep))))))))

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
                                    (delve-export--insert (current-buffer)
                                                          nil
                                                          (cons (delve--heading-create :text
                                                                                       (delve--pile-name p))
                                                                (delve--pile-zettels p))
                                                          o)
                                    nil))
               (delve--note    . ,(lambda (n _) (delve--note-text n)))
               (delve--heading . ,(lambda (h _) (concat "* " (delve--heading-text h))))
               (delve--info    . ,(lambda (i _) (delve--info-text i)))
               (delve--zettel  . ,#'delve-export--zettel-to-link)))
    "Backend for exporting Delve items to simple Org mode links.")

(provide 'delve-export)
;;; delve-export.el ends here
