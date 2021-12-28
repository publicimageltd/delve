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
           (slots (-difference (mapcar #'car (cdr (cl-struct-slot-info type))) exclude))
           (res   nil))
      (cl-dolist (slot slots)
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
  (--reduce-from
   (if-let ((val (plist-get options it)))
       (plist-put acc it (delve-export--value-or-fn (plist-get options it) acc))
     acc)
   options
   keys))

(defun delve-export--item-string (object options)
  "Return the printed representation for OBJECT in backend OPTIONS.
Return the results of calling the printer function with two
arguments, the Delve OBJECT to be printed and a property list of
OPTIONS.  Look up the printer function in the alist associated
with the property `:printers', using the object's type.

Do nothing if no printer is found or if the printer function
returns nil."
  (when object
    (when-let ((printer (alist-get (type-of object)
                                   (plist-get options :printers))))
      (funcall printer object options))))

(defun delve-export--insert (buf backend delve-objects
                                 &optional extra-options)
  "Insert DELVE-OBJECTS into BUF using BACKEND.
Use the slot values defined in BACKEND for printing.  Override
these values if a key in the property list stored in `options'
matches a slot name (e.g. `:name' for slot `name').  Optionally
also use EXTRA-OPTIONS, which override any other options.  If
BACKEND is nil, assume that EXTRA-OPTIONS has all slot values.

Before inserting anything, call the function of the slot
`assert', if defined (symbol or lambda; no arguments).  Throw an
error if that function returns a non-nil value.

Insert header first, then the items, then the footer.  Print
items using the function defined in the backend slot `:printers'.
This slot holds an alist associating the item type with a
function accepting two arguments, the object itself and a
property list.  For that property list, merge the backend slot
values and any additional options, with slot names converted to
property keys (e.g. slot `name' is mapped to the property
`:name').  Thus, the whole backend is available by that list.  If
the printer function returns a string value, insert it, else
ignore the item.

Add separator in between any items or between items and header or
footer, if defined.

If the values for header, footer and separator hold a function
name or object, use the value returned by that function.  This
function, too, is called with the full list of options, including
the values for the backend slots."
  (with-current-buffer buf
    (let* ((n       (length delve-objects))
           ;; merge everything into a big plist:
           (options  (delve-export--merge-plists
                      (delve-export--struct-to-plist backend)
                      (when backend (delve-export-backend-options backend))
                      extra-options
                      (list :n-total n))))

      (if (and (plist-get options :assert)
               (not (funcall (plist-get options :assert))))
          (error "Backend %s: assertion failed, cannot export" (plist-get options :name))

        ;; process special slots where fns might produce the final value:
        (let* ((options (delve-export--process-special-values options :header :footer :separator))
               (header  (plist-get options :header))
               (footer  (plist-get options :footer))
               (sep     (plist-get options :separator)))

          ;; print it:
          (when header (insert (concat header
                                       (when (or delve-objects footer) sep))))
          (when delve-objects
            ;; NOTE this should be faster than calling string-join
            ;; cf. https://nullprogram.com/blog/2014/05/27/
            (let ((counter 1))
              (cl-dolist (obj delve-objects)
                (insert (concat (delve-export--item-string obj options)
                                (unless (eq counter n) sep)))
                (cl-incf counter))))
          (when footer (insert (concat (when (or delve-objects footer) sep)
                                       footer))))))))

;; * Export to Org Links

(defun delve-export--zettel-to-link (z &optional args)
  "Return zettel Z as an Org link pointing to its headline.
Optional argument ARGS is ignored."
  (ignore args)
  (org-link-make-string (concat "id:" (delve--zettel-id z))
                        (delve--zettel-title z)))

(defvar delve-export--backend-for-yanking
  (delve-export-backend-create
   :assert (lambda () (derived-mode-p 'org-mode))
   :name 'yank-into-org
   :header nil
   ;; this is still a yank handler:
   :footer (lambda (o) (when (> (plist-get o :n-total) 1) ""))
   :options nil
   :separator "\n"
   :printers `((delve--pile    . ,(lambda (p o)
                                    ;; NOTE This is not so elegant,
                                    ;; but it works.  Alternatively,
                                    ;; we could check the return value
                                    ;; of `delve-export--item-string'
                                    ;; and recursively postprocess
                                    ;; returned lists as further
                                    ;; items. But, well....
                                    (concat
                                     (string-join (--map (delve-export--item-string it o)
                                                         (cons (delve--heading-create
                                                                :text (delve--pile-name p))
                                                               (delve--pile-zettels p)))
                                                  (plist-get o :separator))
                                     (plist-get o :separator))))
               (delve--note    . ,(lambda (n _) (delve--note-text n)))
               (delve--heading . ,(lambda (h _) (concat "* " (delve--heading-text h))))
               (delve--info    . ,(lambda (i _) (delve--info-text i)))
               (delve--zettel  . ,(lambda (z _) (format "#+transclude: [[%s][%s]]"
                                                        (delve--zettel-id z)
                                                        (delve--zettel-title z))))))
  "Backend for exporting Delve items to simple Org mode links.")

(defvar delve-export--backend-transclusion
  (delve-export-backend-create
   :assert (lambda () (derived-mode-p 'org-mode))
   :name 'transclusion
   :header nil
   :footer (lambda (o) (when (> (plist-get o :n-total) 1) ""))
   :options nil
   :separator "\n"
   :printers `((delve--pile    . ,(lambda (p o)
                                    (concat
                                     (string-join (--map (delve-export--item-string it o)
                                                         (cons (delve--heading-create
                                                                :text (delve--pile-name p))
                                                               (delve--pile-zettels p)))
                                                  (plist-get o :separator))
                                     (plist-get o :separator))))
               (delve--note    . ,(lambda (n _) (delve--note-text n)))
               (delve--heading . ,(lambda (h _) (concat "* " (delve--heading-text h))))
               (delve--info    . ,(lambda (i _) (delve--info-text i)))
               (delve--zettel  . ,#'delve-export--zettel-to-link)))
   "Backend for integration with org-transclusion.")

(defvar delve-export--yank-handlers
  (list delve-export--backend-for-yanking
        delve-export--backend-transclusion)
  "List of available handlers for yanking.")

(provide 'delve-export)
;;; delve-export.el ends here
