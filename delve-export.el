;;; delve-export.el --- Export capabilities for Delve  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023

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
Slot NAME is a name (symbol) for the backend; DESCRIPTION offers
a description for the user (e.g. when selecting a backend).

The value of the slot PARENT denotes another backend instance by
name.  (To be found by name, the instance has to be stored in the
variable `delve-export--backends'.)  When initalizing a backend,
it takes in all values (slots) from a parent instance, if given.
If the parent instance has another parent, values are taken first
from that top parent instance, then overriden by the child
instance, and so on.  In that way, a backend specializes by
inheriting its basic functionality from parent backend(s) and by
setting only some slots on its own.

When inheriting, all slots are overriden by the child instance if
the latter has a non-nil value.  An exception to that rule is the
slot `printers', which is merged with previous values,
effectively allowing the child backend to overwrite only
particular functions instead of the whole set of printers.

Slots HEADER, FOOTER and SEPARATOR are either strings, which are
inserted as is, or functions returning a value to be
inserted. The functions are called with a list of options. If
SEPARATOR is non-nil, its value will be inserted between each
item (including the header and the footer).

Slot PRINTERS is an alist, associating each Delve object type
with a printer function.  Each printer function is called with
the object and a property list of options. Apart from extra
options which can be passed programmatically when calling the
export function, this property list also contains a complete copy
of the backend's slots, each slot name corresponding to a
keyword.  E.g. the property `:separator' contains the value of
the slot `separator'.  If the slot PARENT is non-nil, inheritance
rules apply.  Special slots which accept both a value and a
function are finalized before passing them to the printer
function."
  assert parent name description printers header footer separator)

;; * Global Variables

(defvar delve-export--backends nil
  "Internal list holding all export backends.")

;; * Utilities to mimic some kind of inheritance

(defun delve-export--merge-plist (plist1 plist2 &optional merge-alist)
  "Merge PLIST2 into PLIST1, overwriting the latter's values.
Instead of overwriting the value, optionally use the fn
associated with the key in MERGE-ALIST to construct the merged
value (e.g. `((:key . append))' to use the append function).  That
function will be called with two arguments, the value from PLIST1
and the value from PLIST2."
  (unless (eq 0 (mod (length plist2) 2))
    (error "Malformed property list: %S" plist2))
  (let* ((target  (copy-sequence plist1))
         (from    (copy-sequence plist2)))
    (while from
      (pcase-let ((`(,key ,val _) from))
        (let ((merged-val (if-let ((fn (alist-get key merge-alist)))
                              (funcall fn (plist-get target key) val)
                            val)))
        (setq target  (plist-put target key merged-val)
              from    (cdr (cdr from))))))
    target))

(defun delve-export--merge-plists (merge-alist plist1 &rest plists)
  "Merge all PLISTS into PLIST1, overriding PLIST1's values.
Optionally specify special ways of merging (instead of simply
overriding the value) by associating the merging function with a
key in MERGE-ALIST, e.g. `((:key . append))'."
  (--reduce-from (delve-export--merge-plist acc it merge-alist) plist1 plists))

(defun delve-export--merge-alist (alist1 alist2)
  "Merge ALIST2 into ALIST1, overriding the latter's values.
Key comparison is done with `eq'."
  (let ((target (copy-sequence alist1)))
    (pcase-dolist (`(,key . ,val) alist2)
      (setq target
            ;; I tried first (setf (alist-get ....)), but
            ;; that destroyed the original alist!
            (if-let ((old-val (alist-get key target)))
                (-replace `(,key . ,old-val)
                          `(,key . ,val)
                          target)
              (append target `((,key . ,val))))))
    target))

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

(defun delve-export--eq-name (name instance)
  "Check if Delve export backend INSTANCE has the name NAME."
  (when instance
    (eq (delve-export-backend-name instance) name)))

(defun delve-export--get-backend-by-name (name all-backends)
  "Return instance with name matching NAME.
Search for instances in the list ALL-BACKENDS."
  (-find (-partial #'delve-export--eq-name name) all-backends))

(defun delve-export--get-parent (instance all-backends)
  "Return parent backend of INSTANCE, or nil.
Search for parent instances in the list ALL-BACKENDS (using the
name)."
  (and instance
       (when-let* ((parent (delve-export-backend-parent instance)))
         (delve-export--get-backend-by-name parent all-backends))))

(defun delve-export--get-parent-backends (instance all-backends)
  "Return a list of all parents of INSTANCE.
Search for parent instances in the list ALL-BACKENDS (using the
  name)."
  (let ((child instance) res)
    (while (setq child (delve-export--get-parent child all-backends))
      (push child res))
    res))

(defun delve-export--backend-as-plist (instance all-backends)
  "Return backend INSTANCE as plist using inheritance.
Search for parent instances in the list ALL-BACKENDS (using the
name).  When ALL-BACKENDS is nil, return INSTANCE as a property
list without any modifications."
  (let* ((trail  (cons instance (delve-export--get-parent-backends instance all-backends)))
         (plists (-map #'delve-export--struct-to-plist (reverse trail)))
         (merge-alist `((:printers . ,#'delve-export--merge-alist))))
    (--reduce-from (delve-export--merge-plist acc it merge-alist)
                   (car plists)
                   (cdr plists))))

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
Leave the values associated with KEYS unchanged unless they hold
a function object or a symbol pointing to a function.  In that
latter case, replace the value with the result of calling this
function with OPTIONS as its argument."
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

(defun delve-export--debug ()
  "ARGH.  Eval buffer, dann das in *ielm*."
  (let ((orig (--map (alist-get 'delve--zettel it) (mapcar #'delve-export-backend-printers delve-export--backends))))
    (delve-export--backend-as-plist (delve-export--get-backend-by-name 'org-transclusion delve-export--backends)
                                    delve-export--backends)
    (list :orig orig
          :neu (--map (alist-get 'delve--zettel it) (mapcar #'delve-export-backend-printers delve-export--backends)))))
  
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
                      `((:printers . ,#'delve-export--merge-alist))
                      ;; begin with the backend (with its inherited
                      ;; values)
                      ;; BUG Hier ist der Fehler!
                      ;; siehe delve-export--debug
                      (delve-export--backend-as-plist backend delve-export--backends)
                      ;; pass extra-options from this function call
                      extra-options
                      ;; and finally some values for printing
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

;; * Easier handling of the global backend list

(defun delve-export--unregister-backend (name)
  "Remove backend NAME from the variable `delve-export--backends'."
  (setq delve-export--backends (--remove (delve-export--eq-name name it)
                                         delve-export--backends)))

(defun delve-export--register-backend (instance)
  "Add backend INSTANCE to the list stored in `delve-export--backends'.
If an instance with the same name is found, replace it."
  (delve-export--unregister-backend (delve-export-backend-name instance))
  (setq delve-export--backends
        (append delve-export--backends (list instance))))

(defmacro delve-export-new-backend (name description &rest keyword-value-pairs)
  "Define and register a new export backend called NAME.
Pass NAME, DESCRIPTION and KEYWORD-VALUE-PAIRS to
`delve-export-backend-create' and add the thusly created backend
to the internal variable `delve-export--backends'.

For a list of possible keywords, see
`delve-export-backend-create'."
  (declare (indent 1))
  `(delve-export--register-backend
    (delve-export-backend-create :name ,name :description ,description ,@keyword-value-pairs)))

;; * Functions to be used by the export backends

(defun delve-export--zettel-to-link (z &optional args)
  "Return zettel Z as an Org link pointing to its headline.
Optional argument ARGS is ignored."
  (ignore args)
  (org-link-make-string (concat "id:" (delve--zettel-id z))
                        (delve--zettel-title z)))

;; * Define the actual backends

(delve-export-new-backend 'yank-into-org
  "Insert Delve items as simple Org mode links"
  :assert (lambda () (derived-mode-p 'org-mode))
  :footer (lambda (o) (when (> (plist-get o :n-total) 1) ""))
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

;; NOTE This is still experimental and not enabled by default.
(delve-export-new-backend 'org-transclusion
  "Print Delve zettels as links suitable for org-transclusion"
   :assert (lambda () (and (derived-mode-p 'org-mode)
                           (featurep 'org-transclusion)
                           (boundp 'org-transclusion-mode)
                           org-transclusion-mode))
   :parent 'yank-into-org
   :printers `((delve--zettel  . ,(lambda (z _) (format "#+transclude:  [[id:%s][%s]]"
                                                        (delve--zettel-id z)
                                                        (delve--zettel-title z))))))

(defvar  delve-export--yank-handlers
  (list 'yank-into-org)
  "List of available backends for yanking (by name).
When yanking, check which of these backends can be used in the
current buffer by calling its `assert' function.  If there are
multiple options available, let the user choose the proper
backend.")

(defun delve-export--available-backends (&optional backend-names)
  "Get all export backends available for the current buffer.
Return those backends from `delve-export--backends' for which the
`assert' function returns a non-nil value, or for which no such
function is defined.  Optionally limit check to those backends
with a name in BACKEND-NAMES."
  (--filter (if-let ((assert-fn (delve-export-backend-assert it)))
                (funcall assert-fn)
              t)
            (if backend-names
                (-non-nil
                 (--map (delve-export--get-backend-by-name it delve-export--backends)
                        backend-names))
              delve-export--backends)))

(defun delve-export--select-backend (backends)
  "Let the user select a backend from BACKENDS.
Skip selection if there is only one backend to choose from."
  (let* ((candidates (-group-by #'delve-export-backend-name backends)))
    (if (eq 1 (length candidates))
         (car (cdr (car candidates)))
      (car (alist-get (intern (completing-read "Select insertion format: " candidates)) candidates)))))

(provide 'delve-export)
;;; delve-export.el ends here
