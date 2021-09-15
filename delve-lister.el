;;; delve-lister.el --- Yet another list printer             -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2021

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

;; This is an outdated copy of the lister library.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'cursor-sensor)

;; -----------------------------------------------------------
;; * Variables
;; -----------------------------------------------------------

;; * Version:

(defvar delve-lister-version "0.7.1"
  "Version number.")

;; * Local Variables:

(defvar-local delve-lister-local-mapper nil
  "Function to convert any DATA object to a list of strings.")

(defvar-local delve-lister-local-filter-fn nil
  "Local filter function for Lister items.
All items for which this functions returns nil will be hidden.

Do not set this variable directly; use `delve-lister-set-filter'
instead.")

(defvar-local delve-lister-local-marking-predicate nil
  "Function which decides if an item can be marked.
If nil, any item can be marked.

The function has to be called with one argument: the associated
data. If the function returns non-nil, the item can be marked.")

(defvar-local delve-lister-local-header-marker nil
  "Stores the marker for the upper left corner of the header.")

(defvar-local delve-lister-local-footer-marker nil
  "Stores the marker for the upper left corner of the footer.")

(defvar-local delve-lister-local-marker-list nil
  "Stores a list of marker positions for each lister list item.")

(defvar-local delve-lister-local-left-margin 2
  "Add this left margin when inserting a item.
Set this to nil if no left margin is wanted.")

(defvar-local delve-lister-enter-item-hook nil
  "List of functions to call when point enters an existing item.
Use `delve-lister-add-enter-callback' to add a function to this buffer
local hook. Do not use `add-hook'.

When the callback function is called, the lister buffer is set
current and point is on the current item. Use `delve-lister-get-data'
to access the data.

To avoid recursion, `cursor-sensor-inhibit' is set to `t' during
the execution of the callback functions.")

(defvar-local delve-lister-leave-item-hook nil
  "List of functions to call when point leaves an existing item.

When the callback function is called, the lister buffer is set
current and point is on the current item. Use `delve-lister-get-data'
to access the data.

To avoid recursion, `cursor-sensor-inhibit' is set to `t' during
the execution of the callback functions.

Use `delve-lister-add-leave-callback' to add a function to this buffer
local hook.")

(defvar-local delve-lister-sensor-last-item nil
  "Last item on which the sensor function has been applied.")

;; * Global Variables:

(defvar delve-lister-inhibit-cursor-action nil
  "Bind this to inhibit updating the cursor while inserting items.")

(defvar delve-lister-inhibit-marker-list nil
  "Bind this to inhibit updating the marker list while inserting items.")

(defvar delve-lister-cursor-locked nil
  "Internal lock to avoid duplicate calls of `delve-lister-with-locked-cursor'.
Do not set this variable.")

;; * Customizable Global Variables:

(defcustom delve-lister-mark-face-or-property
  '(:background "darkorange3"
                :foreground "white")
  "Text properties to be added when highlighting marked items.
Possible values are either a plist of face attributes or the name
of a face."
  :group 'lister
  :type '(choice (face :tag "Name of a face")
                 (plist :tag "Plist of face attributes")))

;; -----------------------------------------------------------
;; * Side-effect free utilities

(defun delve-lister--wrap-list (l)
  "Wrap all items in L in a list, with sub-lists as its cdr."
  (declare (pure t) (side-effect-free t))
  (let (acc (walk l))
    (while
        (let ((current (car walk))
              (next    (cadr walk)))
          (unless (consp current)
            (push (cons current (when (consp next)
                                  (delve-lister--wrap-list next)))
                  acc))
          (setq walk (cdr walk))))
    (nreverse acc)))

(defun delve-lister--reorder-wrapped-list (l fn)
  "Destructively reorder L using FN, keeping sublist structure.
L has to be a wrapped list as returned by `delve-lister--wrap-list',
consisting of a cons cell with the actual item as the car and its
associated sublist as its cdr.

Note that FN has to reorder a wrapped list, not a plain list.  Its
result must not undo the wrapping.

Return L in new order."
  (declare (pure t) (side-effect-free t))
  (let (acc (walk  (funcall fn l)))
    ;; test at the beginning to ensure that walk is not already empty
    (while walk
      (let ((item    (caar walk))
            (sublist (cdar walk)))
        (push item acc)
        (when (consp sublist)
          (push (delve-lister--reorder-wrapped-list sublist fn) acc))
        (setq walk (cdr walk))))
    (nreverse acc)))

;; -----------------------------------------------------------
;; * Working with text properties

(defun delve-lister-add-face-property (beg end value &optional append)
  "Add VALUE to the face property between BEG and END."
  (add-face-text-property beg end value append))

(defun delve-lister-remove-face-property (beg end value)
  "Remove VALUE from the face property from BEG to END.
This is a slightly modified copy of `font-lock--remove-face-from-text-property'."
  (let ((beg (text-property-not-all beg end 'face nil))
        next prev)
    (while beg
      (setq next (next-single-property-change beg 'face nil end)
            prev (get-text-property beg 'face))
      (cond ((or (atom prev)
                 (keywordp (car prev))
                 (eq (car prev) 'foreground-color)
                 (eq (car prev) 'background-color))
             (when (eq value prev)
               (remove-list-of-text-properties beg next (list 'face))))
            ((memq value prev)		;Assume prev is not dotted.
             (let ((new (remq value prev)))
               (cond ((null new)
                      (remove-list-of-text-properties beg next (list 'face)))
                     ((= (length new) 1)
                      (put-text-property beg next 'face (car new)))
                     (t
                      (put-text-property beg next 'face new))))))
      (setq beg (text-property-not-all next end 'face nil)))))

(defun delve-lister-add-props (buf pos-or-marker &rest props)
  "Add text properties PROPS at POS-OR-MARKER in BUF.
PROPS is a list of properties and values."
  (with-current-buffer buf
    (let* ((inhibit-read-only t)
           (pos (delve-lister-pos-as-integer pos-or-marker)))
      (add-text-properties pos (1+ pos) props))))

(defun delve-lister-remove-props (buf pos-or-marker &rest props)
  "Remove PROPS from POS-OR-MARKER in BUF.
PROPS is a list of property-value-pairs, but only the property
field is used.  See `remove-text-properties', which is called."
  (when props
    (with-current-buffer buf
      (let* ((inhibit-read-only t)
             (pos (delve-lister-pos-as-integer pos-or-marker)))
        (remove-text-properties pos (1+ pos) props)))))

(defun delve-lister-get-prop (buf pos-or-marker prop)
  "In BUF, get VALUE of PROP at POS-OR-MARKER."
  (let* ((pos (delve-lister-pos-as-integer pos-or-marker)))
    (get-text-property pos prop buf)))

(defun delve-lister-get-props-at (buf pos &rest props)
  "Return the values of all PROPS at POS in BUF."
  (seq-map (apply-partially #'delve-lister-get-prop buf pos) props))

;; * Finding properties in other items

(defun delve-lister-looking-at-prop (lister-buf pos-or-marker prop direction)
  "Looking at the previous or next item, return position of PROP.
POS-OR-MARKER is the starting point to look from.  It can be an
integer position or a marker.  DIRECTION is either the symbol
`previous' or the symbol `next'.

Return the position of PROP in the indicated direction, or nil.

LISTER-BUF is a lister buffer."
  (pcase direction
    ('next     (unless (= pos-or-marker (point-max))
                 (next-single-property-change (1+ pos-or-marker) prop lister-buf)))
    ('previous (when-let ((res (previous-single-property-change pos-or-marker prop lister-buf)))
                 (1- res)))))

;; -----------------------------------------------------------
;; * Safety checks

(defun delve-lister-nonempty-p (lister-buf)
  "Return non-nil if LISTER-BUF has no single list item."
  (buffer-local-value 'delve-lister-local-marker-list lister-buf))

(defun delve-lister-item-p (lister-buf pos-or-symbol)
  "Check if POS-OR-SYMBOL points to a lister item in LISTER-BUF."
  ;; delve-lister-marker-at checks for text property 'item
  (not (null (delve-lister-marker-at lister-buf pos-or-symbol))))

(defun delve-lister-buffer-p (buf)
  "Return BUF if it is ready to be used for lister lists.
Throw an error if BUF is not in `lister mode' or a major mode
derived from it.  Also cancel if the local mapper function is not
defined."
  (unless buf
    (error "Expected buffer, got nil"))
  (with-current-buffer buf
    (or
     (and (derived-mode-p 'delve-lister-mode)
          delve-lister-local-mapper
          buf)
     (error
      (if (not (derived-mode-p 'delve-lister-mode))
          "Buffer %s has to be in lister mode; execution aborted."
        "Buffer %s has to have a local mapper function; execution aborted.")
      buf))))

(defmacro delve-lister-with-lister-buffer (buf &rest body)
  "Execute BODY in BUF.
Throw an error if BUF is not a lister buffer."
  (declare (indent 1) (debug t))
  `(with-current-buffer (delve-lister-buffer-p ,buf)
     ,@body))

;; -----------------------------------------------------------
;; * Utilities for markers and positions

;; Build marker, convert positions

(defun delve-lister-make-marker (buf pos)
  "Create a suitable marker for POS in lister buffer BUF."
  (let ((marker (make-marker)))
    (set-marker marker pos buf)
    (set-marker-insertion-type marker t)
    marker))

(defun delve-lister-pos-as-integer (marker-or-pos)
  "Get the integer value from MARKER-OR-POS."
  (if (markerp marker-or-pos)
      (marker-position marker-or-pos)
    marker-or-pos))

(defun delve-lister-pos-as-marker (lister-buf marker-or-pos)
  "Return the marker MARKER-OR-POS or create one.
LISTER-BUF is a lister buffer."
  (if (markerp marker-or-pos)
      marker-or-pos
    (delve-lister-make-marker lister-buf marker-or-pos)))

;; Add marker to the buffer local marker list

(cl-defun delve-lister--list-pos-in (l new-value &optional (n 0))
  "Find index position to insert NEW-VALUE in an ordered list.
L has to be a list of numbers or markers sorted in ascending
order.  The return value is the zero-based index position of L,
pointing to an item which is either equal or smaller than
NEW-VALUE."
  (let* ((max    (length l))
         (middle (/ max 2))
         (cand   (elt l middle)))
    (if (= 0 middle)
        (if (<= new-value cand) n (1+ n))
      (if (<= new-value cand)
          (delve-lister--list-pos-in (butlast l (- max middle)) new-value n)
        (delve-lister--list-pos-in (nthcdr middle l) new-value (+ n middle))))))

(defun delve-lister--list-insert-at (target-list l n)
  "Destructively insert list L into TARGET-LIST at position N.
This function returns the modified list, but also modifies
TARGET-LIST."
  ;; https://stackoverflow.com/questions/20821295/how-can-i-insert-into-the-middle-of-a-list-in-elisp
  (let* ((padded-list (cons nil target-list))
         (tail        (nthcdr n padded-list)))
    (cl-loop for el in (reverse l)
             do
             (setcdr tail (cons el (cdr tail))))
    (cdr padded-list)))

(defun delve-lister-add-item-marker (lister-buf marker-or-pos)
  "Add MARKER-OR-POS, or a list of these, to LISTER-BUF.
Do nothing if `delve-lister-inhibit-marker-list' is t.

MARKER-OR-POS, if a list, has to sorted in ascending order."
  (unless (or delve-lister-inhibit-marker-list
              (not marker-or-pos))
    ;; transform marker-or-pos to a list of markers:
    (let ((m-list (mapcar (apply-partially #'delve-lister-make-marker lister-buf)
                          (if (listp marker-or-pos)
                              marker-or-pos
                            (list marker-or-pos)))))
      ;; insert list in buffer local list:
      (with-current-buffer lister-buf
        (setq delve-lister-local-marker-list
              (if (null delve-lister-local-marker-list)
                  m-list
                (delve-lister--list-insert-at delve-lister-local-marker-list
                                        m-list
                                        (delve-lister--list-pos-in delve-lister-local-marker-list (car m-list)))))))))

;; Finding positions

(defun delve-lister-eval-pos-or-symbol (lister-buf position-or-symbol)
  "Return a marker position evaluating POSITION-OR-SYMBOL.
POSITION-OR-SYMBOL can itself be a marker, or an integer, or the
symbols `:first', `:last' or `:point'.

LISTER-BUF must be a set up lister buffer.

Note that this function only interpretes POSITION-OR-SYMBOL.  It
does not check whether the position found is valid."
  (let* ((pos
          (cond
           ;; the two most likely use cases first:
           ((markerp position-or-symbol)    position-or-symbol)
           ((integerp position-or-symbol)   position-or-symbol)
           ;; now the keyword cases:
           ((eq position-or-symbol :first)  (delve-lister-item-min lister-buf))
           ((eq position-or-symbol :point)  (with-current-buffer lister-buf (point)))
           ;; FIXME Quicker: Look backwards from item-max, iff list is nonempty.
           ((eq position-or-symbol :last)   (or (car (reverse (with-current-buffer lister-buf delve-lister-local-marker-list)))
                                                (delve-lister-item-max lister-buf)))
           (t
            (error "Unknown value for POSITION-OR-SYMBOL: %s"
                   position-or-symbol)))))
    (and pos
         (delve-lister-pos-as-marker lister-buf pos))))

(defun delve-lister-marker-at (lister-buf position-or-symbol)
  "In LISTER-BUF, return marker according to POSITION-OR-SYMBOL.
Return nil if there is no item at the desired position.

If POSITION-OR-SYMBOL is one of the symbols `:first', `:last' or
`:point', return the position of the first item, the last item or
the item at point, respectively.

If POSITION-OR-SYMBOL is a marker, return it unchanged iff there
is an item.

If POSITION-OR-SYMBOL is an integer, treat it as a buffer
position and return a marker representing it iff there is an
item.

Throw an error if POSITION-OR-SYMBOL has an invalid value."
  (let (m)
    (and (setq m (delve-lister-eval-pos-or-symbol lister-buf position-or-symbol))
         (get-text-property (delve-lister-pos-as-integer m) 'item lister-buf)
         m)))

(defun delve-lister-item-min (lister-buf)
  "Return the first position for a list item in LISTER-BUF.
This is intended to be analogous to `point-min', but restricted
to the list items.  Deleting everything between `delve-lister-item-min'
and `delve-lister-item-max' would delete all items and keep header and
footer."
  (delve-lister-with-lister-buffer lister-buf
    (if delve-lister-local-header-marker
        (delve-lister-end-of-lines lister-buf delve-lister-local-header-marker)
      (point-min))))

(defun delve-lister-item-max (lister-buf)
  "Return the first non-item-position after the list in LISTER-BUF.
This is intended to be analogous to `point-max', but restricted
to the list items.  Deleting everything between `delve-lister-item-min'
and `delve-lister-item-max' would delete all items and keep header and
footer."
  (with-current-buffer lister-buf
    (if delve-lister-local-footer-marker
        (marker-position delve-lister-local-footer-marker)
      (point-max))))

(defun delve-lister-index-position (lister-buf marker-or-pos)
  "Get index position (starting with 0) of the item at MARKER-OR-POS.
Return nil if MARKER-OR-POS in LISTER-BUF is not pointing to an
item."
  (cl-position marker-or-pos
               (buffer-local-value 'delve-lister-local-marker-list lister-buf)
               :test #'=))

(defun delve-lister-index-marker (lister-buf index-position)
  "Get the marker for INDEX-POSITION (starting with 0) in LISTER-BUF.
Return nil if no such position is available."
  (elt (buffer-local-value 'delve-lister-local-marker-list lister-buf)
       index-position))

(cl-defun delve-lister-rescan-item-markers (lister-buf &optional (prop 'item))
  "Get a freshly build list of all item markers in LISTER-BUF.
Items are identified by checking the property PROP.  PROP defaults
to `item', meaning that this function matches all regular items."
  (with-current-buffer lister-buf
    (let (res (pos (point-min)) (max (point-max)))
      (while (< pos max)
        (when (get-text-property pos prop)
          (push (delve-lister-make-marker lister-buf pos) res)
          (setq pos (next-single-property-change pos prop nil max)))
        (setq pos (next-single-char-property-change pos prop nil max)))
      (reverse res))))

(defun delve-lister-normalize-position (buf pos pos-keyword)
  "Normalize POS according to POS-KEYWORD.
BUF has to be a lister buffer."
  (or (when pos
        (delve-lister-marker-at buf pos))
      (delve-lister-marker-at buf pos-keyword)))

;; TODO Add function which returns a subsequence of items for the
;; normalized region. Two possible methods; see "delve-lister-items-in-region"
(defmacro delve-lister-with-normalized-region (buf first last &rest body)
  "Execute BODY with FIRST and LAST as normalized list boundaries.

FIRST and LAST have to be symbols pointing to items.  If either
symbol's value is nil, or if there is no item at the position
indicated, bind it to the first resp.  last item of the list for
the duration of the macro.

Execute BODY only if the list is not empty.

BUF is a lister buffer."
  (declare (indent 3) (debug (sexp sexp sexp body)))
  (let ((buffer-var      (make-symbol "buffer")))
    `(let ((,buffer-var ,buf))
       (when-let ((,first (delve-lister-normalize-position ,buffer-var ,first :first))
                  (,last  (delve-lister-normalize-position ,buffer-var ,last  :last)))
         ,@body))))

(defun delve-lister-items-in-region (lister-buf first last)
  "Get all item markers from FIRST to LAST.
FIRST or LAST have to be item buffer positions.  If FIRST or LAST
is nil, use the first or last item of the whole list as the
respective boundary.  LISTER-BUF must be a lister buffer;"
  (let ((mlist (buffer-local-value 'delve-lister-local-marker-list lister-buf)))
    (if (and (null first) (null last))
        mlist
      (delve-lister-with-normalized-region lister-buf first last
        ;; FIXME Here we use indices, in delve-lister--delete-region, we use a
        ;; custom function to determine whether marker is within the
        ;; boundaries. Which one is to prefer?
        (seq-subseq mlist
                    (delve-lister-index-position lister-buf first)
                    ;; last or nil
                    (when last
                      ;; the manual of seq-subseq says 'end is the last
                      ;; item', the docstring says 'end is exclusive'.
                      ;; The docstring is right.
                      (1+ (delve-lister-index-position lister-buf last))))))))

;; -----------------------------------------------------------
;; * MACRO Lock cursor during longer transactions:

(defun delve-lister-set-visual-line (lister-buf line)
  "Visually set point to LINE.
LINE has to be a line numbered, counting from 0.
LISTER-BUF is a lister buffer."
  (let* ((m-list  (delve-lister-visible-items lister-buf))
         (m-len   (length m-list)))
    (with-current-buffer lister-buf
      (goto-char (if m-list
                     (elt m-list (if (< line m-len) line (1- m-len)))
                   (delve-lister-item-min lister-buf))))))

(defun delve-lister-get-visual-line (lister-buf &optional pos)
  "Get visual line number for POS.
POS is an integer position or marker.  If POS is nil, use point
instead.  LISTER-BUF is a lister buffer.  Return nil if buffer is
empty."
  (let ((m-list (delve-lister-visible-items lister-buf)))
    (cl-position (or pos (with-current-buffer lister-buf (point)))
                 m-list
                 :test #'=)))

(defmacro delve-lister-with-locked-cursor (buf &rest body)
  "Execute BODY and keep visual cursor position.
Return the result of BODY.  If this macro is called within the
BODY of this macro, just execute body directly.

BUF is a lister buffer.  Note that this macro does not make BUF
current."
  (declare (indent 1) (debug (sexp body)))
  (let ((buf-var  (make-symbol "buffer"))
        (line-var (make-symbol "line"))
        (res-var  (make-symbol "result")))

    `(let ((,buf-var  ,buf)
           ,line-var
           ,res-var)

       (unless delve-lister-cursor-locked
         (delve-lister-sensor-leave ,buf-var)
         (setq ,line-var (or (delve-lister-get-visual-line ,buf-var) 0)))

       (setq ,res-var
             (let ((delve-lister-cursor-locked t)
                   (delve-lister-inhibit-cursor-action t)
                   (cursor-sensor-inhibit t))
               ,@body))

       (unless delve-lister-cursor-locked
         (delve-lister-set-visual-line ,buf-var ,line-var)
         (delve-lister-sensor-enter ,buf-var))

       ,res-var)))

;; -----------------------------------------------------------
;; * Building the list using 'lines'

;; These are the core primitives. The following functions either
;; insert, remove or replace lines of text, usually passed to these
;; functions as a list of strings.

;; use this instead of flatten-tree for emacsen < 27.1:
(defun delve-lister--flatten (l)
  "Flatten the list L, removing any null values.
This is a simple copy of dash's `-flatten' using `seq'."
  (if (and (listp l) (listp (cdr l)))
      (seq-mapcat #'delve-lister--flatten l)
    (list l)))

(cl-defun delve-lister-strflat (l &optional (format-string "%s"))
  "Recursively stringify all items in L, flattening any sublists.
If L is not a list item, wrap it into a list.  Every non-nil item
will be passed to FORMAT-STRING, which defaults to \"%s\"."
  (mapcar (apply-partially #'format format-string)
          (delve-lister--flatten l)))

(cl-defun delve-lister-insert-lines (buf marker-or-pos lines level)
  "Insert flattened list LINES with padding LEVEL at POS in BUF.
MARKER-OR-POS can be either a marker object or a buffer position.
LINES must be a list.  LEVEL, an integer, adds extra padding to
the item (e.g. to mark it as a subitem).

If LINES is nil, do nothing.

Mark the inserted text as `intangible', but leave a gap for the
cursor to access the item.  Store some important values at the
position of the gap.  Move point to the end of the newly inserted
text.

If an item has been inserted, return the marker pointing to its
gap position."
  (when lines
    (with-current-buffer buf
      (let* ((padding           (make-string (+ (or delve-lister-local-left-margin 0) (or level 0)) ? ))
             (item-list         (delve-lister-strflat lines (concat padding "%s")))
             (beg               (delve-lister-pos-as-integer marker-or-pos))
             (inhibit-read-only t))
        (goto-char beg)
        ;; Mark the whole item except the newline character as being
        ;; 'intangible'. Assumes rear-stickiness.
        ;; Leaving newline out allows cursor movement:
        (insert (propertize (string-join item-list "\n")
                            'cursor-intangible t
                            'field t)
                "\n") ;; <- this leaves the "tangible" gap for the next item!
        ;;
        ;; Store some useful information at the beginning of the item,
        ;; which is also its "marker position" used to reference the
        ;; item.
        ;;
        ;; Calling `Delve-Lister-set-props' adds too much overhead, we add
        ;; the properties directly:
        (add-text-properties beg (1+ beg)
                             (list 'item t
                                   'level (or level 0)
                                   'nchars (- (point) beg)))
        (delve-lister-make-marker buf beg)))))

(defun delve-lister-remove-lines (buf marker-or-pos)
  "Remove the 'lines' element beginning at MARKER-OR-POS in BUF.
A 'lines' element can be the header, a list item or the footer."
  (with-current-buffer buf
    (let* ((inhibit-read-only t)
           (cursor-sensor-inhibit t))
      (delete-region marker-or-pos (delve-lister-end-of-lines buf marker-or-pos)))))

(defun delve-lister-replace-lines (buf marker-or-pos new-lines)
  "In BUF, Replace the 'lines' element at MARKER-OR-POS with NEW-LINES.
A 'lines' element can be the header, a list item or the footer.

If NEW-LINES is nil, delete the entry at MARKER-OR-POS.

Return the marker of the new item or nil if no item has been
inserted."
  (let ((level (get-text-property marker-or-pos 'level buf)))
    (delve-lister-remove-lines buf marker-or-pos)
    (delve-lister-insert-lines buf marker-or-pos new-lines level)))

;; FIXME Better name: delve-lister-next-lines? delve-lister-next-item?
;; delve-lister-after-lines? "End-of-lines" is unclear whether the "end" is
;; inclusive or not.
(defun delve-lister-end-of-lines (buf marker-or-pos &optional no-error)
  "Get the end position of the 'lines' element at MARKER-OR-POS in BUF.
A 'lines' element can be a list item or a static item, such as a
header or footer.

Effectively, the value returned is the position of the cursor gap
of the next (possible) item after the item at POS-OR-MARKER.

An error will be thrown if there is no item at POS-OR-MARKER.  If
NO-ERROR is non-nil, return nil in that case."
  (if-let* ((nchars (delve-lister-get-prop buf marker-or-pos 'nchars)))
      (+ marker-or-pos nchars)
    (unless no-error
      (error "Did not find text property 'nchars at buffer position %d"
             (delve-lister-pos-as-integer marker-or-pos)))))

;; -----------------------------------------------------------
;; * Static items

;; Basic functions for dealing with so-called static items. Best
;; examples for static items are the list header and the list footer.
;; More generally, 'static' items are distinguished from normal list
;; items by two features:
;;
;;  (1.) They have no cursor gap. Thus, they are completely
;; "unreachable" with the usual navigation tools. Accordingly, they
;; are not marked as an item and are not part of the
;; `delve-lister-local-marker-list'. Thus they are effectively hidden from
;; view for all functions which act on regular list items.
;;
;;  (2.) Their content is printed as-is, not using the local mapper.
;;

(defun delve-lister-make-item-static (lister-buf marker-or-pos &rest props)
  "In LISTER-BUF, mark the item at MARKER-OR-POS as a static item.
Basically, that means (a) to close the cursor gap, and (b) to
remove the text property 'item' and to replace it with the text
property 'static'.

Optionally also use PROPS to add extra property-value-pairs to be
set at the cursor gap position (i.e. to make the item easily
detectable by searching for text properties)."
  (apply #'delve-lister-add-props lister-buf marker-or-pos
         'item nil
         'static t
         'cursor-intangible t
         'front-sticky t
         props))

;; TODO Write tests
(defun delve-lister-insert-static-item (lister-buf position-or-symbol data &optional level)
  "Insert DATA as a static item at POSITION-OR-SYMBOL in LISTER-BUF.
POSITION-OR-SYMBOL must be either an integer position, a marker,
or one of the symbols `:first', `:last' or `:point'.  DATA must be
a list of strings which will be inserted unmodified.  Optional
argument LEVEL determines the indentation level of the item,
defaulting to 0.

Return the marker of the inserted item."
  (let ((cursor-sensor-inhibit t))
    (delve-lister-sensor-leave lister-buf)
    (let* ((pos    (delve-lister-eval-pos-or-symbol lister-buf position-or-symbol))
           (marker (delve-lister-insert-lines lister-buf
                                        pos
                                        data
                                        (delve-lister-determine-level lister-buf
                                                                pos
                                                                (or level 0)))))
      (delve-lister-make-item-static lister-buf marker)
      ;; TODO Hide this static item iff its associate is also hidden.
      (with-current-buffer lister-buf
        (goto-char marker))
      (delve-lister-sensor-enter lister-buf)
      marker)))

;; -----------------------------------------------------------
;; * Set header or footer of the list

;; Headers or footers are static items placed at the end or the
;; beginning of the list. Headers or footers have their own text
;; property, the property 'static is removed.

(defun delve-lister-make-header-or-footer (lister-buf marker-or-pos)
  "Change the item at MARKER-OR-POS to a header or footer.
LISTER-BUF is a lister buffer."
  (delve-lister-make-item-static lister-buf marker-or-pos
                           'header-or-footer t
                           'static nil))

(defun delve-lister-set-header (lister-buf header)
  "Insert or replace HEADER before the first item in LISTER-BUF.
HEADER can be a string or a list of strings.  If HEADER is nil,
remove any existing header."
  (with-current-buffer lister-buf
    (and
     (setq delve-lister-local-header-marker
           (if delve-lister-local-header-marker
               (delve-lister-replace-lines lister-buf delve-lister-local-header-marker header)
             (delve-lister-insert-lines lister-buf (point-min) header 0)))
     (delve-lister-make-header-or-footer lister-buf delve-lister-local-header-marker))))

(defun delve-lister-set-footer (lister-buf footer)
  "Insert or replace FOOTER after the last item of LISTER-BUF.
FOOTER can be a string or a list of strings.  If FOOTER is nil,
remove any existing footer."
  (with-current-buffer lister-buf
    (and
     (setq delve-lister-local-footer-marker
           (if delve-lister-local-footer-marker
               (delve-lister-replace-lines lister-buf delve-lister-local-footer-marker footer)
             (delve-lister-insert-lines lister-buf (point-max) footer 0)))
     (delve-lister-make-header-or-footer lister-buf delve-lister-local-footer-marker))))


;; -----------------------------------------------------------
;; * Filtering

;; Showing and hiding items

(defun delve-lister-set-item-invisibility (lister-buf marker-or-pos value)
  "In LISTER-BUF, show or hide the item at MARKER-OR-POS.
The VALUE t hides the item, nil makes it visible."
  (with-current-buffer lister-buf
    (let* ((inhibit-read-only t)
           (cursor-sensor-inhibit t)
           (beg (delve-lister-pos-as-integer marker-or-pos))
           (end (delve-lister-end-of-lines lister-buf marker-or-pos)))
      (put-text-property beg end 'invisible value)
      ;; this closes the gap for the marker:
      (put-text-property beg (1+ beg) 'front-sticky value))))

(defun delve-lister-show-item (lister-buf marker-or-pos)
  "In LISTER-BUF, set the item at MARKER-OR-POS as visible."
  (delve-lister-set-item-invisibility lister-buf marker-or-pos nil))

(defun delve-lister-hide-item (lister-buf marker-or-pos)
  "In LISTER-BUF, set the item at MARKER-OR-POS as invisible."
  (delve-lister-set-item-invisibility lister-buf marker-or-pos t))

(defun delve-lister-item-invisible-p (lister-buf marker-or-pos)
  "Check if MARKER-OR-POS in LISTER-BUF is visible.

This is just wrapper for calling `invisible-p'.  If you have the
buffer current, you might as well that function directly."
  (with-current-buffer lister-buf
    (invisible-p (delve-lister-pos-as-integer marker-or-pos))))

;; Access only the hidden or visible items

;; TODO Add boundaries, use items-in-region instead of delve-lister-local-marker-list
(defun delve-lister-invisible-items (lister-buf)
  "Get all markers pointing only to hidden items in LISTER-BUF."
  (delve-lister-with-lister-buffer lister-buf
    (seq-filter (lambda (m)
                  ;; Since the marker position is the place for
                  ;; accessing the item with the cursor, we can safely
                  ;; assume that if the marker position is invisible,
                  ;; the whole item is invisible:
                  (text-property-any m (1+ m) 'invisible t))
                delve-lister-local-marker-list)))

;; TODO Add boundaries, use items-in-region instead of delve-lister-local-marker-list
(defun delve-lister-visible-items (lister-buf)
  "Get all markers pointing only to visible items in LISTER-BUF."
  (delve-lister-with-lister-buffer lister-buf
    (seq-filter (lambda (m)
                  ;; Since the marker position is the place for
                  ;; accessing the item with the cursor, we can safely
                  ;; assume that if the marker position is invisible,
                  ;; the whole item is invisible:
                  (text-property-any m (1+ m) 'invisible nil))
                delve-lister-local-marker-list)))

;; * The actual filtering:

(defun delve-lister-filter--all-items (lister-buf filter-fn)
  "In LISTER-BUF, set visibility of each item according to FILTER-FN.

FILTER-FN must accept one argument, the item's data.  It is called
with point on the item examined.  If FILTER-FN returns t, show
the item; else hide it.

This is an internal function which does NOT store the filter in
the buffer.  For setting or removing the filter, use
`delve-lister-set-filter' instead."
  (let* ((w-fn (lambda (data)
                 (delve-lister-set-item-invisibility (current-buffer)
                                               (point)
                                               (not (funcall filter-fn data))))))
    (delve-lister-walk-all lister-buf w-fn)))

(defun delve-lister-set-filter (lister-buf filter-fn)
  "Activate FILTER-FN, replacing any previous filter settings.

Only show items where FILTER-FN returns non-nil values.  FILTER-FN
will be called with each item's data.  Subsequent insertions of
new items will respect the active filter.

To deactivate the filter, call this function with FILTER-FN set
to nil.  This restores visibility for all items.

LISTER-BUF is a lister buffer."
  (with-current-buffer lister-buf
    ;; do nothing if there is no filter passed and no filter active
    (when (or filter-fn delve-lister-local-filter-fn)
      ;; else update all items
      (if (setq delve-lister-local-filter-fn filter-fn)
          ;; that is: either apply the new filter per item
          (delve-lister-filter--all-items lister-buf filter-fn)
        ;; or if there is no filter, show all items again
        (delve-lister-walk-all lister-buf
                         (lambda (_)
                           (delve-lister-show-item (current-buffer)
                                             (point))))))))

(defun delve-lister-filter-active-p (lister-buf)
  "Return non-nil if LISTER-BUF has an active filter."
  (buffer-local-value 'delve-lister-local-filter-fn lister-buf))

;;; -----------------------------------------------------------
;;; * Insert, add, remove or replace list items

;; Utilities for insertion

(defun delve-lister-determine-level (lister-buf pos-or-marker level)
  "Return the indentation level for new items at POS-OR-MARKER.
LEVEL can be nil or an integer.

If LEVEL is nil, return the level of the previous item or 0.

If LEVEL is an integer, return it unchanged if it is below or
equal the previous item's level, else return the previous item's
level + 1.

LISTER-BUF is a lister buffer."
  (let* ((prev-pos       (delve-lister-looking-at-prop lister-buf pos-or-marker 'level 'previous))
         (prev-level     (and prev-pos (get-text-property prev-pos 'level lister-buf))))

    (max 0
         (cond
          ((null prev-pos)        0)  ;; no previous level
          ((null level)           prev-level)
          ((> level prev-level)   (1+ prev-level))
          (t                      level)))))

;; Insert Single Items

(defun delve-lister-insert (lister-buf position-or-symbol data &optional level)
  "Insert DATA as item at POSITION-OR-SYMBOL in LISTER-BUF.
POSITION-OR-SYMBOL must be a buffer position, a marker, or the
symbols `:point', `:first' or `:last'.  The indicated position
will not be checked for validity.

Insert DATA at the indentation level LEVEL.  For the possible
values of LEVEL, see `delve-lister-determine-level'.

Return the marker of the inserted item's front cursor gap
position (the position 'of' the inserted item itself).

Note that to insert a new item at a position means to move any
existing items at this position further down.  Thus, `:last'
effectively inserts an item before the last item.  If you want to
add an item to the end of the list, use `delve-lister-add'."
  (let* ((cursor-sensor-inhibit t))
    (delve-lister-sensor-leave lister-buf)
    (let* ((marker-or-pos (delve-lister-eval-pos-or-symbol lister-buf position-or-symbol))
           (marker        (delve-lister-insert-lines lister-buf
                                               marker-or-pos
                                               (funcall (buffer-local-value 'delve-lister-local-mapper lister-buf) data)
                                               (delve-lister-determine-level lister-buf marker-or-pos level))))

      ;; set item properties:
      (delve-lister-set-data lister-buf marker data)
      (delve-lister-add-props lister-buf marker
                        'cursor-sensor-functions
                        '(delve-lister-sensor-function))

      ;; maybe hide item:
      (when-let ((fn (buffer-local-value 'delve-lister-local-filter-fn lister-buf)))
        (unless (funcall fn data)
          (delve-lister-hide-item lister-buf marker)))

      ;; add marker to marker list:
      (delve-lister-add-item-marker lister-buf marker)

      ;; update display
      (with-current-buffer lister-buf
        (goto-char marker))
      (delve-lister-sensor-enter lister-buf)

      marker)))

;; Insert sequences of items

(defun delve-lister-insert-sequence (lister-buf pos-or-marker seq &optional level)
  "Insert SEQ at POS-OR-MARKER in LISTER-BUF.
Insert the list SEQ above the item marked by POS-OR-MARKER.  If
POS-OR-MARKER is nil, add it to the end of the list.

LEVEL determines the level of hierarchical indentation.  See
`delve-lister-determine-level' for all possible values for LEVEL.

Return an incrementally sorted list of the newly inserted
markers."
  (when seq
    (let* ((new-marker     nil)
           (last-pos       nil))

      (delve-lister-sensor-leave lister-buf)

      (let* ((delve-lister-inhibit-cursor-action t)
             (delve-lister-inhibit-marker-list t)
             (cursor-sensor-inhibit t)
             (pos          (or pos-or-marker (delve-lister-item-max lister-buf)))
             (new-level    (delve-lister-determine-level lister-buf pos level)))

        (cl-dolist (item seq)
          ;; For reasons of speed, we build the new marker list in the
          ;; 'wrong' decremental order and reverse it afterwards.
          ;; Accessing the last inserted marker via (car) is muuuuch
          ;; faster than using (car (last)), since the latter has to
          ;; traverse the whole list.
          (setq new-marker (nconc
                            (if (eq (type-of item) 'cons)
                                (nreverse (delve-lister-insert-sequence lister-buf pos item (1+ new-level)))
                              (list (delve-lister-insert lister-buf pos item new-level)))
                            new-marker))
          (setq pos (delve-lister-end-of-lines lister-buf (setq last-pos (car new-marker))))))
      (setq new-marker (nreverse new-marker))
      (delve-lister-add-item-marker lister-buf new-marker)
      (delve-lister-sensor-enter lister-buf last-pos)
      new-marker)))

(defun delve-lister-insert-sublist-below (lister-buf pos-or-marker seq)
  "Insert SEQ with indentation below the item at POS-OR-MARKER.
LISTER-BUF is a lister buffer."
  (when (delve-lister-item-p lister-buf pos-or-marker)
    (delve-lister-with-locked-cursor lister-buf
      (delve-lister-insert-sequence lister-buf
                              (delve-lister-end-of-lines lister-buf pos-or-marker)
                              seq
                              (1+ (delve-lister-get-level-at lister-buf pos-or-marker))))))

;; Add single item to the end of the list

(defun delve-lister-add (lister-buf data &optional level)
  "Add a list item representing DATA to the end of the list in LISTER-BUF.
Insert DATA at the indentation level LEVEL.  For all possible
values of LEVEL, see `delve-lister-determine-level'.

Return the marker of the added item's cursor gap position."
  (delve-lister-insert lister-buf
                 (delve-lister-item-max lister-buf)
                 data level))

;; Add sequence of items to the end of the list

(defun delve-lister-add-sequence (lister-buf seq &optional level)
  "Add SEQ as items to LISTER-BUF with indentation LEVEL.
Despite its name, SEQ must be a list.  Traverse SEQ and store its
elements as data into the newly created list items.  Nested lists
will be represented as sublists.

LEVEL determines the level of indentation.  When LEVEL is nil,
insert SEQ at the level defined by the item at point.  For all
possible values of LEVEL, see `delve-lister-determine-level'.

Return a list of newly inserted markers."
  (delve-lister-insert-sequence lister-buf nil seq level))

;; Remove item

(defun delve-lister-remove (lister-buf position-or-symbol)
  "Remove the item at POSITION-OR-SYMBOL from LISTER-BUF.
POSITION can be either a buffer position, a marker, or one of the
symbols `:point', `:last' or `:first'.  Do nothing if the position
does not indicate an item.

Also call the sensor functions before and after removing the
item; update the buffer local marker list and move point if the
removed item was at the end of the list.

The automatic correction of point is turned off when
`delve-lister-inhibit-cursor-action' is set to t."
  (when-let* ((pos-marker (delve-lister-marker-at lister-buf position-or-symbol)))
    (let* ((cursor-pos         (with-current-buffer lister-buf (point)))
           (pos                (marker-position pos-marker)))
      ;; call sensor functions for leaving the item at point:
      (unless delve-lister-inhibit-cursor-action
        (when (= cursor-pos pos)
          (delve-lister-sensor-leave lister-buf)))
      ;; remove associated marker from the local marker list
      (with-current-buffer lister-buf
        (setq delve-lister-local-marker-list
              (cl-remove pos delve-lister-local-marker-list :test #'=)))
      ;; remove the item
      (delve-lister-remove-lines lister-buf pos)
      ;; move point if it is not on an item anymore:
      (unless (or delve-lister-inhibit-cursor-action
                  (get-text-property pos 'item lister-buf))
        (with-current-buffer lister-buf
          (goto-char (delve-lister-marker-at lister-buf :last))))
      ;; if we left the sensor, turn it on again:
      (unless delve-lister-inhibit-cursor-action
        (when (= cursor-pos pos)
          (delve-lister-sensor-enter lister-buf pos))))))

;; Remove intended sublists

(defun delve-lister-get-level-at (lister-buf position-or-symbol)
  "Get current indentation level of item at POSITION-OR-SYMBOL.
LISTER-BUF is a lister buffer.

Return nil is there is no valid item at the position indicated."
  (when-let ((m (delve-lister-marker-at lister-buf position-or-symbol)))
    (or (get-text-property (marker-position m) 'level lister-buf) 0)))

(defun delve-lister-sublist-boundaries (lister-buf marker-or-pos)
  "Return the inner boundaries of the sublist containing MARKER-OR-POS.
The return value is a list with four items.  The first item is a
marker pointing to the first item of the sublist.  The second item
is a marker pointing to the last item of the sublist.  The third
and the fourth item are the the corresponding integer positions.

Example:
  ;; these are the boundaries of the first four items:
  (#<marker ....> #<marker ...> 0 3)

LISTER-BUF is a lister buffer."
  ;; NOTE An alternative approach would be to proceed from the item
  ;; and to move up and down using text property searches. It should
  ;; be faster since the current version still needs to access the
  ;; text properties to determine the level.
  (delve-lister-with-lister-buffer lister-buf
    (let* ((marker  (delve-lister-pos-as-marker lister-buf marker-or-pos))
           (n       (cl-position marker delve-lister-local-marker-list :test #'=))
           (last-n  (1- (length delve-lister-local-marker-list)))
           (level   (get-text-property marker 'level))
           (beg-n   (cl-loop for i downfrom n to 0
                             ;; to determine ONLY the same level, use =
                             while (<= level (get-text-property (elt delve-lister-local-marker-list i) 'level))
                             finally return (1+ i)))
           (end-n   (cl-loop for i upfrom n to last-n
                             while (<= level (get-text-property (elt delve-lister-local-marker-list i) 'level))
                             finally return (1- i)))
           (beg     (elt delve-lister-local-marker-list beg-n))
           (end     (elt delve-lister-local-marker-list end-n)))
      (list beg end beg-n end-n))))

(defmacro delve-lister-with-sublist-at (buf pos first-sym last-sym &rest body)
  "Execute BODY with FIRST-SYM and LAST-SYM bound to the sublist at POS.
BUF is a lister buffer, POS an integer position or marker.
FIRST-SYM and LAST-SYM have to be symbol names.

When executing BODY, FIRST-SYM and LAST-SYM will be bound to the
first and the last item's position of the sublist at POS."
  (declare (indent 4) (debug (sexp sexp symbolp symbolp body)))
  (let ((boundaries-var (make-symbol "boundaries"))
        (buf-var        (make-symbol "buffer")))
    `(let ((,buf-var ,buf))
       (and (delve-lister-nonempty-p ,buf)
            (let* ((,boundaries-var (delve-lister-sublist-boundaries ,buf-var ,pos))
                   (,first-sym (car ,boundaries-var))
                   (,last-sym  (cadr ,boundaries-var)))
              ,@body)))))

(defun delve-lister-get-sublist-data (lister-buf pos)
  "Return the sublist at POS in LISTER-BUF as a flat list."
  (delve-lister-with-sublist-at lister-buf pos first last
    (delve-lister-get-all-data lister-buf first last)))

(defun delve-lister-get-sublist-data-tree (lister-buf pos)
  "Return the sublist at POS in LISTER-BUF as a nested list."
  (delve-lister-with-sublist-at lister-buf pos first last
    (delve-lister-get-all-data-tree lister-buf first last)))

(defun delve-lister--pos-in-region-p (beg end m)
  "Check if pos or marker M is between BEG and END (inclusive)."
  (and (>= m beg) (<= m end)))

(defun delve-lister--delete-region (lister-buf beg end)
  "Delete region between BEG and END, updating local marker list.
LISTER-BUF is a lister buffer.  BEG and END are integer positions
or marker.  If BEG or END are nil, use the lower or upper
boundaries of the whole list instead."
  (let ((kill-em-all-p (and (null beg) (null end))))
    (delve-lister-with-normalized-region lister-buf beg end
      (with-current-buffer lister-buf
        (let ((inhibit-read-only t)
              (cursor-sensor-inhibit t))
          ;; re-set local marker list
          (setq delve-lister-local-marker-list
                (unless kill-em-all-p
                  ;; FIXME  wrap that in an extra function, which will
                  ;; then also be used by delve-lister-items-in-region. That
                  ;; function should use the subseq method, which
                  ;; seems faster.
                  (cl-remove-if (apply-partially #'delve-lister--pos-in-region-p beg end)
                                delve-lister-local-marker-list)))
          ;; if item is deleted, remove it from the sensor queue:
          (when (and delve-lister-sensor-last-item
                     (delve-lister--pos-in-region-p beg end delve-lister-sensor-last-item))
            (setq delve-lister-sensor-last-item nil))
          ;; delete the region:
          (delete-region beg
                         ;; end points to the cursor gap of the last
                         ;; item, or to the local footer, or to point max.
                         (+ end (or (if (get-text-property end 'item)
                                        (get-text-property end 'nchars)
                                      0)))))))))

(defun delve-lister-remove-this-level (lister-buf pos-or-marker)
  "Remove all surrounding items matching the level of the item at POS-OR-MARKER.
LISTER-BUF is a lister buffer."
  (delve-lister-with-sublist-at lister-buf pos-or-marker first last
    (delve-lister--delete-region lister-buf first last)))

(defun delve-lister-sublist-below-p (lister-buf pos-or-marker)
  "Check if the next item is indented with respect to POS-OR-MARKER.
LISTER-BUF is a lister buffer."
  (let* ((next-item      (delve-lister-end-of-lines lister-buf pos-or-marker))
         (current-level  (delve-lister-get-level-at lister-buf pos-or-marker))
         (next-level     (delve-lister-get-level-at lister-buf next-item)))
    (unless current-level
      (error "No item at position %d" (delve-lister-pos-as-integer pos-or-marker)))
    (if next-level
        (> next-level current-level)
      'nil)))

(defun delve-lister-remove-sublist-below (lister-buf pos-or-marker)
  "Remove the sublist below the item at POS-OR-MARKER.
Do nothing if the next item is not a sublist.
LISTER-BUF is a lister buffer."
  (when (delve-lister-sublist-below-p lister-buf pos-or-marker)
    ;; don't call sensor function if removed items are below point:
    (let* ((delve-lister-inhibit-cursor-action (= (with-current-buffer lister-buf (point))
                                            pos-or-marker)))
      (delve-lister-remove-this-level lister-buf (delve-lister-end-of-lines lister-buf pos-or-marker)))))

;; Replace item

(defun delve-lister-replace (lister-buf position-or-symbol data &optional new-level)
  "Replace the item at POSITION-OR-SYMBOL with one representing DATA.
POSITION-OR-SYMBOL is either a marker, a buffer position or one
the symbols `:point', `:first' or `:last'.

The new item keeps the indentation level, if NEW-LEVEL is not
set.

LISTER-BUF is a lister buffer."
  (delve-lister-with-locked-cursor lister-buf
    (let* ((pos-marker (delve-lister-marker-at lister-buf position-or-symbol))
           (level  (or new-level (get-text-property (marker-position pos-marker) 'level lister-buf))))
      (delve-lister-remove lister-buf pos-marker)
      (delve-lister-insert lister-buf pos-marker data level))))

;; * Replace the whole buffer list (set the list)

(defun delve-lister-replace-list (lister-buf seq first last &optional level)
  "Replace the list between positions FIRST and LAST with SEQ.
FIRST and LAST have to be item buffer positions.  If either is
nil, use the position of the very first or last item instead.

LEVEL is the hierarchy level of the list to be inserted.  SEQ is a
list of items to be inserted.

Return the marker list of the inserted sequence SEQ.  If SEQ is
empty, only delete the current list.  If FIRST and LAST do not
mark any list items, do nothing.

LISTER-BUF is a lister buffer."
  (delve-lister-with-locked-cursor lister-buf
    (delve-lister-with-normalized-region lister-buf first last
      (delve-lister--delete-region  lister-buf first last)
      (delve-lister-insert-sequence lister-buf first seq level))))

(defun delve-lister-set-list (lister-buf seq)
  "In LISTER-BUF, insert SEQ, leaving header and footer untouched.
SEQ can be nested to insert hierarchies."
  (delve-lister-with-lister-buffer lister-buf
    (if (delve-lister-nonempty-p lister-buf)
        (delve-lister-replace-list lister-buf seq nil nil)
      (delve-lister-with-locked-cursor lister-buf
        (delve-lister-add-sequence lister-buf seq)))))

;; -----------------------------------------------------------
;; * Marking and unmarking items

;; Visually reflect the mark state

(defun delve-lister-display-mark-state (lister-buf marker-or-pos)
  "In LISTER-BUF, display the item as marked or not marked.
The item is referred to via MARKER-OR-POS pointing to its cursor
gap position."
  (delve-lister-with-lister-buffer lister-buf
    (let* ((inhibit-read-only t)
           (state    (delve-lister-get-mark-state lister-buf marker-or-pos))
           (beg      (delve-lister-pos-as-integer marker-or-pos))
           (end      (delve-lister-pos-as-integer (delve-lister-end-of-lines lister-buf beg))))
      (if state
          (delve-lister-add-face-property beg end delve-lister-mark-face-or-property)
        (delve-lister-remove-face-property beg end delve-lister-mark-face-or-property)))))

;; Query the mark state

(defun delve-lister-get-mark-state (lister-buf pos-or-symbol)
  "In LISTER-BUF, test if the item at POS-OR-SYMBOL is marked.
POS-OR-SYMBOL can be either a marker, a position, or one of the
symbols `:point', `:first' and `:last'.

Return the marker state (nil or true).  Also return nil if there
is no item at POS-OR-SYMBOL."
  (let (m)
    (when (and pos-or-symbol
               (setq m (delve-lister-marker-at lister-buf pos-or-symbol)))
      (get-text-property (delve-lister-pos-as-integer m) 'mark lister-buf))))

(defun delve-lister-all-marked-items (lister-buf)
  "Get all markers pointing to marked items in LISTER-BUF."
  (seq-filter (apply-partially #'delve-lister-get-mark-state lister-buf)
              (buffer-local-value 'delve-lister-local-marker-list lister-buf)))

;; Marking a single item

(defun delve-lister-markable-p (lister-buf position-or-symbol)
  "Check if item at POSITION-OR-SYMBOL can be marked.
LISTER-BUF is a lister buffer."
  (let ((pred-fn (buffer-local-value 'delve-lister-local-marking-predicate lister-buf)))
    (or (null pred-fn)
        ;; implicitly also checks if the item is valid
        (when-let ((data (delve-lister-get-data lister-buf position-or-symbol)))
          (funcall pred-fn data)))))

(defun delve-lister-mark-item (lister-buf position-or-symbol value)
  "According to VALUE, mark or unmark the item at POSITION-OR-SYMBOL.
VALUE must be a boolean value.  POSITION-OR-SYMBOL can be a
marker, a buffer position, or one of the symbols `:point',
`:first' or `:last'.  LISTER-BUF is a lister buffer.

Return t if the item's state has been set to VALUE, else nil."
  (let (m)
    (when (and position-or-symbol
               (setq m (delve-lister-marker-at lister-buf position-or-symbol))
               (delve-lister-markable-p lister-buf m))
      (delve-lister-add-props lister-buf m 'mark value)
      (delve-lister-display-mark-state lister-buf m)
      t)))

;; Marking a list of items

(defun delve-lister-mark-some-items (lister-buf positions value)
  "In LISTER-BUF, mark all items in POSITIONS with VALUE."
  (delve-lister-with-lister-buffer lister-buf
    ;; using 'save-excursion' instead of `delve-lister-with-locked-cursor'
    ;; assumes that marking does not change the item's positions or
    ;; size:
    (save-excursion
      (seq-do (lambda (m) (delve-lister-mark-item lister-buf m value))
              positions))))

(defun delve-lister-mark-all-items (lister-buf value)
  "Set all items to the marking state VALUE in LISTER-BUF."
  (delve-lister-mark-some-items lister-buf
                          (buffer-local-value 'delve-lister-local-marker-list lister-buf)
                          value))

(defun delve-lister-mark-this-sublist (lister-buf marker-or-pos value)
  "Mark the sublist to which MARKER-OR-POS belongs.
LISTER-BUF has to be a valid lister buffer."
  (let* ((bounds (delve-lister-sublist-boundaries lister-buf marker-or-pos))
         (index-first (cl-third bounds))
         (index-last  (cl-fourth bounds))
         (all-markers (buffer-local-value 'delve-lister-local-marker-list lister-buf)))
    (delve-lister-mark-some-items lister-buf
                            (cl-subseq all-markers
                                       index-first
                                       (1+ index-last))
                            value)))

;; Walk the marked data

(defun delve-lister-walk-marked-items (lister-buf action &optional pred)
  "In LISTER-BUF, apply ACTION on each marked item.
The function ACTION is executed on each marked item, with point
on the item's cursor gap.  It receives the item's data object as
its sole argument.  The accumulated results will be passed as the
return value.

Optionally further restrict action on only those marked items
matching PRED."
  (delve-lister-walk-some lister-buf
                    (delve-lister-all-marked-items lister-buf)
                    action pred))


;; -----------------------------------------------------------
;; * Setting and getting stored data

;; Set data

(defun delve-lister-set-data (lister-buf position-or-symbol data)
  "Store DATA at POSITION-OR-SYMBOL in LISTER-BUF.
POSITION-OR-SYMBOL is either a buffer position, a marker, or one
 of the symbols `:point', `:last' or `:first'."
  (when-let* ((m (delve-lister-marker-at lister-buf position-or-symbol)))
    (delve-lister-add-props lister-buf m 'data data)))

;; Get data

(defun delve-lister-get-data (lister-buf position-or-symbol)
  "Return the data stored at POSITION-OR-SYMBOL in LISTER-BUF.
POSITION-OR-SYMBOL can be either a buffer position, a marker, or
 one of the symbols `:point', `:last' or `:first' ."
  (when-let* ((m (delve-lister-marker-at lister-buf position-or-symbol)))
    (delve-lister-get-prop lister-buf m 'data)))

;; Get lists of data:

(defun delve-lister-get-all-data (lister-buf &optional first last)
  "Collect data of all items in LISTER-BUF.
Return a flat list, ignoring any nested levels or hierarchies.

Optional positions FIRST and LAST can be used to restrict the
result to the range from FIRST up to, and including, LAST.

LISTER-BUF is a lister buffer."
  (seq-map (apply-partially #'delve-lister-get-data lister-buf)
           (delve-lister-items-in-region lister-buf first last)))

(defun delve-lister-get-visible-data (lister-buf)
  "Collect the data values of all items visible in LISTER-BUF."
  (seq-map (apply-partially #'delve-lister-get-data lister-buf)
           (delve-lister-visible-items lister-buf)))

;; Get data as tree:

(defun delve-lister-group-by-level (l &optional start-level)
  "Map L to a tree.
L is a list of cons cells, each pairing the item and its
associated nesting level, e.g. ((a 0) (b 0) (c 1)).  Nesting
begins with 0.

START-LEVEL indicates at what level the nesting begins.  Thus, if
START-LEVEL is 0, the group ((a 1) (b 1)) will be returned as the
nested list ((a b)).  If START-LEVEL is nil, take the level of L's
first item as the starting level, effectively using it as a base
level."
  (declare (pure t) (side-effect-free t))
  (when l
    (let (push-item
          res
          (level (or start-level
                     ;; if no start-level is given, use the first
                     ;; element's level as reference:
                     (cadr (car l))))
          (walk l))
      (while
          (progn
            (pcase-let ((`(,item ,new-level) (car walk)))
              ;; return if we have finished a level:
              (unless (> level new-level)
                ;; else push item or list:
                (if (= level new-level)
                    (setq push-item item
                          walk (cdr walk))
                  (setq push-item (delve-lister-group-by-level walk (1+ level))
                        walk (nthcdr (length (delve-lister--flatten push-item)) walk)))
                (push push-item res)
                walk))))
      (nreverse res))))

(defun delve-lister-get-all-data-tree (lister-buf &optional beg end)
  "Collect all data values in LISTER-BUF, respecting its hierarchy.
Optionally restrict the result to the items ranging from the
buffer positions BEG and END (END is inclusive).  If either BEG or
END is nil, use the position of the first or last item."
  (when-let* ((data-list (seq-map
                          (lambda (pos)
                            ;; we want a value pair: (data level)
                            (delve-lister-get-props-at lister-buf pos 'data 'level))
                          (delve-lister-items-in-region lister-buf beg end))))
    (delve-lister-group-by-level data-list)))

;; -----------------------------------------------------------
;; * Walk the lister buffer

(defun delve-lister-walk-some (lister-buf item-positions action
                                    &optional predicate)
  "Collect the results from applying ACTION on all ITEM-POSITIONS.

LISTER-BUF is a lister buffer.

ITEM-POSITIONS is a list consisting of either integer positions
or markers.

The function ACTION will be called with the item's associated
data.  Optional function PREDICATE can be used to further restrict
the items on which ACTION will be executed.

Execute ACTION if the position points to a valid item (and
optionally, if PREDICATE also returns a non-nil value).  Positions
not pointing to an item will be silently skipped.

Both PREDICATE and ACTION are called with point on the item's
cursor gap and the current buffer set to LISTER-BUF, making it
easy to use all common lister functions.  The whole loop is
wrapped in a call to `delve-lister-with-locked-cursor', which see.

Return the accumulated results of all executed actions."
  (delve-lister-with-locked-cursor lister-buf
    (with-current-buffer lister-buf
      (let ((acc nil)
            (min (delve-lister-item-min lister-buf))
            (max (delve-lister-item-max lister-buf)))
        (cl-dolist (item item-positions)
          (goto-char item)
          (when (and (>= item min)
                     (<= item max)
                     (get-text-property item 'item))
            (let ((data (delve-lister-get-data lister-buf item)))
              (when (or (null predicate)
                        (funcall predicate data))
                (push (funcall action data) acc)))))
        (nreverse acc)))))

(defun delve-lister-walk-all (lister-buf action &optional pred)
  "In LISTER-BUF, execute ACTION for each item matching PRED.
See `delve-lister-walk-some' for more details."
  (delve-lister-walk-some lister-buf
                    (buffer-local-value 'delve-lister-local-marker-list
                                        lister-buf)
                    action
                    pred))

;; -----------------------------------------------------------
;; * Moving point API

;; Go to an item

(defun delve-lister-goto (lister-buf position-or-symbol)
  "In LISTER-BUF, move point to POSITION-OR-SYMBOL.
POSITION-OR-SYMBOL is a marke, a buffer position or one of the
symbols `:last', `:point' or `:first'.  Return the position.
Throw an error if the item is not visible."
  (let* ((m (or (delve-lister-marker-at lister-buf position-or-symbol)
                (delve-lister-item-max  lister-buf))))
    (delve-lister-with-lister-buffer lister-buf
      (if (invisible-p m)
          (error "Item not visible")
        (goto-char m)
        (delve-lister-sensor-leave lister-buf)
        (delve-lister-sensor-enter lister-buf)
        m))))

;; -----------------------------------------------------------
;; * Editing the list

(defun delve-lister-move-item-up (buf pos)
  "In lister buffer BUF, move item at POS one up."
  (interactive (list (current-buffer) (point)))
  (delve-lister--move-item-vertically buf pos 'previous))

(defun delve-lister-move-item-down (buf pos)
  "In lister buffer BUF, move item at POS one down."
  (interactive (list (current-buffer) (point)))
  (delve-lister--move-item-vertically buf pos 'next))

(defun delve-lister-move-item-left (buf pos)
  "In lister buffer BUF, unindent item at POS."
  (interactive (list (current-buffer) (point)))
  (delve-lister--move-item-horizontally buf pos 'left))

(defun delve-lister-move-item-right (buf pos)
  "In lister buffer BUF, indent item at POS."
  (interactive (list (current-buffer) (point)))
  (delve-lister--move-item-horizontally buf pos 'right))

(defun delve-lister--move-item-vertically (buf pos direction)
  "Move item up or down.
BUF is a lister buffer.  POS is the position of the item to be
moved.  DIRECTION is either the symbol `previous' or `next'."
  (unless (delve-lister-item-p buf pos)
    (user-error "There is no item at point"))
  ;; first check if there is an item above or below.
  (let* ((next-pos (delve-lister-looking-at-prop buf pos 'item direction)))
    (unless next-pos
      (user-error (format "Item cannot be moved in that direction")))
    ;; make sure that moving the item retains the level:
    (let* ((level-current (delve-lister-get-level-at buf pos))
           (level-next    (delve-lister-get-level-at buf next-pos)))
      (when (not (= level-current level-next))
        (user-error "Items can only be moved within their indentation level"))
      ;; the actual movement:
      (let* ((item       (delve-lister-get-data buf pos))
             (mark-state (delve-lister-get-mark-state buf pos)))
        (delve-lister-remove buf pos)
        (setq next-pos (pcase direction
                         ('previous  next-pos)
                         ;; FIXME Shouldn't this be simply
                         ;; delve-lister-end-of-lines pos?
                         ;; Why so complicated?
                         ('next      (or (delve-lister-looking-at-prop buf pos 'item 'next)
                                         (delve-lister-item-max buf)))))
        (delve-lister-insert buf next-pos item level-current)
        (delve-lister-mark-item buf next-pos mark-state)))))

(defun delve-lister--move-item-horizontally (buf pos direction)
  "Move item left or right, changing its indentation level.
BUF is a lister buffer.  POS is the position of the item to be
moved.  DIRECTION is either the symbol `left' or `right'."
  (unless (delve-lister-item-p buf pos)
    (user-error "There is no item at point"))
  (let* ((level-current (delve-lister-get-level-at buf pos))
         (level-new     (pcase direction
                          ('right (delve-lister-determine-level buf pos (1+ level-current)))
                          ('left  (max 0 (1- level-current))))))
    (if (= level-new level-current)
        (user-error (format "Cannot move item further %s" direction))
      ;; the actual movement:
      (let* ((mark-state (delve-lister-get-mark-state buf pos))
             (data       (delve-lister-get-data buf pos)))
        (delve-lister-replace buf pos data level-new)
        (delve-lister-mark-item buf pos mark-state)))))

;; -----------------------------------------------------------
;; * Sorting (or, abstractly, reordering) a list

;; The abstract functions

(defun delve-lister-reorder-list (lister-buf fn &optional first last)
  "Reorder all items from FIRST to LAST using FN.
If FIRST or LAST are nil, use the beginning or the end of the
list as boundaries.  Return the marker list of the reordered
items.  If there are no items to reorder, return nil and do
nothing.

Note that FN has to reorder a wrapped list, consisting of a
cons cell with the actual item as the car and its associated
sublist as its cdr.  It must not undo the wrapping.

LISTER-BUF is a lister buffer."
  (delve-lister-with-normalized-region lister-buf first last
    (when-let* ((old-list (delve-lister-get-all-data-tree lister-buf first last)))
      (let* ((level        (delve-lister-get-level-at lister-buf first))
             (wrapped-list (delve-lister--wrap-list old-list))
             (new-list     (delve-lister--reorder-wrapped-list wrapped-list fn)))
        (delve-lister-replace-list lister-buf new-list first last level)))))

(defun delve-lister-reorder-this-level (lister-buf pos-or-marker fn)
  "Reorder the sublist at POS-OR-MARKER.

Use FN for reordering.  Note that FN has to reorder a wrapped
list, consisting of a cons cell with the actual item as the car
and its associated sublist as its cdr.  It must not undo the
wrapping.

LISTER-BUF is a lister buffer.

Return NIL if there is nothing to reorder."
  (delve-lister-with-sublist-at lister-buf pos-or-marker first last
    (delve-lister-reorder-list lister-buf fn first last)))

(defun delve-lister-reorder-dwim (lister-buf pos-or-marker fn)
  "Reorder the sublist below POS-OR-MARKER or the current level's list.

Use FN for reorderingq.  Note that FN has to reorder a wrapped
list, consisting of a cons cell with the actual item as the car
and its associated sublist as its cdr.  It must not undo the
wrapping.

LISTER-BUF is a lister buffer.

Return NIL if there is nothing to reorder."
  (delve-lister-reorder-this-level lister-buf
                             (if (delve-lister-sublist-below-p lister-buf pos-or-marker)
                                 (delve-lister-end-of-lines lister-buf pos-or-marker)
                               pos-or-marker)
                             fn))

;; Sorting is the most useful case of reordering

(defun delve-lister-sort-list (lister-buf pred &optional first last)
  "Sort all items from FIRST to LAST according to PRED.
If FIRST or LAST are nil, use the beginning or the end of the
list as boundaries.

LISTER-BUF is a lister buffer.

Return NIL if there is nothing to sort."
  (delve-lister-reorder-list lister-buf
                       (apply-partially #'seq-sort-by #'car pred)
                       first last))

(defun delve-lister-sort-this-level (lister-buf pos-or-marker pred)
  "Sort the sublist at POS-OR-MARKER according to PRED.
LISTER-BUF is a lister buffer.

Return NIL if there is nothing to sort."
  (delve-lister-with-sublist-at lister-buf pos-or-marker first last
    (delve-lister-sort-list lister-buf pred first last)))

(defun delve-lister-sort-dwim (lister-buf pos-or-marker pred)
  "Sort the sublist below POS-OR-MARKER or the current level's list.
PRED is sorting predicate.  LISTER-BUF is a lister buffer."
  (delve-lister-reorder-dwim lister-buf pos-or-marker
                       (apply-partially #'seq-sort-by #'car pred)))



;; -----------------------------------------------------------
;; * Cursor Sensor Function

(defun delve-lister-sensor-enter (buf &optional pos)
  "Call the sensor functions for entering POS or point.
POS can be a buffer position or a marker.

Runs the hook `delve-lister-enter-item-hook'.

Do nothing if `delve-lister-inhibit-cursor-action' is t, or if position
POS is not on a valid lister item.

BUF is a lister buffer."
  (with-current-buffer buf
    (when (and (not delve-lister-inhibit-cursor-action)
               cursor-sensor-mode)
      (let ((cursor-sensor-inhibit t))
        (save-excursion
          (setq pos (or pos (point)))
          (when (get-text-property pos 'item)
            (goto-char pos)
            (setq delve-lister-sensor-last-item (delve-lister-pos-as-integer pos))
            (run-hooks 'delve-lister-enter-item-hook)))))))

(defun delve-lister-sensor-leave (buf)
  "Call the sensor functions for leaving the last visited item.

Run the hook `delve-lister-leave-item-hook' with point at the position
of the last visited item.

Do nothing if `delve-lister-inhibit-cursor-action' is t.

BUF is a lister buffer."
  (with-current-buffer buf
    (when (and (not delve-lister-inhibit-cursor-action)
               cursor-sensor-mode
               delve-lister-sensor-last-item)
      (save-excursion
        (let ((cursor-sensor-inhibit t))
          (goto-char delve-lister-sensor-last-item)
          (run-hooks 'delve-lister-leave-item-hook)
          (setq delve-lister-sensor-last-item nil))))))

(defun delve-lister-sensor-function (win previous-point direction)
  "Internal function to run hooks on entering or leaving a lister item.
In a lister buffer, this function will be called when the cursor
moves.  The values for WIN, PREVIOUS-POINT and DIRECTION are used
internally to determine what kind of event has been caused, and
what hooks to run.

Do nothing if `delve-lister-inhibit-cursor-action' is t."
  (with-current-buffer (window-buffer win)
    (when (and (derived-mode-p 'delve-lister-mode)
               (not delve-lister-inhibit-cursor-action))
      (let ((cursor-sensor-inhibit t)
            (inhibit-read-only t))
        (cond
         ((eq direction 'left)    (delve-lister-sensor-leave (current-buffer)))
         ((eq direction 'entered) (delve-lister-sensor-enter (current-buffer)))
         ;; special cases to avoid that the cursor stays on the footer
         ;; or header:
         ((eobp)                  (goto-char previous-point))
         ((not (get-text-property (point) 'item)) nil)
         (t nil))))))

(defun delve-lister-add-enter-callback (lister-buf callback-fn &optional append)
  "Register CALLBACK-FN for the event 'entering an item'.
LISTER-BUF is a lister buffer."
  (with-current-buffer lister-buf
    (add-hook 'delve-lister-enter-item-hook callback-fn append t)))

(defun delve-lister-remove-enter-callback (lister-buf callback-fn)
  "Unregister CALLBACK-FN for the event 'entering an item'.
LISTER-BUF is a lister buffer."
  (with-current-buffer lister-buf
    (remove-hook 'delve-lister-enter-item-hook callback-fn t)))

(defun delve-lister-add-leave-callback (lister-buf callback-fn)
  "Register CALLBACK-FN for the event 'leaving an item'.
LISTER-BUF is a lister buffer."
  (with-current-buffer lister-buf
    (add-hook 'delve-lister-leave-item-hook callback-fn nil t)))

(defun delve-lister-remove-leave-callback (lister-buf callback-fn)
  "Unregister CALLBACK-FN for the event 'leaving an item'.
LISTER-BUF is a lister buffer."
  (with-current-buffer lister-buf
    (add-hook 'delve-lister-leave-item-hook callback-fn nil t)))

;; -----------------------------------------------------------
;; * Lister Major Mode

;; Handle isearch properly

(defvar delve-lister-local-isearch-opoint nil
  "Buffer local variable storing starting point during isearch.")

(defun delve-lister-before-isearch ()
  "Prepare lister buffer for using isearch."
  (cursor-intangible-mode 0)
  (setq-local delve-lister-local-isearch-opoint (point)))

(defun delve-lister-after-isearch ()
  "Make sure point will end on an item after isearch."
  (when (/= (point) delve-lister-local-isearch-opoint)
    (beginning-of-line))
  (cursor-intangible-mode 1)
  (when (not (get-text-property (point) 'item))
    (goto-char delve-lister-local-isearch-opoint)))

;; Keys
(defun delve-lister-key-toggle-mark (&optional sublist)
  "Toggle mark of item at point.
With prefix SUBLIST, mark the whole sublist with the inverted
status of the item at point.  That is, if the item at point is
marked, unmark the whole list, and it is not marked, mark the
whole list."
  (interactive "P")
  (let* ((buf (current-buffer))
         (m (delve-lister-marker-at buf :point)))
    (unless m
      (user-error "No item at point"))
    (let* ((mark-state (delve-lister-get-mark-state buf m)))
      (if sublist
          (delve-lister-mark-this-sublist buf m (not mark-state))
        (if (not (delve-lister-mark-item buf m (not mark-state)))
            (user-error "Item cannot be marked")
          ;; move forward one line after marking:
          (when-let* ((next-item (delve-lister-end-of-lines buf m)))
            (unless (invisible-p next-item)
              (delve-lister-goto buf next-item))))))))

(defun delve-lister-key-mark-all-items ()
  "Mark all items of the current list."
  (interactive)
  (delve-lister-mark-all-items (current-buffer) t))

(defun delve-lister-key-unmark-all-items ()
  "Umark all items of the current list."
  (interactive)
  (delve-lister-mark-all-items (current-buffer) nil))

;; Major modes

(defvar delve-lister-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "m" 'delve-lister-key-toggle-mark)
    (define-key map "*" 'delve-lister-key-mark-all-items)
    (define-key map "u" 'delve-lister-key-unmark-all-items)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map (kbd "<M-up>")    'delve-lister-move-item-up)
    (define-key map (kbd "<M-down>")  'delve-lister-move-item-down)
    (define-key map (kbd "<M-right>") 'delve-lister-move-item-right)
    (define-key map (kbd "<M-left>")  'delve-lister-move-item-left)
    map)
  "Key map for `delve-lister-mode'.")

(define-derived-mode delve-lister-mode
  special-mode "Lister"
  "Major mode for selecting list items."
  :group 'lister
  (cursor-sensor-mode)
  (cursor-intangible-mode)
  (add-hook 'isearch-mode-hook #'delve-lister-before-isearch nil t)
  (add-hook 'isearch-mode-end-hook #'delve-lister-after-isearch nil t)
  (setq buffer-undo-list t))

;; * Set up a lister buffer

;;;###autoload
(defun delve-lister-setup (buf mapper-fn &optional data-list
                         header footer)
  "Set up BUF for Lister mode using MAPPER-FN.
Set the major mode to `delve-lister-mode', if not done yet; prepare the
buffer for further Lister actions; and optionally insert
DATA-LIST, HEADER and FOOTER.  Return BUF.

MAPPER-FN must accept only one argument, the data object, and
returns either a string or a list of strings.

Optional argument DATA-LIST is a list of data objects which will
be inserted using MAPPER-FN.  Optional arguments HEADER and FOOTER
are strings, or a list of strings, which are inserted at the top
or the bottom of the list."
  (with-current-buffer buf
    ;; first of all, set the major mode
    (unless (derived-mode-p 'delve-lister-mode)
      (if (buffer-file-name)
          (error "Lister lists can only be set up in a buffer with no file attached")
        (delve-lister-mode)))
    ;; prepare the buffer:
    (setq delve-lister-local-mapper mapper-fn)
    (setq delve-lister-enter-item-hook nil
          delve-lister-leave-item-hook nil)
    (setq delve-lister-sensor-last-item nil)
    (setq delve-lister-local-filter-fn nil)
    (let ((cursor-sensor-inhibit t)
          (inhibit-read-only t))
      (erase-buffer))
    ;; add header, list and footer:
    (when header
      (delve-lister-set-header buf header))
    (when footer
      (delve-lister-set-footer buf footer))
    (when data-list
      (delve-lister-set-list buf data-list))
    ;; move to first item:
    (if (delve-lister-visible-items buf)
        (delve-lister-goto buf :first)
      (goto-char (point-min)))
    buf))

(provide 'delve-lister)
;;; delve-lister.el ends here
