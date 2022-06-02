;;; delve-crm.el --- completing read multiple for Delve nodes  -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <joerg@joergvolbers.de>
;; Keywords:

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

;; Provide completing read multiple interface.
;; This is taken from citar.el (https://github.com/emacs-citar/citar/compare/simple-crm)
;;

;;; Code:

(defun delve-crm--sort-by-selection (selected-hash candidates)
  "Sort the CANDIDATES by putting those in SELECTED-HASH first."
  (let (selected others)
    (dolist (cand candidates (nreverse (nconc others selected)))
      (if (gethash (substring-no-properties cand) selected-hash)
          (push cand selected)
        (push cand others)))))

(defun delve-crm-select (prompt candidates &optional history)
  "Select multiple CANDIDATES with PROMPT.
HISTORY is the 'completing-read' history argument."
  ;; Because completing-read-multiple just does not work for long
  ;; candidate strings
  (let ((selected-hash (make-hash-table :test #'equal)))
    (while
        (let ((item  (completing-read
                      ;; prompt
                      (format "%s (%s/%s): " prompt
                              (hash-table-count selected-hash)
                              (length candidates))
                      ;; programmed completion
                      (lambda (str pred action)
                        (if (eq action 'metadata)
                            `(metadata (display-sort-function . (lambda (cands) (delve-crm--sort-by-selection ,selected-hash cands)))
                                       (cycle-sort-function . ,#'identity)
                                       (group-function
                                        . ,(lambda (cand transform)
                                             (pcase (list (not (not transform)) (gethash (substring-no-properties cand) selected-hash))
                                               ('(nil nil) "Select Multiple")
                                               ('(nil t)   "Selected")
                                               ('(t   nil) cand)
                                               ('(t   t  ) (propertize cand 'face 'highlight))))))
                          (complete-with-action action candidates str pred)))
                      ;; further arguments:
                      nil
                      t
                      nil
                      history ; FIXME
                      "")))
          (unless (equal item "")
            (cond
             ((gethash item selected-hash)
              (remhash item selected-hash))
             (t
              (puthash item t selected-hash)))
            t)))
    (hash-table-keys selected-hash)))

(provide 'delve-crm)
;;; delve-crm.el ends here
