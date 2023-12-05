;;; delve-embark.el --- Add delve actions as embark actions  -*- lexical-binding: t; -*-

;; Copyright (C) 2023

;; Author:  <joerg@joergvolbers.de>
;; Keywords: delve, embark

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

;; Provide functionality to integrate Delve actions into Embark.

;;; Code:

(require 'embark)
(require 'delve)
(require 'delve-query)

;; TODO Es gibt eine org-link keymap (mit id:xxxx); die kann benutzt
;;werden, um z.B. das Ziel in eine Delve collecetion zu speichern
;; TODO Keymap funktioniert nicht

;;; Variables

(defvar delve-embark--target-finders
  (list #'delve-embark--uuid-finder)
  "List of Delve specific finders for Embark.")

;; TODO Generisch UUID -> Node dann function
;; (defun delve-embark--call-node-with-uuid

(defun delve-embark--test-embark (uuids)
  "Do someting with UUIDS."
  (org-roam-node-open (delve-query-nodes-by-id uuids)))

(defvar delve-embark-node-map
  "Keymap for Embark actions on Org Roam Nodes.
The actions must accept a list of UUIDs, which are passed as a
strings."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") #'delve-embark--test-embark)
    map))

(defvar delve-embark--keymap-alist
  '((delve--uuid . delve-embark-node-map))
  "Alist defining Embark actions for Delve specific types.")

;; * Define Embark Target Finder

;; Nodes are identified via UUID

(defun delve-embark--node-at-point ()
  "Return the current Org Roam node at point."
  (cond
   ((delve--org-roam-buffer-p)
    (org-roam-node-at-point))
   ((delve--buffer-p)
    (delve--zettel-node (delve--current-item-or-error 'delve--zettel)))))

(defun delve-embark--uuid-finder ()
  "Return the current Org Roam node UUID at point as an Embark target.
Target type is `delve--uuid'."
  (when-let* ((node (delve-embark--node-at-point))
              (uuid (org-roam-node-id node)))
    (cons 'delve--uuid uuid)))

;;;; Enable and disable Delve/Embark integration

;; TODO also add keymap
(defun delve-embark--enable ()
  "Add Delve-specific functions to Embark."
  (mapc (apply-partially #'add-hook 'embark-target-finders)
        delve-embark--target-finders)
  (pcase-dolist (`(,type . ,keymap) delve-embark--keymap-alist)
    (setf (alist-get type embark-keymap-alist) keymap)))

(defun delve-embark--disable ()
  "Remove Delve-specific functions from Embark."
  (mapc (apply-partially #'remove-hook 'embark-target-finders)
        delve-embark--target-finders)
  (setq embark-keymap-alist (cl-set-difference embark-keymap-alist
                                               delve-embark--keymap-alist
                                               :test #'equal)))

(provide 'delve-embark)
;;; delve-embark.el ends here
