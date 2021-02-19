;;; delve-with-fake-db.el --- call delve with a fake db   -*- lexical-binding: t; -*-

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

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Call this with Eldev to open a fresh emacs instance with a fake org
;; roam db:
;;
;; eldev emacs -L tests -L tests/utils -eval '(load "delve-with-fake-db")'

(require 'delve-test-db-utils)
(require 'delve)

(setq visible-bell nil
      ring-bell-function 'ignore
      confirm-kill-emacs 'yes-or-no-p)

(setq org-roam-completion-everywhere t)

(let ((key-alist 
       '(("C-c n l" . org-roam)
	 ("C-c n f" . org-roam-find-file)
	 ("C-c n j" . org-roam-jump-to-index)
	 ("C-c n b" . org-roam-switch-to-buffer))))
  (dolist (key-fn-pair key-alist)
    (global-set-key (kbd (car key-fn-pair))
		    (cdr key-fn-pair))))

(delve-test-setup-db)

(add-to-list 'delve-searches
	     '(:name "All pages"
		     :constraint [:order-by (asc titles:title)]))

(delve)

