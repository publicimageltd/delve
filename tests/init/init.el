;;; init.el --- Startup file for loading Emacs with Delve  -*- lexical-binding: t; -*-

;; Copyright (C) 2023

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

;; This startup file can be used to test the current Delve version in
;; this repository 'live'.  To run Emacs, use:
;;
;;   eldev emacs -- --init-directory <link-to-the-directory-containing-this-file>
;;
;; Note that all packages have to be installed as an additional
;; dependency via Eldev's `eldev-add-extra-dependencies'.  These
;; dependencies are defined in the file `Eldev', located at the root
;; of this repository.
;;
;; To install these additional dependencies, edit `Eldev' and then call:
;;
;;   eldev upgrade
;;
;; To double-check the packages which are installed for this
;; live-environment, use:
;;
;;   eldev dependencies emacs
;;


;;; Code:

(setq init-dir  (file-name-directory user-init-file))

;; Some sane keyboard defaults
(setq select-enable-clipboard t
      save-interprogram-paste-before-kill t
      mark-even-if-inactive nil
      delete-active-region 'kill
      visible-bell nil
      ring-bell-function 'ignore
      delete-selection-temporary-region 'selection
      shift-select-mode t)
(delete-selection-mode)
(put 'narrow-to-region 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)

(use-package vertico
  :demand t
  :config
  (setq completions-detailed t)
  (vertico-mode))

(use-package marginalia
  :after vertico
  :config
  (marginalia-mode +1)
  (setq marginalia-annotators
        '(marginalia-annotators-heavy marginalia-annotators-light nil)))

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic)))

(use-package org-id
  :custom
  (org-id-link-to-org-use-id 'create-if-interactive))

(use-package all-the-icons
  :hook
  (marginalia-mode . all-the-icons-completion-marginalia-setup))

(require 'all-the-icons nil t)

(use-package org-roam
  :custom
  (org-return-follows-link t)
  (org-startup-folded t)
  (org-support-shift-select 'always)
  (org-roam-database-connector 'sqlite-builtin)
  (org-roam-directory
   ;; Zettelkasten is located at ../zettelkasten:
   (file-name-concat
    (file-name-parent-directory init-dir)
    "zettelkasten"))
  :config
  (org-roam-db-autosync-mode +1)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n n" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)))

(use-package delve
  :hook
  (delve-mode . delve-compact-view-mode)
  (delve-mode . hl-line-mode)
  :custom
  (delve-dashboard-tags '("concept" "idea" "definition"))
  :bind
  (("<f12>" . delve)))

(use-package delve-minor-mode
  :init
  (setq delve-minor-mode-prefix-key (kbd "C-."))
  :config
  (delve-global-minor-mode +1))


;; (provide 'init)
;;; init.el ends here
