;;; module-orgmode.el --- Org Mode: organize your entire life inside Emacs!   -*- lexical-binding: t -*-

;; Copyright (C) 2018 Leo Martel

;; Author: Leo Martel <leo@lpm.io>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org Mode is wonderful! Take the time to check it out.

;;; Code:
(require 'meso)
(require 'meso-utils)

(use-package org :ensure org-plus-contrib
  :init

  ;; Ignore packages that request "org" as a dependency, because
  ;; we want to install only 'org-plus-contrib, not 'org as well.
  (meso/package-dependency-check-ignore 'org)
  (setq-default org-directory (f-join user-emacs-directory "org"))

  :config

  ;; let OSX keep the s-* keybinds
  (setq org-replace-disputed-keys t)
  (setq org-startup-indented t)
  (setq org-startup-folded nil)

  (with-eval-after-load "diminish"
    (diminish 'org-indent-mode)))

;; I don't like gnuplot. TODO: use python for inline plotting, instead. From: http://ehneilsen.net/notebook/orgExamples/org-examples.html
;; (use-package gnuplot)


;; There's a shadowing bug that prevents installing org dependencies immediately after installing org itself.
;; The simplest solution is to install org, let emacs finish booting, then install dependencies later.
;; This code ensures that happens without errors.
(defun meso--load-org-extensions ()
  "Load org extensions. Loading these successfully requires org-plus-contrib to be loaded, \
not Emacs' built-in outdated org version."
  
  (use-package org-bullets
    :config
    (add-hook 'org-mode-hook #'org-bullets-mode))

  (use-package org-cliplink
    :config
    (define-key org-mode-map (kbd "C-c C-y") 'org-cliplink))

  ;; Additional packages to facilitate using org-mode as a journaling system, a personal wiki, a concept-mapping system, etc.
  (use-package org-brain)
  (use-package org-journal
    :init
    (setq org-journal-dir (f-join org-directory "journal"))))

(with-eval-after-load "org"
  (if (fboundp 'org-link-types)
      (meso--load-org-extensions)
    (warn "[Meso] Failed to install org-mode extensions because of a known package bug. To finish setting up module-orgmode, restart Emacs.")))


(provide 'module-orgmode)
;;; module-orgmode.el ends here
