;;; module-lang-javascript.el --- Javascript language.   -*- lexical-binding: t -*-

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

;; Javascript development tools.

;;; Code:
(require 'meso)
(require 'meso-utils)

(meso--set-if-unset user/lang/javascript-assume-jsx nil "Assume jsx is present in all javascript, even in files ending in .js rather than .jsx")


(use-package js2-mode
  :ensure-system-package node
  :interpreter "node"
  :commands js2-mode
  :mode (("\\.js\\'" . js2-mode)
         ("\\.ejs\\'" . js2-mode))
  :config
  nil ;; TODO - explore best js2-mode settings around indentation, strictness etc
  )

(defun meso/force-js2-mode () 
  "Force enable js2-mode, even if web mode is set to override it."
  (interactive)
  (remove-hook 'js2-mode-hook #'web-mode)
  (js2-mode)
  (when user/lang/javascript-assume-jsx
    (add-hook 'js2-mode-hook #'web-mode)))


(use-package js2-refactor
  :after js2-mode
  :init
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m"))


(use-package web-mode
  :commands web-mode
  :init
  (when user/lang/javascript-assume-jsx
    (add-hook 'js2-mode-hook #'web-mode)) 
  :config 
  (when user/lang/javascript-assume-jsx
    (add-to-list 'web-mode-content-types-alist '("jsx" . "\\.[jt]sx?\\'")))
  (with-eval-after-load "dtrt-indent"
    (add-hook 'dtrt-indent-mode-hook #'meso--web-mode-set-all-indents)))

;; Detect and avoid infinite recursion via web-mode hooks.
(let ((indents-already-set nil))
  (defun meso--web-mode-set-all-indents (&optional value)
    "Set all web-mode indent offsets (HTML, CSS, code) to VALUE, or detect from buffer settings if VALUE is nil."
    (if indents-already-set
        (setq indents-already-set nil)
      (let ((v (or value standard-indent)))
        (setq-default web-mode-markup-indent-offset v)
        (setq-default web-mode-css-indent-offset v)
        (setq-default web-mode-code-indent-offset v))
      (when (eq major-mode 'web-mode)
        (setq indents-already-set t)
        (web-mode))
      value)))


(use-package company-flow
  :after (js2-mode web-mode company)
  :ensure-system-package flow
  :init
  (meso--hook-localize-list 'js2-mode-hook 'company-backends 'company-flow)
  (meso--hook-localize-list 'web-mode-hook 'company-backends 'company-flow))

(use-package json-mode)

(provide 'module-lang-javascript)
;;; module-lang-javascript.el ends here
