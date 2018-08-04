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
(meso/load-module ide)

;;; User-facing configuration variables and functions

(meso--set-if-unset user/lang/javascript-assume-jsx nil "Assume jsx is present in all JavaScript, even in files ending in .js rather than .jsx")

(defun meso/javascript-initialize-this-project ()
  "Copy some good default config files to the git project root of the current buffer. Won't overwrite existing files.
Creates: .eslintrc for `eslint' linting; .jsbeautifyrc for `web-beautify' reformatting; jsconfig.json for `tide' autocompletion."
  (interactive)
  (cl-loop for (filename . template) in '((".eslintrc" . "template.eslintrc")
                                          (".jsbeautifyrc" . "template.jsbeautifyrc")
                                          ("jsconfig.json" . "template.jsconfig.json"))
           do (meso--try-copy-resource template filename))
  (tide-setup))

;;; Packages, dependencies and npm

(use-package nvm
  :ensure-system-package node)

(defun meso/npm-ensure-g (package)
  "Ensure PACKAGE is installed using npm. Useful for packages that don't provide binaries."
  (when (s-blank? (meso/shell (format "npm ls -g | grep ' %s@'" package)))
    (async-shell-command (format "npm i -g %s" package))))

;;; General editing: major and minor modes

(define-minor-mode meso/js-minor-mode
  "Toggle js-minor-mode. The Meso JavaScript module uses different major modes for different kinds of 
files (e.g. embedded JSX vs not); they all hook into this minor mode. Add hooks and keymaps here to add them to
all JS editing modes.")

(use-package js2-mode
  :ensure-system-package node
  :interpreter "node"
  :commands js2-mode
  :mode (("\\.js\\'" . js2-mode)
         ("\\.ejs\\'" . js2-mode))
  :init
  (add-hook 'meso/js-minor-mode-hook #'meso/js-minor-mode))

(use-package rjsx-mode
  :commands rjsx-mode
  :init
  (add-hook 'meso/js-minor-mode-hook #'meso/js-minor-mode)
  (when user/lang/javascript-assume-jsx
    (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))))

(use-package json-mode) ; Intentionally don't load js-minor-mode hooks here.

;;; Linting, syntax checking, and gradual typechecking

(use-package flycheck
  :ensure-system-package (node (eslint . "npm i -g eslint"))
  :init
  ;; Disable jshint, prefer eslint.
  (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)
  (flycheck-add-mode 'javascript-eslint 'meso/js-minor-mode)
  :config
  (meso/npm-ensure-g "babel-eslint")
  (meso/npm-ensure-g "eslint-plugin-react"))

(use-package flow-minor-mode
  :ensure-system-package ((flow . "npm i -g flow-bin"))
  :after (js2-mode rjsx-mode)
  :commands (flow-minor-enable-automatically)
  :hook ((js2-mode . flow-minor-mode)
         (rjsx-mode . flow-minor-mode)))

(use-package flycheck-flow
  :after (flycheck flow-minor-mode)
  :config
  (flycheck-add-next-checker 'javascript-flow 'javascript-eslint))

(use-package company-flow
  :after (company flow-minor-mode)
  :init
  (add-to-list 'company-backends 'company-flow))

(use-package tide
  :after (company flycheck js2-mode rjsx-mode)
  :hook ((js2-mode . tide-setup)
         (rjsx-mode . tide-setup)))

;;; Refactoring and reformatting

(use-package js2-refactor
  :after js2-mode rjsx-mode
  :hook ((js2-mode . js2-refactor-mode)
         (rjsx-mode . js2-refactor-mode))
  :init
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (add-hook '-mode-hook #'js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m"))

(use-package web-beautify
  :ensure-system-package (node (js-beautify . "npm i -g js-beautify"))
  :commands web-beautify-js
  :init
  (with-eval-after-load 'meso/js-minor-mode
    (define-key meso/js-minor-mode-map (kbd "C-c C-b") 'web-beautify-js)))

(provide 'module-lang-javascript)
;;; module-lang-javascript.el ends here
