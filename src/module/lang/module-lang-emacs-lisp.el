;;; module-lang-emacs-lisp.el --- Tools for editing Emacs Lisp, configured in Emacs Lisp!   -*- lexical-binding: t -*-

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

;; Emacs Lisp specific configs and packages.
;; General Lisp editing tools can be found in the Lisp module.

;;; Code:
(require 'meso)
(require 'meso-utils)

(meso/lisp/add-hooks 'emacs-lisp-mode)

(with-eval-after-load "module-help"
  (add-to-list 'user/mode-help-overrides '(emacs-lisp-mode . paredit-mode)))

;; Evaluation Result OverlayS
(use-package eros
  :commands eros-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'eros-mode))

(provide 'module-lang-emacs-lisp)
;;; module-lang-emacs-lisp.el ends here
