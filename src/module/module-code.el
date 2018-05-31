;;; module-code.el --- General (non-language-specific) code editing settings.   -*- lexical-binding: t -*-

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

;; For language-specific modules, check out the lang/ subdirectory.

;;; Code:
(require 'meso)
(require 'meso-utils)

;; The dtrt-indent mode guesses and mimics existing indentation within source code files.
(use-package dtrt-indent
  :config
  (add-hook 'prog-mode-hook #'enable-dtrt-indent-mode))

(defun enable-dtrt-indent-mode ()
  "Enable buffer-local dtrt-indent-mode."
  (dtrt-indent-mode 1))

(defun disable-dtrt-indent-mode ()
  "Disable buffer-local dtrt-indent-mode."
  (dtrt-indent-mode 0))

(provide 'module-code)
;;; module-code.el ends here
