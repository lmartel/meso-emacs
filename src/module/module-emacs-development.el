;;; module-emacs-development.el --- Emacs packages for developing Emacs packages.   -*- lexical-binding: t -*-

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

;; General tools for editing Emacs Lisp can be found in the lang-emacs-lisp module.
;; This module is for preparing your code to be published as a package after you write it.

;;; Code:
(require 'meso)
(require 'meso-utils)

(use-package package-lint
  :commands (package-lint-buffer package-lint-current-buffer))

(use-package flycheck-package
  :after flycheck
  :commands flycheck-package-setup
  :init
  (with-eval-after-load 'flycheck
    (flycheck-package-setup)))

(provide 'module-emacs-development)
;;; module-emacs-development.el ends here
