;;; module-lang-python.el --- Python language.   -*- lexical-binding: t -*-

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

;; Python development tools.

;;; Code:
(require 'meso)
(require 'meso-utils)

(use-package anaconda-mode
  :init
  (add-hook 'python-mode-hook #'anaconda-mode))

(use-package company-anaconda
  :after company
  :init
  (meso--hook-localize-list 'python-mode-hook 'company-backends 'company-anaconda))

(provide 'module-lang-python)
;;; module-lang-python.el ends here
