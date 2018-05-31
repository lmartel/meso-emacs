;;; module-text.el --- Settings for editing non-code: markdown, plain text, etc.   -*- lexical-binding: t -*-

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

;; This module sticks to general-purpose text editing.
;; For org-mode, grab the orgmode module in addition to this one.

;;; Code:
(require 'meso)
(require 'meso-utils)

(setq-default fill-column 80)
(use-package visual-fill-column
  :config
  (setq-default visual-fill-column-center-text t)
  (add-hook 'text-mode-hook #'turn-on-visual-line-mode)
  (add-hook 'visual-line-mode-hook #'turn-on-visual-fill-column-mode)
  (add-hook 'visual-fill-column-mode-hook #'ruler-mode))

(add-hook 'text-mode-hook #'turn-on-flyspell)

(provide 'module-text)
;;; module-text.el ends here
