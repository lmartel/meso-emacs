;;; module-lisp.el --- Non-dialect-specific configs for the Lisp family of programming languages.   -*- lexical-binding: t -*-

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

;; Emacs Lisp is a great place to start if you haven't used a Lisp before!
;; These packages handle all the parentheses and other complicated business
;; for you.

;;; Code:
(require 'meso)
(require 'meso-utils)

;; The big one: Paredit, for structured editing of (S-expressions).
;; Paredit reference: http://pub.gajendra.net/src/paredit-refcard.pdf
;; TODO integrate smartparens for non-lisp langs
(use-package paredit
  :commands (enable-paredit-mode paredit-mode))

(use-package aggressive-indent
  :commands aggressive-indent-mode
  :diminish aggressive-indent-mode)

(use-package highlight-parentheses
  :commands highlight-parentheses-mode
  :init
  (setq hl-paren-colors '("Springgreen3"
                          "IndianRed1"
                          "IndianRed3"
                          "IndianRed4"))
  :diminish highlight-parentheses-mode)


(defun meso/lisp/add-hooks (mode-name)
  "Set up Lisp editing hooks for MODE-NAME."
  (let ((mode-hook-name (intern (format "%s-hook" mode-name))))
    (add-hook mode-hook-name #'aggressive-indent-mode)
    (add-hook mode-hook-name #'eldoc-mode) ; Lisp documentation lookup in the minibuffer
    (add-hook mode-hook-name #'enable-paredit-mode)
    (add-hook mode-hook-name #'highlight-parentheses-mode)))

(provide 'module-lisp)
;;; module-lisp.el ends here
