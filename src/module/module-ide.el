;;; -*- lexical-binding: t -*-
;;; module-ide.el --- Enable IDE-style functionality like autocomplete and error checking.

;; Copyright (C) 2017 Leo Martel

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

;; This configures general IDE functionality and packages.
;; You'll also need to enable language-specific modules to use enhanced IDE
;; functionality for those languages.

;;; Code:
(require 'meso)
(require 'meso-utils)

(use-package company
  :commands company-mode
  :init
  (setq company-idle-delay 0.2)         ; delay in seconds, default 0.5
  (setq company-tooltip-idle-delay 0.2) ; delay in seconds, default 0.5
  :config
  (global-company-mode)
  ;; This seemed like a good idea, but it overrides type-compatibility in
  ;; typechecked backends. EG: Math.Math as the top suggestion instead of
  ;; Math.abs because you've just typed "Math".
  ;; (setq company-transformers '(company-sort-by-occurrence))
  ;; TODO: explore company-sort-by-backend-importance
  :diminish company-mode)

(use-package company-quickhelp
  :config
  (company-quickhelp-mode 1))

(use-package company-try-hard
  :bind ("C-\\" . company-try-hard)
  :config
  (bind-keys :map company-active-map
             ("C-\\" . company-try-hard)))


(use-package flycheck
  :init
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (add-hook 'prog-mode-hook #'flycheck-mode))

(use-package flycheck-color-mode-line
  :init
  (with-eval-after-load "flycheck"
    (add-hook 'flycheck-mode-hook #'flycheck-color-mode-line-mode)))

(with-eval-after-load "module-helm"
  (use-package helm-flycheck
    :bind (("C-c ! !" . helm-flycheck))))

(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)

(use-package yasnippet
  :config
  (yas-global-mode 1)
  :diminish yas-minor-mode)


(provide 'module-ide)
;;; module-ide.el ends here
