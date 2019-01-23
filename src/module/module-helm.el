;;; module-helm.el --- Incremental completion and selection narrowing.   -*- lexical-binding: t -*-

;; Copyright (C) 2018 Leo Martel
;; Author: Leo Martel <leo@lpm.io>

;; Copyright © 2014-2018 Tu, Do Hoang
;; Author: Tu, Do Hoang (tuhdo1710@gmail.com)

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

;; There are alternatives (Ido, Ivy) but Helm is quite popular and pretty good.
;; It has extensibility issues but works great for most needs out of the box.
;; Many of the keybinds are inspired by Prelude's `helm-everywhere'.
;;   See: https://github.com/bbatsov/prelude/blob/master/modules/prelude-helm-everywhere.el

;;; Code:
(require 'meso)
(require 'meso-utils)

;; recentf keeps track of recent files.
;; We set up a helm keybind below.
;; TODO - move this elsewhere and make it work without helm.
(use-package recentf
  :init
  (setq recentf-max-menu-items 20)
  (setq recentf-exclude '(".*-autoloads\\.el"))
  :config
  (recentf-mode 1))

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x C-m" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-h C-h" . helm-apropos)
         ("C-h r" . helm-info-emacs)
         ("C-h C-l" . helm-locate-library)
         ("C-x C-r" . helm-recentf)) 
  :config
  (require 'helm-config)
  (require 'helm) 
  (helm-mode 1)

  ;; More binds (non-global)
  (define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
  (define-key isearch-mode-map (kbd "C-o") 'helm-occur-from-isearch)
  (with-eval-after-load "shell-mode"
    (define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring))
  
  ;; Change behavior: fuzzy match everywhere
  (setq-default helm-M-x-fuzzy-match t
                helm-buffers-fuzzy-matching t
                helm-recentf-fuzzy-match t
                helm-apropos-fuzzy-match t)

  ;; Change looks
  (setq-default helm-display-header-line nil
                helm-split-window-in-side-p t))


(use-package helm-ag
  :ensure-system-package ag
  :bind (("C-c C-g" . helm-do-ag)))

(with-eval-after-load "module-projects"
  (use-package helm-projectile
    :bind (("C-c f" . helm-projectile-find-file-dwim)
           ("C-c g" . helm-projectile-ag)
           ("C-c b" . helm-projectile-switch-to-buffer)
           ("C-c p" . helm-projectile-switch-project))
    :config
    (helm-projectile-on)))

(use-package swiper-helm
  :bind (("C-S-s" . swiper-helm)))

(use-package helm-ext
  :config
  (helm-ext-ff-enable-skipping-dots t)
  (helm-ext-ff-enable-auto-path-expansion t)
  (helm-ext-minibuffer-enable-header-line-maybe t))

(with-eval-after-load "company"
  (use-package helm-company
    :commands helm-company
    :init
    (define-key company-mode-map (kbd "C-:") 'helm-company)
    (define-key company-active-map (kbd "C-:") 'helm-company)))


(provide 'module-helm)
;;; module-helm.el ends here
