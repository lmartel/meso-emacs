;;; module-pandoc.el --- Pandoc is a file format converter, useful for e.g. exporting org or markdown docs to word or pdf.   -*- lexical-binding: t -*-

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

;; You can also use pandoc directly on the command line.

;;; Code:
(require 'meso)
(require 'meso-utils)

(use-package pandoc-mode
  :ensure-system-package pandoc
  :config
  (add-hook 'markdown-mode-hook #'pandoc-mode)
  (add-hook 'org-mode-hook #'pandoc-mode)

  ;; When starting pandoc-mode, check for and load default settings for project/file
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
  (with-eval-after-load "pandoc-mode"
    (define-key pandoc-mode-map (kbd "C-c P") #'pandoc-main-hydra/body)
    ;; TODO: regenerate hydra with custom commands injected into heads
    ;; (add-to-list 'pandoc-main-hydra/heads '("A" pandoc-autosave-mode nil :exit t))
    ;; For now, just use a different keybind
    (define-key pandoc-mode-map (kbd "C-c C-P") #'pandoc-autosave-mode)))

(define-minor-mode pandoc-autosave-mode
  "Run a pandoc export with the current settings after save."
  :init-value nil
  :lighter " Pandoc/AUTO"
  :after-hook
  (if pandoc-autosave-mode
      (add-hook 'after-save-hook #'pandoc-main-hydra/pandoc-run-pandoc-and-exit :run-last :buffer-local)
    (remove-hook 'after-save-hook #'pandoc-main-hydra/pandoc-run-pandoc-and-exit :buffer-local)))


(provide 'module-pandoc)
;;; module-pandoc.el ends here
