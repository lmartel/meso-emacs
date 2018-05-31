;;; module-better-defaults.el --- Modernize and improve Emacs defaults, conservatively.  -*- lexical-binding: t -*-

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

;; You can change these here, but may prefer to override them in meso/user-config
;; in your init.el.

;;; Code:
(require 'meso)
(require 'meso-utils)

;; Use the popular better-defaults package as a starting point.
;; See https://github.com/technomancy/better-defaults for what this does.
(use-package better-defaults)

;; Use UTF-8 everywhere.
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(load-library "iso-transl")

;; Put `customize' settings in their own file instead of at the bottom of init.el
(setq custom-file (f-join user-emacs-directory "user" "custom.el"))
(f-mkdir (f-dirname custom-file))
(f-touch custom-file)

;; Be less annoying
(defalias 'yes-or-no-p 'y-or-n-p) ; Always ask for y/n instead of yes/no
(setq custom-buffer-done-kill t)

;; Simplify the start screen, and don't assume scratch buffer is elisp
(setq initial-major-mode 'text-mode)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; Always run a server, so you can open files from terminal with emacsclient
(server-force-delete)
(server-start)


;; Set up some startup hooks (code to run at the very end of initialization)
(defun meso--quit-and-bury (buffer-or-name)
  "If buffer BUFFER-OR-NAME is visible in a window, quit the window. Either way, bury the buffer if it exists."
  (when-let ((window (get-buffer-window buffer-or-name)))
    (quit-window nil window))
  (when-let ((buffer (get-buffer buffer-or-name)))
    (bury-buffer buffer-or-name)))

(defun meso--bury-compile-log ()
  "Quit and bury the *Compile-Log* window and buffer."
  (meso--quit-and-bury "*Compile-Log*"))
(add-hook 'emacs-startup-hook #'meso--bury-compile-log)

(defun meso--say-hello ()
  "Initialization message."
  (message "Finished initializing Emacs %s." emacs-version))
(add-hook 'emacs-startup-hook #'meso--say-hello :append)


;; Finally, set some useful miscellaneous keybinds.
(global-set-key (kbd "C-x C-|") #'align-regexp)

;; There's a built-in `kill-this-buffer' but the docstring warns that
;;   "This command can be reliably invoked only from the menu bar,
;;   otherwise it could decide to silently do nothing."
;; So, we use our own function.
(defun meso/kill-this-buffer ()
  "Reliably kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") #'meso/kill-this-buffer) ; default: kill-buffer


(provide 'module-better-defaults)
;;; module-better-defaults.el ends here
