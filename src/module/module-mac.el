;;; module-mac.el --- Fix and improve Emacs behavior on Mac.   -*- lexical-binding: t -*-

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

;; When life gives you Mac OS...

;;; Code:
(require 'meso)
(require 'meso-utils)

(defvar user/init/use-cmd-as-meta nil "If t, use cmd as meta and option (alt) as super in macOS. If nil, option is meta and cmd is super. Default nil.")


(defun meso/mac-gui (&optional frame)
  "Return t if FRAME is a macOS GUI window (not an X window or terminal window)."
  (memq (window-system frame) '(mac ns)))


;; Map modifiers as specified, if available
(when (symbolp 'mac-control-modifier)
  (if user/init/use-cmd-as-meta
      (progn (setq mac-control-modifier 'control)
             (setq mac-option-modifier 'super)
             (setq mac-command-modifier 'meta))
    (progn (setq mac-control-modifier 'control)
           (setq mac-option-modifier 'meta)
           (setq mac-command-modifier 'super))))

;; Provide system-standard shortcuts IF you're using command as super.
;; We don't bind these if command is meta, because they don't make
;; sense on option as s-* bindings while M-* bindings are used elsewhere.
;; Consider cmd-as-meta a "power user" setting and take the chance to
;; learn the Emacsy equivalents to these shortcuts.
(unless user/init/use-cmd-as-meta
  (global-set-key (kbd "s-a") #'mark-whole-buffer)
  (global-set-key (kbd "s-v") #'yank)
  (global-set-key (kbd "s-c") #'kill-ring-save)
  (global-set-key (kbd "s-s") #'save-buffer)
  (global-set-key (kbd "s-l") #'goto-line)
  (global-set-key (kbd "s-w")
                  (lambda () (interactive) (delete-window)))
  (global-set-key (kbd "s-z") #'undo))

;; This seems not to actually show the menu bar in emacs-mac,
;; but it does fix the Emacs <> Mission Control interaction issue
;; described here: https://github.com/railwaycat/homebrew-emacsmacport/issues/139
(when (meso/mac-gui)
  (menu-bar-mode 1))

(provide 'module-mac)
;;; module-mac.el ends here
