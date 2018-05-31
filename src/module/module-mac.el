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

;; Line the modifiers up reasonably
(when (symbolp 'mac-control-modifier)
  (setq mac-control-modifier 'control)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

;; Set all the usual keybinds
(global-set-key (kbd "s-a") #'mark-whole-buffer)
(global-set-key (kbd "s-v") #'yank)
(global-set-key (kbd "s-c") #'kill-ring-save)
(global-set-key (kbd "s-s") #'save-buffer)
(global-set-key (kbd "s-l") #'goto-line)
(global-set-key (kbd "s-w")
                (lambda () (interactive) (delete-window)))
(global-set-key (kbd "s-z") #'undo)

(provide 'module-mac)
;;; module-mac.el ends here
