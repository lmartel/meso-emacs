;;; module-gui.el --- Improve Emacs behavior in GUI windows.   -*- lexical-binding: t -*-

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

;; Some of this code is specific to one window system, e.g. X/XQuartz vs Mac native, etc.
;; It's all conditional based on what window system is detected, so all should be well.
;; I've tested on Mac and several flavors of Linux.

;;; Code:
(require 'meso)
(require 'meso-utils)

;; Distinguish C-i/C-m from TAB/RET (GUI only)
(when (window-system)
  (define-key input-decode-map [?\C-m] [C-m])
  (define-key input-decode-map [?\C-i] [C-i]))

;; Use alt as super in X
(when (eq window-system 'x)
  (setq x-alt-keysym 'super))

;; Ensure access to $PATH for finding binaries
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(provide 'module-gui)
;;; module-gui.el ends here
