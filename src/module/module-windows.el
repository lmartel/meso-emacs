;;; module-windows.el --- Manipulating windows and frames.   -*- lexical-binding: t -*-

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

;; In Emacs-land, "frames" are OS-level windows (or terminal windows/tabs)
;; and "windows" are the panes within each frame.
;; None of this code has anything to do with Microsoft Windows;
;; use Linux Subsystem for Windows if you want to use Meso Emacs on Windows.

;;; Code:

(use-package popwin :demand t
             :bind-keymap (("C-z" . popwin:keymap))
             :config
             (popwin-mode 1))


(provide 'module-windows)
;;; module-windows.el ends here
