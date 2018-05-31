;;; module-help.el --- Tooltips, cheat sheets, keybind help and discovery. -*- lexical-binding: t -*-

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

;; Hello world!

;;; Code:
(require 'meso)
(require 'meso-utils)

(use-package which-key
  :demand t ; Don't lazy load
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.4)
  (setq which-key-idle-secondary-delay 0.01) ; 0 is buggy
  (setq which-key-special-keys nil) ; Don't use weird shorthand for SPC etc
  :bind ("C-h C-k" . which-key-show-top-level)
  :diminish which-key-mode)


(defcustom user/mode-help-overrides ()
  "Association list of major modes where help popups should defer by default to a particular
more complicated minor mode."
  :group 'user
  :type '(alist :key-type symbol :value-type symbol))

(defun user/discover-my-mode ()
  "Discover the current major mode, or a prominent minor mode." ; TODO describe all non-diminished minor modes, as well.
  (interactive)
  (discover-my-mode (or (alist-get major-mode user/mode-help-overrides) major-mode)))

(use-package discover-my-major
  :bind (("C-h <C-m>" . user/discover-my-mode)
         ("C-h C-M" . discover-my-major)
         ("C-h M-m" . discover-my-mode)))

;; Add bindings for Dash, an offline documenation browser for Mac, if installed
(let ((dash-installed
       (and (equal system-type 'darwin)
            (s-present? (meso/shell "defaults read com.apple.LaunchServices/com.apple.launchservices.secure | grep com.kapeli.dash")))))
  (when dash-installed
    (use-package dash-at-point
      :bind (("C-c d" . dash-at-point)
             ("C-c e" . dash-at-point-with-docset)))))

(provide 'module-help)
;;; module-help.el ends here
