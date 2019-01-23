;;; module-appearance.el --- Fonts and colors and stuff. -*- lexical-binding: t -*-

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

;; Dress for the code you want, not the code you have.
;; To avoid bloat, this module does not include any specific themes;
;; see the separate `theme-samples' module for some of those.
;; Instead, this module provides tools to customize and manage fonts, themes etc.

;;; Code:
(require 'meso)
(require 'meso-utils)

(meso--require-set user/appearance/font "Set default font name and size. Format eg: \"Font Name 11\". Explicitly set this to `nil' to use Emacs' default font.")

;; Font setup
(defun meso--maybe-set-font (font)
  "Set font for all frames to FONT if it's available."
  (if (or (member font (font-family-list))
          (and (window-system) (x-list-fonts font)))
      (progn (set-frame-font font nil t)
             (message "Successfully set default font to %s" font)
             t)
    (progn (message "Font %s not found; default font not changed." font)
           nil)))
(when user/appearance/font
  (meso--maybe-set-font user/appearance/font))


;; Themes setup. Use M-x customize-themes to choose a theme.
;; Hide built-in themes from the list, they're not good.
(delete t custom-theme-load-path)

;; If a theme is available as a package, add it here or in your init.el file.
;; (meso/install-theme your-theme) ; example

;; Otherwise, download the .el file and put it in here: ~/.emacs.d/themes/
(setq custom-theme-directory (meso--f-join-dir user-emacs-directory "themes"))
(f-mkdir custom-theme-directory)

(defun meso/disable-all-themes ()
  "Quickly disable all currently enabled themes."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(meso--set-if-unset meso/default-theme-no-warn nil "If non-nil, don't warn when using Emacs' default colors.")

;; Explain how to use themes if no theme active.
(defun meso--detect-default-theme ()
  "Check if `custom-enabled-themes' is set; if not, explain how to do so."
  (unless custom-enabled-themes
    (warn "[Meso] This is the default Emacs color scheme. Use M-x customize-themes to preview \
and select a custom theme. Find new ones at https://emacsthemes.com/. You can also disable this message \
by adding `(setq meso/default-theme-no-warn t)' to your init.el file.")))

(add-hook 'after-init-hook #'meso--detect-default-theme)

;; Many theme packages include "extra" files that Emacs detects as loadable themes.
;; For example, solarized-theme includes valid "solarized-light" and "solarized-dark"
;; themes but also an invalid "solarized" theme that throws an error when selected.
;; This code filters invalid themes out of the list in M-x customize-theme.
(defun meso--filter-valid-themes (themes)
  "Filter list of theme names THEMES down to only valid, loadable themes."
  (--filter (progn (load-theme it :no-confirm :no-enable)
                   (custom-theme-p it))
            themes))
(advice-add 'custom-available-themes :filter-return #'meso--filter-valid-themes)

(provide 'module-appearance)
;;; module-appearance.el ends here
