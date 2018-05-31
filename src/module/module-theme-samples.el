;;; module-theme-samples.el --- Some fun themes.   -*- lexical-binding: t -*-

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

;; Just some fun themes! Install your own if you prefer.
;; We use the `install-theme' helper rather than `use-package'
;; to avoid auto-loading all the themes.

;;; Code:
(require 'meso)
(require 'meso-utils)

(meso/install-theme dracula-theme)
(meso/install-theme exotica-theme)
(meso/install-theme klere-theme)
(meso/install-theme material-theme)
(meso/install-theme moe-theme)
(meso/install-theme nord-theme)
(meso/install-theme solarized-theme)
(meso/install-theme ujelly-theme)

(provide 'module-theme-samples)
;;; module-theme-samples.el ends here
