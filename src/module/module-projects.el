;;; module-projects.el --- Managing and switching between different projects.   -*- lexical-binding: t -*-

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

;; Projectile can detect projects by the presence of a .git root.
;; Alternatively, you can add an empty .projectile file to denote the root of a
;; project or nested subproject. There's also a special syntax within the .projectile file,
;; similar to gitignore, that helps limit where search-in-project commands will look.

;;; Code:
(require 'meso)
(require 'meso-utils)

(use-package projectile
  :bind ("C-c p" . projectile-switch-project)
  :init
  (setq projectile-keymap-prefix "")
  :config
  (projectile-global-mode)
  :diminish projectile-mode)

(provide 'module-projects)
;;; module-projects.el ends here
