;;; module-persistence.el --- Improve Emacs' ability to automatically save and restore your session on restart.   -*- lexical-binding: t -*-

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

;; Hello world!

;;; Code:
(require 'meso)
(require 'meso-utils)

;; Save scratch buffer contents between sessions
(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))

;; TODO saved perspectives / window layouts go here

(use-package restart-emacs
  :config
  (global-set-key (kbd "C-c C-r") #'restart-emacs))

(provide 'module-persistence)
;;; module-persistence.el ends here
