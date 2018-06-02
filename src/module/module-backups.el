;;; module-backups.el --- Keep more backups and autosaves, but make them less irritating.   -*- lexical-binding: t -*-

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

;; By default, Emacs makes a backup of each file once per session, on first save.
;; It keeps these in the same directory as the file and calls them `NAME~'

;; Emacs also maintains an autosave file for each currently unsaved file,
;; used to restore your work after a sudden crash.
;; These are also kept in your working directory as `#NAME#'

;;; Code:
(require 'meso)
(require 'meso-utils)

;; Centralized backups directory: ~/.emacs.d/backups/
(meso--set-if-unset user/backups-directory (meso--f-join-dir user-emacs-directory "backups") "Path to directory to store auto-save and backup files")

;; Create nested backup directories if they do not exist.
(--each '("auto-save"
          "auto-save-list"
          "on-session-first-save"
          "on-save"
          "on-quit")
  (f-mkdir (f-join user/backups-directory it)))

;; Centralize backups instead of littering in the workdir.
(setq backup-directory-alist `(("" . ,(meso--f-join-dir user/backups-directory "on-session-first-save"))))
(setq auto-save-file-name-transforms `((".*" ,(meso--f-join-dir user/backups-directory "auto-save") 'uniqify)))
(setq auto-save-list-file-prefix (f-join user/backups-directory "auto-save-list" "saves-"))

;; Now that the backup files are out of the way, make more of them
(setq version-control t       ;; Use version numbers for backups.
      kept-new-versions 10    ;; Number of newest versions to keep.
      kept-old-versions 2     ;; Number of oldest versions to keep.
      delete-old-versions t   ;; Don't ask to delete excess backup versions.
      backup-by-copying t     ;; Copy all files, don't rename them (works better w/ symlinks)
      vc-make-backup-files t) ;; Back up version-controlled files too.

;; Make backups on session first save (as normal), but also on each save.
(add-hook 'before-save-hook (apply-partially #'force-backup-of-buffer "on-save"))

;; Also back up modified buffers (that the user declined to save when prompted) on quit.
;; The auto-saved protect you from crashes, but let's protect you from yourself here as well.
(add-hook 'kill-emacs-hook (apply-partially #'backup-modified-buffers "on-quit"))

(defun force-backup-of-buffer (backup-subdirectory)
  "Back up buffer to BACKUP-SUBDIRECTORY within user/backups-directory."
  (let ((buffer-backed-up nil)
        (backup-directory-alist `(("" . ,(meso--f-join-dir user/backups-directory backup-subdirectory)))))
    (backup-buffer)))

;; For each file-backed modified buffer, make an in-place temporary
;; autosave file, back it up, then delete it.
(defun backup-modified-buffers (backup-subdirectory)
  "Make a special backup of each modified file-backed buffer in BACKUP-SUBDIRECTORY."
  (let ((modified-buffers (--filter (and (buffer-file-name it)
                                         (buffer-modified-p it))
                                    (buffer-list))))
    (--each modified-buffers (let* ((orig-file (buffer-file-name it))
                                    (orig-dir (file-name-directory orig-file))
                                    (orig-nondir (file-name-nondirectory orig-file))
                                    (temp-file (concat orig-dir "#" orig-nondir "#")))
                               (write-file temp-file)
                               (let ((buffer-backed-up t))
                                 (force-backup-of-buffer backup-subdirectory))
                               (delete-file temp-file)))))

;; TODO list-all-backups command w/ sort by date and select-to-restore

(provide 'module-backups)
;;; module-backups.el ends here
