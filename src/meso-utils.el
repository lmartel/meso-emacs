;;; meso-utils.el --- Helper functions for writing Meso modules, and modern Emacs Lisp generally.   -*- lexical-binding: t -*-

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

;; Helpers in this file should have no or few dependencies.
;; Complicated functionality deserves its own module!

;; The meso-- prefix with two dashes signifies a "private" function,
;; designed to be called from module implementation code rather than
;; by user configuration.

;;; Code:

(require 'use-package)

;; Modern collection, file and string manipulation libs
(use-package dash)
(use-package f)
(use-package s)

(defun meso/shell (command)
  "Run shell COMMAND and return trimmed output."
  (s-chomp (shell-command-to-string command)))

(defmacro meso/install-theme (theme)
  "Install, but do not load, theme THEME. This lets you build a library of \
themes to choose between in customize-theme without enabling all of them at once."
  `(let ((packaged-themes-dir (meso--f-join-dir custom-theme-directory "elpa")))
     (add-to-list 'custom-theme-load-path 
                  (meso--package-download ,theme packaged-themes-dir))))


(defmacro meso--require-set (var &optional desc)
  "Assert symbol VAR is defined. Set the docstring to DESC if it is, and warn with a clear error message if it isn't."
  `(if (boundp (quote ,var))
       (progn (when ,desc
                (put (quote ,var) 'variable-documentation ,desc))
              ,var)
     (progn (warn "[Meso] Error loading file %s: module expected variable `%s' to be defined, but got void. Please set an explicit value in init.el (nil is fine). Docstring: %s"
                  ,(f-filename (f-this-file)) (quote ,var) ,desc)
            (defvar ,var nil ,desc)
            nil)))

(defmacro meso--set-if-unset (var value &optional desc)
  "If VAR is not bound, set its value globally to VALUE. Set the docstring to DESC either way."
  `(progn (unless (boundp (quote ,var))
            (defvar ,var ,value ,desc))
          (when ,desc
            (put (quote ,var) 'variable-documentation ,desc))
          ,value))

(defun meso--f-join-dir (&rest args)
  "Join ARGS to a single path with f-join, but ensure exactly one trailing slash at the end."
  (s-append "/" (s-chop-suffix "/ "(apply 'f-join args))))

(defun meso--noop (&rest args)
  "A simple function that accepts any number of arguments and does nothing."
  nil)

(defmacro meso--let-noop (fns &rest body)
  "Override all functions FNS to do nothing while executing &BODY, then \
restore their previous functionality."
  `(progn
     (dolist (fn ,fns)
       (message "advising %s" fn)
       (advice-add fn :override #'meso--noop))
     ,@body
     (dolist (fn ,fns)
       (advice-remove fn #'meso--noop))))

(defmacro meso--package-download (name dir &optional force-update)
  "Leverage package.el to download package NAME into directory DIR *without* loading it. Useful for themes.\
You must specify a non-default directory, because `package-initialize' will auto-load packages from the default directory. This will not reinstall packages that are already installed, unless FORCE-UPDATE is non-nil. Returns the installed package directory path."
  `(let ((old-package-dir package-user-dir)
         (old-package-alist package-alist))

     ;; Scan the target directory for packages in order to detect whether the package is already installed.
     (setq package-user-dir ,dir)
     ;; (package-initialize :no-activate)
     ;; (package-refresh-contents)
     (when (or ,force-update
               ;; (not (package-installed-p (quote ,name))) ;; TODO too slow
               (not (f-glob (format "%s-*" (quote ,name)) ,dir))
               )
       
       ;; These functions are pulled from `package-unpack'. We want to let package.el download and unpack the package
       ;; as well as generate autoloads etc, but prevent it from "activating" (loading).
       ;; We also skip compilation, which relies on the package being "activated."
       ;; This code is hacky and brittle, because it relies on the internals of package.el.
       ;; If this breaks, look at `package-unpack'.
       (meso--let-noop '(package-activate-1
                         package--compile
                         package--load-files-for-activation)
                       (if ,force-update
                           (package-reinstall (quote ,name))
                         (package-install (quote ,name) :dont-select))))

     ;; Clean up after ourselves: reset installed package list and package directory to their previous, proper values
     (setq package-alist old-package-alist)
     (setq package-user-dir old-package-dir)
     
     (let ((pkg-dir (car (-sort #'string> (f-glob (format "%s-*" (quote ,name)) ,dir)))))

       ;; We *do* want the package on the load-path, so we can `require' it later.
       ;; We just don't want to load it *now*. This is normally handled during "activation."
       (add-to-list 'load-path pkg-dir)
       pkg-dir)))

(defmacro meso--hook-localize-list (hook-name list-name value)
  "Add a hook to HOOK-NAME to localize list LIST-NAME and add VALUE to the localized list."
  (let ((hook-fn (intern (format "meso--localize-list--%s--%s" (eval list-name) (eval value)))))
    `(progn (defun ,hook-fn ()
              "Auto-generated by `meso--hook-localize-list'."
              (add-to-list (make-local-variable ,list-name) ,value))
            (add-hook ,hook-name (quote ,hook-fn)))))


(defun meso--try-copy-resource (from-name to-name &optional to-dir)
  "Try to copy file FROM-NAME from module/resource/ to TO-NAME in TO-DIR (default: project root) if file TO-NAME doesn't already exist there."
  (when-let ((from-file (f-join user-emacs-directory "src" "module" "resource" from-name))
             (-to-dir (or to-dir (vc-root-dir))) ; fail silently if `to-dir' is nil and `vc-root-dir' can't find a project root
             (to-file (f-join -to-dir to-name)))
    (unless (f-exists? to-file)
      (f-copy from-file to-file))))

(provide 'meso-utils)
;;; meso-utils.el ends here
