;;; meso.el --- Bootstrap the package and module systems.   -*- lexical-binding: t -*-

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

;; This file bootstraps modern package and module management tools.
;; We also set up package hygene: missing packages are installed automatically,
;; and unneeded packages are deleted automatically.

;; This is the most dense file within Meso, because it sets up the simple helpers
;; used everywhere else. You shouldn't need to modify this file.

;;; Code:

;; Force a modern-ish version of Emacs
(when (< emacs-major-version 25)
  (error "Emacs version (%s) too old for Meso compatibility, stopping" emacs-version))

;; Add ~/.emacs.d/src/ and *all recursive descendants* to load-path.
(dolist (srcs-dir-name (list "src/"
			     ;; add more source directories here if needed!
			     ))
  (let ((default-directory (concat user-emacs-directory srcs-dir-name)))
    (add-to-list 'load-path default-directory)
    (normal-top-level-add-subdirs-to-load-path)))

;; Initialize the package system, and add useful package repositories.
(require 'package)  
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; Bootstrap package list on first run.
(unless package-archive-contents
  (package-refresh-contents))

;; In addition, automatically refresh package indices once per session, lazily.
(let ((package-contents-refreshed nil))
  (defun package-refresh-contents-once (&rest ignored)
    "Run package-refresh-contents if it hasn't been run before this session."
    (unless package-contents-refreshed
      (package-refresh-contents)))

  (defun set-package-contents-refreshed (&rest ignored)
    (setq package-contents-refreshed t))

  (advice-add 'package-refresh-contents :after #'set-package-contents-refreshed)
  (advice-add 'package-install :before 'package-refresh-contents-once)
  (advice-add 'package-list-packages :before 'package-refresh-contents-once))


;; Bootstrap use-package for installing and configuring packages.
(when (not (package-installed-p 'use-package))
  (package-install 'use-package))
(require 'use-package)
(setq-default use-package-always-ensure t) ; Always install missing packages.

;; Enhanced use-package functionality: let us require and auto-install system packages (command line tools) as well.
(use-package system-packages)
(use-package use-package-ensure-system-package)


;; List of symbols to ignore in dependency checks.
;; Add a package name to this list to prevent it from being automatically downloaded by use-package.
;; This is rarely needed, but (for example) it enables using `org-plus-contrib' as a drop-in replacement
;; for `org' without the latter being downloaded as well.
(let ((package-dependency-blacklist ()))

  (defun meso/package-dependency-check-ignore (pkg)
    "Don't install PKG in package-install dependency checks when another package requests it, if you know better than the package does."
    (add-to-list 'package-dependency-blacklist pkg))

  ;; from https://emacs.stackexchange.com/questions/26507/use-org-plus-contrib-to-satisfy-dependency-of-org2blog/26513#26513
  (defun meso/--package-dependency-check-ignore (orig-ret)
    "Remove the `black listed packages' from ORIG-RET.

Packages listed in the let-bound `package-dependency-blacklist' will not be auto-installed
even if they are found as dependencies.

It is known that this advice is not effective when installed packages
asynchronously using `paradox'. Below is effective on synchronous
package installations."
    (let (new-ret
          pkg-name)
      (dolist (pkg-struct orig-ret)
        (setq pkg-name (package-desc-name pkg-struct))
        (if (member pkg-name package-dependency-blacklist)
            (message (concat "Package `%s' will not be installed. "
                             "See `meso/package-dependency-check-ignore'.")
                     pkg-name)
          (push pkg-struct new-ret)))
      ;; It's *very* critical that the order of packages stays the same in NEW-RET
      ;; as in ORIG-RET. The `push' command flips the order, so use `reverse'
      ;; to flip the order back to the original.
      ;;   Without this step, you will get package activation errors when
      ;; installing packages with dependencies.
      (setq new-ret (reverse new-ret))
      new-ret))
  (advice-add 'package-compute-transaction :filter-return #'meso/--package-dependency-check-ignore))


;; Set up hooks for package-safe-delete to delete unused packages automatically.
;; This way you can manually install packages for testing and just restart to clean up.
;; If you like the package, add it to your init file or to a module.
(use-package package-safe-delete
  :demand t 
  :config
  (mapcar (lambda (package-name) (add-to-list 'package-safe-delete-required-packages package-name))
          '(paradox use-package package-safe-delete))
  (advice-add #'use-package 
              :around 
              (lambda (old-function package-name &rest options)
                "Add packages to the package-safe-delete required packages list when loaded with use-package"
                (let ((ensured (plist-get options ':ensure)))

                  ;; If ensured different package from package-name, protect that one instead
                  (if (not (booleanp ensured))
                      (add-to-list 'package-safe-delete-required-packages ensured)
                    (add-to-list 'package-safe-delete-required-packages package-name)))
                (apply old-function (cons package-name options)))))


;; Finally, set up the Meso module system. By default, you must load or explicitly skip all modules,
;; to ensure that you know what's available and that modules don't go missing and cause confusion.
(require 'meso-utils)

(meso--set-if-unset meso/force-explicit-skip-module t
                    "If non-nil, unwanted meso modules must be explicitly skipped with meso/skip-module.")


;; This cannot be bound to a clojure with `let' because macros use the lexical scope of their
;; *call site*, where they're compiled; not the scope where they're defined.
;; I learned here that defmacro inside let doesn't do what you want.
;; https://emacs.stackexchange.com/questions/5416/how-is-the-variable-scoping-for-macros-determined
(setq meso--referenced-modules ())

(defmacro meso/load-module (name)
  "Acknowledge and load source file: module-NAME.el."
  `(let ((module (intern (format "module-%s" (quote ,name)))))
     (require module)
     (add-to-list 'meso--referenced-modules module)))

(defmacro meso/skip-module (name)
  "Acknowledge, but do not load, source file: module-NAME.el."
  `(let ((module (intern (format "module-%s" (quote ,name)))))
     (add-to-list 'meso--referenced-modules module)))

(defun meso--audit-modules ()
  "After all initialization is finished, assert that all modules have been explicitly loaded or skipped."
  (if meso/force-explicit-skip-module
      (let* ((all-modules (f-entries (meso--f-join-dir user-emacs-directory "src" "module") (lambda (file) (s-ends-with? ".el" file)) :recursive))
	     (all-module-names (--map (intern (s-chop-suffix ".el" (f-filename it))) all-modules))
	     (missing-modules (-difference all-module-names meso--referenced-modules)))
	(if missing-modules
	    (warn "[Meso] Modules found in src/modules but omitted in init.el: %s. Must load with `load-module' \
or explicitly skip with `skip-module', or disable this warning by adding \
`(setq meso/force-explicit-skip-module nil)' to your init.el file.'." missing-modules)))))

(add-hook 'after-init-hook #'meso--audit-modules)

(provide 'meso)
;;; meso.el ends here
