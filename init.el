;;; init.el --- This is the customization file emacs looks for on startup, that loads everything else.   -*- lexical-binding: t -*-

;;; Commentary:

;; What is meso: TODO
;; How to customize: TODO

;;; Code:

(defun meso/user-init ()
  "This initialization function runs before all other setup. \
This is a good place to change how that setup happens, \
e.g. by changing file paths or variables that are read at load time. \
When in doubt, add your code in meso/user-config, not here."

  ;; Show a stack trace when an error is raised. This is built into Emacs.
  ;; I recommend to turn this on when developing modules or heavily editing configs,
  ;; and off otherwise to improve usability. Most day-to-day errors aren't showstopping.
  (setq debug-on-error t)

  ;; Settings with the user/ prefix are custom settings used by Meso and its modules.
  ;; TODO fixup these with init/config secondary prefix to clarify when they're read
  (setq user/full-name "Leo Martel")
  (setq user/email "leo@lpm.io")
  (setq user/appearance/font "Fira Code Retina 12")
  (setq user/lang/javascript-assume-jsx t)

  (setq user/init/use-cmd-as-meta t)

  ;; Your init code here!
  
  
  )

(defun meso/user-modules ()
  "Choose modules to load. \
Load modules with `(meso/load-module module-name)' or skip modules with `(meso/skip-module module-name)'. \
You need to load or explicitly skip every module; to disable this, add `(setq meso/force-explicit-skip-module nil)` \
to your meso/user-init function."

  (meso/load-module appearance)
  (meso/load-module backups)
  (meso/load-module better-defaults)
  (meso/load-module code)
  (meso/load-module emacs-development)
  (meso/load-module gui)
  (meso/load-module helm)
  (meso/load-module help)
  (meso/load-module ide)
  (meso/load-module lisp)
  (meso/load-module mac)
  (meso/load-module orgmode)
  (meso/load-module pandoc)
  (meso/load-module persistence)
  (meso/load-module projects)
  (meso/load-module text)
  (meso/load-module theme-samples)
  (meso/load-module windows)

  (meso/load-module lang-emacs-lisp)

  (meso/load-module lang-javascript)
  (meso/load-module lang-python)
  (meso/load-module lang-scala))


(defun meso/user-config ()
  "This configuration function runs after all other setup. \
This is a good place to customize behavior, e.g. by overriding \
variable defaults, adding new keybinds or hooks, etc. \
When in doubt, add your code here."

  ;; Your config code here!
  (when (require 'asana nil :noerror)
    (global-asana-mode 1)))

(defun meso/init ()
  "Run user and framework init code. You shouldn't need to modify this function."
  (package-initialize)
  (meso/user-init)
  (add-to-list 'load-path (concat user-emacs-directory "src/"))
  (require 'meso)
  (meso/user-modules)
  (when meso/user-src-dir
    (meso/add-to-load-path-with-subdirs meso/user-src-dir :try-mkdir))
  (meso/user-config)
  (when custom-file
    (load custom-file)))

;; Run the init function. Don't forget this!
(meso/init)

;;; init.el ends here
