# -*- mode: snippet -*-
# name: use-package
# key: use-package
# --
(use-package $1
  :commands (${2:$1-mode})
  :bind (("$3" . ${4:$2})) ; :bind implies :commands
  :diminish ${5:$2}
  :init
  $0
  :config)
