(use-package s
  :ensure t)

(use-package async
  :ensure t
  :config
  (async-bytecomp-package-mode +1))

(use-package validate
  :ensure t
  :config
  (use-package ar-vsetq))
