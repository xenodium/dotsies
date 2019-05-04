(use-package s
  :ensure t)

(use-package ht
  :ensure t)

(use-package async
  :ensure t
  :config
  (async-bytecomp-package-mode +1)
  (dired-async-mode +1))

(use-package validate
  :ensure t
  :config
  (use-package ar-vsetq))

;; flet is no longer available. Use noflet as a replacement.
(use-package noflet
  :ensure t
  :after dash)
