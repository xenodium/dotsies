;;; -*- lexical-binding: t; -*-
(use-package s
  :ensure t
  :defer 60)

(use-package ht
  :ensure t
  :defer 60)

(use-package async
  :ensure t
  :defer 60
  :config
  (async-bytecomp-package-mode +1))

;; Timestamp and date/time library.
(use-package ts
  :ensure t
  :defer 60)

(use-package f
  :ensure t
  :defer 60)
