;;; -*- lexical-binding: t; -*-
(use-package s
  :ensure t
  :defer)

(use-package ht
  :ensure t
  :defer)

(use-package async
  :ensure t
  :defer
  :config
  (async-bytecomp-package-mode +1))

;; Timestamp and date/time library.
(use-package ts
  :ensure t
  :defer)

(use-package f
  :ensure t
  :defer)
