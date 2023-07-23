;;; -*- lexical-binding: t; -*-

(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" . json-mode))

(use-package js
  :validate-custom
  (js-indent-level 2))

(use-package typescript-mode
  :ensure t
  :validate-custom
  (typescript-indent-level 2))
