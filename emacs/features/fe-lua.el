;;; -*- lexical-binding: t; -*-

(use-package lua-mode
  :mode ("\\.lua\\'" . lua-mode)
  :ensure t
  :config
  (use-package company-lua
    :ensure t))
