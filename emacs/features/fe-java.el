;;; -*- lexical-binding: t; -*-

(use-package kotlin-mode
  :mode ("\\.kts?\\'" . kotlin-mode)
  :bind (:map
         kotlin-mode-map
         ("C-c C-c" . ar/compile))
  :ensure t)
