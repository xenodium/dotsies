;;; -*- lexical-binding: t; -*-

(use-package proced
  :functions ar/proced--hook-fun
  :commands proced
  :hook (proced-mode . ar/proced--hook-fun)
  :config
  (defun ar/proced--hook-fun ()
    (setq proced-auto-update-flag t)))
