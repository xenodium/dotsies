;;; -*- lexical-binding: t; -*-

(use-package ar-info
  :commands ar/format-info-mode)

(use-package info-look
  :commands (info-lookup-symbol
             info-lookup-maybe-add-help)
  :config
  (add-to-list 'Info-directory-list "~/.emacs.d/info"))
