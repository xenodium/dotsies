(use-package ar-info
  :commands ar/format-info-mode)

(use-package info-look
  :config
  (add-to-list 'Info-directory-list "~/.emacs.d/info"))
