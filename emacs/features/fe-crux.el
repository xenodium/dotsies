;;; -*- lexical-binding: t; -*-

(use-package crux
  :ensure t
  :commands crux-open-with
  :bind
  (("C-x C-d" . crux-duplicate-current-line-or-region)
   ("C-o" . crux-smart-open-line))
  :config
  ;; I used to define my own versions of these functions.
  ;; Crux packaged them up. Aliasing, to find faster.
  (defalias 'ar/transpost-windows #'crux-transpose-windows)
  (defalias 'ar/swap-windows #'crux-transpose-windows)
  (defalias 'ar/switch-windows #'crux-transpose-windows)
  (defalias 'ar/file-delete-current-file #'crux-delete-file-and-buffer)
  (defalias 'ar/file-rename-current #'crux-rename-buffer-and-file)
  (defalias 'ar/duplicate-line #'crux-duplicate-current-line-or-region))
