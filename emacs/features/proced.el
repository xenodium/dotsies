(require 'ar-csetq)

(use-package proced
  :commands proced
  :hook (proced-mode . ar/proced--hook-fun)
  :init
  (defun ar/proced--hook-fun ()
    (ar/csetq proced-auto-update-flag t)))
