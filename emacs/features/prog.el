(use-package prog-mode
  :init
  (defun ar/prog-mode-hook ()
    (company-mode +1))
  :hook (prog-mode . ar/prog-mode-hook))
