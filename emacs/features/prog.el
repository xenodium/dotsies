(use-package prog-mode
  :hook ((prog-mode . company-mode)
         (prog-mode . flyspell-prog-mode))
  :config
  (use-package flyspell))
