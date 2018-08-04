(use-package yasnippet
  :ensure t
  :commands (yas-expand
             yas-minor-mode)
  :config
  (require 'ar-vsetq)

  (use-package ar-yas
    :commands (ar/yas-install--github-yasnippets
               ar/yas-install-external-yasnippets))

  (use-package yasnippet-snippets
    :ensure t)

  (ar/vsetq yas-indent-line 'fixed)
  (ar/vsetq yas-snippet-dirs
         '("~/.emacs.d/yasnippets/personal"))

  (yas-reload-all))
