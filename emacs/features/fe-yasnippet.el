;;; -*- lexical-binding: t; -*-
(use-package yasnippet
  :ensure t
  :validate-custom
  (yas-indent-line 'fixed)
  (yas-triggers-in-field t)
  :commands (yas-expand
             yas-minor-mode)
  :hook ((prog-mode . yas-minor-mode)
         (org-mode . yas-minor-mode))
  :config
  (use-package yasnippet-snippets
    :ensure t)

  (add-to-list 'yas-snippet-dirs
               "~/.emacs.d/yasnippets/personal" t)

  (yas-reload-all))

;; Displays yasnippet previous inline when cycling through results.
(use-package ivy-yasnippet
  :ensure t
  :commands ivy-yasnippet)
