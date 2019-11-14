(use-package markdown-mode
  :ensure t
  :mode (("\\.text\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode))
  :hook ((markdown-mode . ar/markdown-mode-hook)
         (markdown-mode . ar/whitespace-mode-enable))
  :config
  (defun ar/markdown-mode-hook ()
    "Called when entering `markdown-mode'."
    (set-fill-column 80)))
