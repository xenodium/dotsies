(use-package prog-mode
  :bind (:map
         prog-mode-map
         ([f6] . recompile))
  :hook ((prog-mode . company-mode)
         (prog-mode . flycheck-mode)
         (prog-mode . flyspell-prog-mode)
         (prog-mode . yas-minor-mode)
         (prog-mode . centered-cursor-mode)
         (prog-mode . rainbow-mode)
         (prog-mode . goto-address-prog-mode))
  :config
  (require 'flyspell)
  (require 'flycheck)

  ;; Highlight hex strings in respective color.
  (use-package rainbow-mode
    :ensure t)

  ;; Make all prog mode buffers read-only by default.
  ;; Nicer for navigation.
  (add-hook 'prog-mode-hook
            (lambda ()
              (read-only-mode +1)) t))
