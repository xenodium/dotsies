(use-package prog-mode
  :hook ((prog-mode . company-mode)
         (prog-mode . flycheck-mode)
         (prog-mode . flyspell-prog-mode)
         (prog-mode . yas-minor-mode)
         (prog-mode . centered-cursor-mode)
         (prog-mode . rainbow-mode))
  :config
  (require 'flyspell)
  (require 'flycheck)

  ;; Highlight hex strings in respective color.
  (use-package rainbow-mode
    :ensure t))
