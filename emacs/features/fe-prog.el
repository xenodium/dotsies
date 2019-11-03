;;; -*- lexical-binding: t; -*-
(use-package prog-mode
  :after (flycheck)
  :bind (:map
         prog-mode-map
         ([f6] . recompile)
         ("C-x C-q" . view-mode))
  :hook ((prog-mode . company-mode)
         (prog-mode . flycheck-mode)
         (prog-mode . flyspell-prog-mode)
         (prog-mode . yas-minor-mode)
         ;; Trying out native alternative.
         (prog-mode . centered-cursor-mode)
         (prog-mode . rainbow-mode)
         (prog-mode . goto-address-prog-mode))
  :config
  (require 'flyspell)
  (require 'flycheck)

  (use-package insert-shebang
    :ensure t
    :commands insert-shebang)

  ;; Highlight hex strings in respective color.
  (use-package rainbow-mode
    :ensure t))
