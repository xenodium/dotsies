;;; -*- lexical-binding: t; -*-

(use-package info-colors
  :ensure t
  :hook ((Info-selection . info-colors-fontify-node)))

(use-package help
  :validate-custom
  ;; Select help window by default.
  (help-window-select t))

(use-package discover-my-major
  :ensure t
  :commands (discover-my-major
             discover-my-mode))

(use-package helpful
  :ensure t
  :bind  (("C-h f" . helpful-callable)
          ("C-h v" . helpful-variable)
          ("C-h k" . helpful-key))
  :validate-custom
  (helpful-switch-buffer-function #'ar/helpful-switch-to-buffer)
  :init
  ;; from https://d12frosted.io/posts/2019-06-26-emacs-helpful.html
  (defun ar/helpful-switch-to-buffer (buffer-or-name)
    "Switch to helpful BUFFER-OR-NAME.

The logic is simple, if we are currently in the helpful buffer,
reuse it's window, otherwise create new one."
    (if (eq major-mode 'helpful-mode)
        (switch-to-buffer buffer-or-name)
      (pop-to-buffer buffer-or-name)))
  :config
  (use-package elisp-demos
    :ensure t
    :config
    (advice-add 'helpful-update
                :after
                #'elisp-demos-advice-helpful-update)))

(use-package tldr
  :ensure t
  :commands (tldr
             tldr-update-docs))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))
