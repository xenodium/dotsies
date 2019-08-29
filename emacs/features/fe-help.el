(require 'ar-vsetq)
(require 'ar-csetq)

(use-package help
  :config
  ;; Select help window by default.
  (ar/vsetq help-window-select t))

(use-package helpful
  :ensure t
  :bind  (("C-h f" . helpful-callable)
          ("C-h v" . helpful-variable)
          ("C-h k" . helpful-key))
  :config
  (use-package elisp-demos
    :ensure t
    :config
    (advice-add 'helpful-update
                :after
                #'elisp-demos-advice-helpful-update))

  (ar/csetq helpful-switch-buffer-function #'ar/helpful-switch-to-buffer)

  ;; from https://d12frosted.io/posts/2019-06-26-emacs-helpful.html
  (defun ar/helpful-switch-to-buffer (buffer-or-name)
    "Switch to helpful BUFFER-OR-NAME.

The logic is simple, if we are currently in the helpful buffer,
reuse it's window, otherwise create new one."
    (if (eq major-mode 'helpful-mode)
        (switch-to-buffer buffer-or-name)
      (pop-to-buffer buffer-or-name))))

(use-package tldr
  :ensure
  :commands (tldr
             tldr-update-docs))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))
