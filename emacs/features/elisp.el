(use-package elisp-mode
  :commands emacs-lisp-mode
  :hook (emacs-lisp-mode . pcre-mode)
  :config
  (require 'simple)
  (require 'ar-csetq)
  ;; From https://github.com/daschwa/emacs.d
  ;; Nic says eval-expression-print-level needs to be set to nil (turned off) so
  ;; that you can always see what's happening.
  (ar/csetq eval-expression-print-level nil)

  ;; make ELisp regular expressions more readable.
  (use-package easy-escape :ensure t
    :hook (emacs-lisp-mode . easy-escape-minor-mode)
    :config
    ;; TODO: Figure out why face foreground isn't displayed.
    (set-face-attribute 'easy-escape-face nil :foreground "red")
    (ar/vsetq easy-escape-character ?⑊)))
