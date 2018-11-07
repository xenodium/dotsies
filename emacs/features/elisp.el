(use-package elisp-mode
  :commands emacs-lisp-mode
  :hook ((emacs-lisp-mode . pcre-mode)
         (emacs-lisp-mode . ar/emacs-lisp-mode-hook-function))
  :init
  (defun ar/emacs-lisp-mode-hook-function ()
    "Called when entering `emacs-lisp-mode'."
    ;; Pretty print output to *Pp Eval Output*.
    (local-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)
    (setq-local company-backends '((company-yasnippet
                                    company-dabbrev-code
                                    company-keywords
                                    company-files
                                    company-capf)))
    (fontify-face-mode +1))
  :config
  (require 'simple)
  (require 'ar-csetq)
  ;; From https://github.com/daschwa/emacs.d
  ;; Nic says eval-expression-print-level needs to be set to nil (turned off) so
  ;; that you can always see what's happening.
  (ar/csetq eval-expression-print-level nil)

  ;; make ELisp regular expressions more readable.
  (use-package easy-escape
    :ensure t
    :hook (emacs-lisp-mode . easy-escape-minor-mode)
    :config
    ;; TODO: Figure out why face foreground isn't displayed.
    (set-face-attribute 'easy-escape-face nil :foreground "red")
    (ar/vsetq easy-escape-character ?⑊))

  ;; Apply face to face symbols themselves.
  (use-package fontify-face
    :ensure t
    :commands fontify-face-mode))
