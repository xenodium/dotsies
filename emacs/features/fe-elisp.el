;;; -*- lexical-binding: t; -*-
(use-package ielm
  :hook ((ielm-mode . company-mode)))

;; Highlight bound variables and quoted exprs.
(use-package lisp-extra-font-lock
  :ensure t
  :hook ((emacs-lisp-mode . lisp-extra-font-lock-global-mode)))

(use-package elisp-mode
  :commands emacs-lisp-mode
  :bind ("C-x C-e" . ar/eval-last-sexp)
  :hook ((emacs-lisp-mode . pcre-mode)
         (emacs-lisp-mode . ar/emacs-lisp-mode-hook-function)
         (ielm-mode . ar/emacs-lisp-mode-hook-function))
  :init
  (defun ar/emacs-lisp-mode-hook-function ()
    "Called when entering `emacs-lisp-mode'."
    ;; Pretty print output to *Pp Eval Output*.
    (local-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)
    (setq-local company-backends '((company-yasnippet
                                    company-dabbrev-code
                                    company-keywords
                                    company-files
                                    company-capf))))
  :config
  (require 'simple)
  (require 'ar-csetq)

  (use-package suggest
    :ensure t)

  ;; From https://github.com/daschwa/emacs.d
  ;; Nic says eval-expression-print-level needs to be set to nil (turned off) so
  ;; that you can always see what's happening.
  (ar/csetq eval-expression-print-level nil)

  ;; Based on https://emacsredux.com/blog/2013/06/21/eval-and-replace
  (defun ar/eval-last-sexp (arg)
    "Replace the preceding sexp with its value."
    (interactive "P")
    (if arg
        (progn
          (backward-kill-sexp)
          (condition-case nil
              (prin1 (eval (read (current-kill 0)))
                     (current-buffer))
            (error (message "Invalid expression")
                   (insert (current-kill 0)))))
      (let ((current-prefix-arg nil)
            (buffer-name "*Pp Eval Output*"))
        (call-interactively 'pp-eval-last-sexp)
        ;; New window? Select and make read-only (q closes window).
        (when (get-buffer-window buffer-name)
          (with-current-buffer buffer-name
            (view-mode +1)
            (select-window (get-buffer-window buffer-name)))))))

  (use-package eros
    ;; Inline evaluation (use M-C-x).
    :ensure t
    :hook (emacs-lisp-mode . eros-mode))

  (use-package relint
    :ensure t)

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

;; Better M-. elisp navigation (enabled with smart jump).
(use-package elisp-slime-nav
  :ensure t)
