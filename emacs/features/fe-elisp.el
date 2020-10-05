;;; -*- lexical-binding: t; -*-

(use-package ielm
  :commands ielm
  :hook ((ielm-mode . company-mode)))

(use-package pcre2el
  :ensure t
  :commands pcre-mode)

(use-package elisp-mode
  :bind ("C-x C-e" . ar/eval-last-sexp)
  :hook ((emacs-lisp-mode . pcre-mode)
         (emacs-lisp-mode . ar/emacs-lisp-mode-hook-function)
         (ielm-mode . ar/emacs-lisp-mode-hook-function))
  :config
  (require 'prog-mode)

  ;; Highlight bound variables and quoted exprs.
  (use-package lisp-extra-font-lock
    :ensure t
    :demand
    :hook ((emacs-lisp-mode . lisp-extra-font-lock-global-mode)))

  (use-package edebug
    :validate-custom
    ;; Display all.
    (edebug-print-length nil))

  (use-package suggest
    :ensure t
    :commands suggest)

  (use-package describe-hash
    :ensure t
    :commands describe-hash)

  (defun ar/emacs-lisp-mode-hook-function ()
    "Called when entering `emacs-lisp-mode'."
    ;; Pretty print output to *Pp Eval Output*.
    (local-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)
    (setq-local company-backends '((company-yasnippet
                                    company-dabbrev-code
                                    company-keywords
                                    company-files
                                    company-capf))))

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

  (use-package relint
    :commands relint-current-buffer
    :ensure t)

  ;; Apply face to face symbols themselves.
  (use-package fontify-face
    :ensure t
    :demand
    :commands fontify-face-mode)

  ;; Better M-. elisp navigation (enabled with smart jump).
  (use-package elisp-slime-nav
    :ensure t))

;; make ELisp regular expressions more readable.
(use-package easy-escape
  :ensure t
  :hook (emacs-lisp-mode . easy-escape-minor-mode)
  :validate-custom
  (easy-escape-character ?⑊)
  :config
  ;; TODO: Figure out why face foreground isn't displayed.
  (set-face-attribute 'easy-escape-face nil :foreground "red"))

(use-package eros
  ;; Inline evaluation (use M-C-x).
  :ensure t
  :commands eros-eval-defun
  :hook (emacs-lisp-mode . eros-mode))
