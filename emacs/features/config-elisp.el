;;; -*- lexical-binding: t; -*-

;; Highlight bound variables and quoted exprs.
(use-package lisp-extra-font-lock
  :ensure t
  :defer 60
  :hook ((emacs-lisp-mode . lisp-extra-font-lock-global-mode)))

(use-package edebug
  :defer 60
  :validate-custom
  ;; Display all.
  (edebug-print-length nil))

(use-package suggest
  :ensure t
  :commands suggest)

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

(use-package eros
  ;; Inline evaluation (use M-C-x).
  :ensure t
  :defer 60
  :hook (emacs-lisp-mode . eros-mode))

(use-package relint
  :commands relint-current-buffer
  :ensure t)

;; make ELisp regular expressions more readable.
(use-package easy-escape
  :defer 60
  :ensure t
  :hook (emacs-lisp-mode . easy-escape-minor-mode)
  :validate-custom
  (easy-escape-character ?⑊)
  :config
  ;; TODO: Figure out why face foreground isn't displayed.
  (set-face-attribute 'easy-escape-face nil :foreground "red"))

;; Apply face to face symbols themselves.
(use-package fontify-face
  :ensure t
  :commands fontify-face-mode)

;; Better M-. elisp navigation (enabled with smart jump).
(use-package elisp-slime-nav
  :ensure t
  :defer 60)
