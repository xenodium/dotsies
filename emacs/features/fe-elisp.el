;;; -*- lexical-binding: t; -*-

(use-package ielm
  :commands ielm
  :hook ((ielm-mode . company-mode))
  :validate-custom
  (ielm-header ""))

(use-package pcre2el
  :ensure t
  :config
  (pcre-mode +1))

(use-package elisp-mode
  :bind ("C-x C-e" . ar/eval-last-sexp)
  :hook ((emacs-lisp-mode . ar/emacs-lisp-mode-hook-function)
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

  (defun ar/eval-uncommented-region (start end)
    (interactive "r")
    (let ((text (buffer-substring-no-properties start end)))
      (with-temp-buffer
        (insert text)
        (emacs-lisp-mode)
        (uncomment-region (point-min) (point-max))
        (eval-buffer)
        (message "Evaluated"))))

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

;; Inline evaluation (use M-C-x).
(use-package eros
  :ensure t
  :commands eros-eval-defun
  :hook (emacs-lisp-mode . eros-mode)
  :config
  (defun adviced:edebug-compute-previous-result (_ &rest r)
    "Adviced `edebug-compute-previous-result'."
    (let ((previous-value (nth 0 r)))
      (if edebug-unwrap-results
          (setq previous-value
                (edebug-unwrap* previous-value)))
      (setq edebug-previous-result
            (with-temp-message ""
              (edebug-safe-prin1-to-string previous-value)
              (message "%s" (prin1-to-string previous-value))))))

  (advice-add #'edebug-compute-previous-result
              :around
              #'adviced:edebug-compute-previous-result)

  (defun adviced:edebug-previous-result (_ &rest r)
    "Adviced `edebug-previous-result'."
    (eros--make-result-overlay edebug-previous-result
      :where (point)
      :duration eros-eval-result-duration))

  (advice-add #'edebug-previous-result
              :around
              #'adviced:edebug-previous-result))

(use-package clojure-mode
  :ensure t
  :mode ("\\`clj[scxd]?\\." . clojure-mode)
  :config
  (use-package cider
    :ensure t
    :config
    (defun ar/cider-mode-hook ()
      "`cider-mode' hook."
      (setq-local company-backends '((company-capf
                                      company-files
                                      company-keywords))))
    (add-hook 'clojure-mode-hook #'ar/cider-mode-hook)))

;;; fe-elisp.el ends here
