(use-package cc-mode
  :mode ("\\.m\\'" . objc-mode)
  :hook (objc-mode . ar/objc-mode-hook-function)
  :bind (:map objc-mode-map
              ("M-]" . ar/smartparens-wrap-square-bracket))
  :init
  ;; Workaround to define two missing functions. Try removing in future.
  (defun c-before-change-check-unbalanced-strings (beg end))
  (defun c-after-change-mark-abnormal-strings (beg end _old-len))

  (defun ar/objc-mode-hook-function ()
    "Called when entering `objc-mode'."
    ;; Hook is run twice. Avoid:
    ;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=16759
    (unless (boundp 'objc-mode-hook-did-run)
      (set-fill-column 100)

      (ggtags-mode +1)
      (counsel-gtags-mode +1)

      ;; Company backends for Objective-C.
      (setq-local company-backends '((company-files
                                      company-keywords)))

      ;; Format files for me.
      (add-hook 'before-save-hook #'clang-format-buffer t t)

      (setq-local objc-mode-hook-did-run t)))
  (defun ar/smartparens-wrap-square-bracket (arg)
    "[] equivalent of `paredit-wrap-round'."
    (interactive "P")
    (save-excursion
      (unless (sp-point-in-symbol)
        ;; (sp-skip-backward-to-symbol)
        (sp-backward-parallel-sexp)
        ;; (sp-backward-down-sexp)
        )
      (sp-wrap-with-pair "[")))
  :config
  (use-package clang-format
    :ensure t)
  (use-package company))

;; Recognize .h headers can also be Objective-C (enable objc-mode for them).
(use-package dummy-h-mode
  :mode (("\\.h\\'" . dummy-h-mode)))
