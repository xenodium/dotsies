;;; -*- lexical-binding: t; -*-

(use-package cc-mode
  :mode ("\\.m\\'" . objc-mode)
  :hook (objc-mode . ar/objc-mode-hook-function)
  :bind (:map
         objc-mode-map
         ("M-]" . ar/smartparens-wrap-square-bracket)
         :map
         c-mode-base-map
         ("C-c C-c" . ar/compile))
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

      ;; Disabled in favor of counsel-etags
      ;; (ggtags-mode +1)
      ;; (counsel-gtags-mode +1)

      ;; Company backends for Objective-C.
      (setq-local company-backends '((company-files
                                      company-keywords)))

      (when (require 'reformatter nil 'noerror)
        (clang-format-on-save-mode +1))

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
  ;; Prefer global M-a binding.
  (unbind-key "M-a" c-mode-base-map)

  (require 'company)

  (when (require 'reformatter nil 'noerror)
    (reformatter-define clang-format
      :program "clang-format")))

;; Recognize .h headers can also be Objective-C (enable objc-mode for them).
(use-package dummy-h-mode
  :mode (("\\.h\\'" . dummy-h-mode)))
