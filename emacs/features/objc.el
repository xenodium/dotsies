(use-package objc-mode
  :init
  (defun ar/objc-mode-hook-function ()
    "Called when entering `objc-mode'."
    ;; Hook is run twice. Avoid:
    ;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=16759
    (unless (boundp 'objc-mode-hook-did-run)
      (set-fill-column 100)

      ;; Company backends for Objective-C.
      (setq-local company-backends '((company-files
                                      company-keywords)))

      ;; Userful for camel-case Objective-C.
      (subword-mode +1)

      ;; Format files for me.
      (add-hook 'before-save-hook #'clang-format-buffer t t)

      (setq-local objc-mode-hook-did-run t)))
  :hook (objc-mode . ar/objc-mode-hook-function)
  :bind (:map objc-mode-map
              ([f6] . recompile))
  :config
  (use-package clang-format
    :ensure t)
  (use-package company)
  (use-package simple)
  (use-package subword))

;; Recognize .h headers can also be Objective-C (enable objc-mode for them).
(use-package dummy-h-mode
  :mode (("\\.h\\'" . dummy-h-mode)))
