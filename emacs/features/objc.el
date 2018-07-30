(use-package objc-mode
  :init
  (defun ar/objc-mode-hook-function ()
    "Called when entering `objc-mode'."
    ;; Hook is run twice. Avoid:
    ;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=16759
    (unless (boundp 'objc-mode-hook-did-run)
      (set-fill-column 100)
      (company-mode +1)
      (subword-mode +1)
      (setq-local company-backends '((company-files
                                      company-keywords)))
      (add-hook 'before-save-hook #'clang-format-buffer t t)
      (setq-local objc-mode-hook-did-run t)))
  :hook (objc-mode . ar/objc-mode-hook-function)
  :bind (:map objc-mode-map
              ([f6] . recompile))
  :config
  (use-package clang-format
    :ensure t))
