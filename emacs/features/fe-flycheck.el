(use-package flycheck
  :ensure t
  :config
  ;; Override default flycheck triggers
  (ar/vsetq flycheck-check-syntax-automatically
            '(save idle-change mode-enabled)
            flycheck-idle-change-delay 0.8)

  (use-package flycheck-inline
    :ensure t
    :hook (flycheck-mode . turn-on-flycheck-inline)))
