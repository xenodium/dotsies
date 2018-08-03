(use-package flycheck :ensure t
  :config
  ;; Override default flycheck triggers
  (validate-setq flycheck-check-syntax-automatically
                 '(save idle-change mode-enabled)
                 flycheck-idle-change-delay 0.8))
