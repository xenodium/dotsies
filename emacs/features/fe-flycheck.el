(use-package flycheck
  :ensure t
  :config
  ;; Override default flycheck triggers
  (ar/vsetq flycheck-check-syntax-automatically
            '(save idle-change mode-enabled)
            flycheck-idle-change-delay 0.8)

  (use-package flycheck-status-emoji
    :ensure t
    :custom
    (flycheck-status-emoji-indicator-finished-error ?💀)
    (flycheck-status-emoji-indicator-finished-ok ?👍)
    (flycheck-status-emoji-indicator-finished-warning ?👎)
    :config
    (flycheck-status-emoji-mode +1))

  (use-package flycheck-inline
    :ensure t
    :hook (flycheck-mode . turn-on-flycheck-inline)))
