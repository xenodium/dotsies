(require 'ar-vsetq)

(use-package ledger-mode
  :ensure t
  :ensure-system-package ledger
  :config
  (ar/vsetq ledger-reconcile-default-commodity "GBP")
  (use-package flycheck-ledger
    :ensure t
    :hook (ledger-mode . flycheck-mode)))
