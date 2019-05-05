(require 'ar-vsetq)

(use-package ledger-mode
  :ensure t
  :ensure-system-package ledger
  :hook ((ledger-mode . company-mode)
         (ledger-mode . ar/ledger-mode-hook-function))
  :config
  (ar/vsetq ledger-reconcile-default-commodity "GBP")

  (defun ar/ledger-mode-hook-function ()
    (setq-local company-backends '((company-capf))))

  (use-package flycheck-ledger
    :ensure t
    :hook ((ledger-mode . flycheck-mode))))

(use-package csv-mode
  :mode ("\\.[Cc][Ss][Vv]\\'" . csv-mode)
  :ensure t)
