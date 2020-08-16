;;; -*- lexical-binding: t; -*-

(use-package ledger-mode
  :ensure t
  :ensure-system-package ledger
  :validate-custom
  (ledger-reconcile-default-commodity "GBP")
  :hook ((ledger-mode . company-mode)
         (ledger-mode . ar/ledger-mode-hook-function))
  :config
  (defun ar/ledger-mode-hook-function ()
    (setq-local company-backends '((company-capf))))

  (use-package flycheck-ledger
    :ensure t
    :hook ((ledger-mode . flycheck-mode))))

(use-package csv-mode
  :mode ("\\.[Cc][Ss][Vv]\\'" . csv-mode)
  :bind (:map csv-mode-map
              ("<tab>" . csv-forward-field)
              ("<backtab>" . csv-backward-field))
  :ensure t)
