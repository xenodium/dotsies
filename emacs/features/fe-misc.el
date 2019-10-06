(use-package ar-misc
  :commands (ar/misc-clipboard-to-qr
             ar/misc-financial-times-lookup-symbol
             ar/misc-diff-last-2-yanks))

;; Weather forecast (no login/account needed).
(use-package wttrin
  :ensure t
  :commands (wttrin
             ar/weather)
  :custom
  (wttrin-default-accept-language '("Accept-Language" . "en-GB"))
  (wttrin-default-cities (list "London"))
  :config
  (defun ar/weather ()
    "Show London weather."
    (interactive)
    (wttrin-query "London")))
