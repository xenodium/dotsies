;;; -*- lexical-binding: t; -*-
(use-package ar-misc
  :commands (ar/misc-clipboard-to-qr
             ar/misc-financial-times-lookup-symbol
             ar/misc-diff-last-2-yanks))

;; Weather forecast (no login/account needed).
(use-package wttrin
  :ensure t
  :commands (wttrin
             ar/weather)
  :validate-custom
  (wttrin-default-accept-language '("Accept-Language" . "en-GB"))
  (wttrin-default-cities (list "London"))
  :config
  (defun ar/weather ()
    "Show London weather."
    (interactive)
    (wttrin-query "London")))

;; https://gist.github.com/syohex/626af66ba3650252b0a2
(defun ar/hash-region (algorithm beg end)
  "Hash region using ALGORITHM with BEG and END endpoints."
  (interactive
   (list
    (completing-read "Hash type: " '(md5 sha1 sha224 sha256 sha384 sha512))
    (if (use-region-p)
        (region-beginning)
      (point-min))
    (if (use-region-p)
        (region-end)
      (point-max))))
  (message "%s: %s"
           algorithm (secure-hash (intern algorithm) (current-buffer) beg end)))
