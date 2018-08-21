(use-package ar-url
  :commands (ar/url-view-links-at)
  :config
  (use-package enlive :ensure t))

;; Make URLs and e-mail addresses clickable.
(use-package goto-addr
  :commands (goto-address-prog-mode
             goto-address-mode))
