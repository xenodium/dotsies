(use-package enlive
  :ensure t)

(use-package ar-url
  :commands (ar/url-view-links-at))

;; Make URLs and e-mail addresses clickable or activatable wit <RET>.
(use-package goto-addr
  :bind (:map goto-address-highlight-keymap
              ("<RET>" . goto-address-at-point)
              ("M-<RET>" . newline))
  :commands (goto-address-prog-mode
             goto-address-mode))

(use-package css-mode
  :mode (("\\.css\\'" . css-mode)
         ("\\.rasi\\'" . css-mode)))

(use-package auto-rename-tag
  :hook ((nxml-mode . auto-rename-tag-mode)
         (html-mode . auto-rename-tag-mode))
  :ensure t)
