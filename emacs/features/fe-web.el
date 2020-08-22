;;; -*- lexical-binding: t; -*-

(use-package enlive
  :ensure t
  :defer)

(use-package ar-url
  :commands (ar/url-view-links-at))

;; Make URLs and e-mail addresses clickable or activatable wit <RET>.
(use-package goto-addr
  :functions (goto-address-at-point)
  :bind (:map goto-address-highlight-keymap
              ("<RET>" . goto-address-at-point)
              ("M-<RET>" . newline))
  :commands (goto-address-prog-mode
             goto-address-mode))

(use-package css-mode
  :mode (("\\.css\\'" . css-mode)
         ("\\.rasi\\'" . css-mode)))

(use-package sgml-mode
  :bind(:map
        sgml-mode-map
        ("<tab>" . ar/indent-for-tab-command-dwim)))

(use-package auto-rename-tag
  :hook ((nxml-mode . auto-rename-tag-mode)
         (html-mode . auto-rename-tag-mode))
  :ensure t)

(use-package web-mode
  :ensure t
  :validate-custom
  (web-mode-auto-close-style 2))
