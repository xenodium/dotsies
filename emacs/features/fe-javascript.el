;;; -*- lexical-binding: t; -*-

(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" . json-mode))

(use-package js
  :validate-custom
  (js-indent-level 2))

(use-package js-comint
  :ensure t
  :commands js-comint-repl)

(use-package typescript-mode
  :ensure t
  :validate-custom
  (typescript-indent-level 2)
  :config
  ;; Resolve mocha-reported errors
  (add-to-list 'compilation-error-regexp-alist-alist
               '(mocha
                 "file://\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)"
                 1 2 3))
  (add-to-list 'compilation-error-regexp-alist 'mocha))
