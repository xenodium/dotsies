;;; -*- lexical-binding: t; -*-

;; Requires gocode daemon. Install with:
;; go get -u golang.org/x/tools/cmd/...
;; go get -u github.com/mdempsky/gocode
;; go get -u github.com/rogpeppe/godef
;; Useful info at:
;; From http://tleyden.github.io/blog/2014/05/22/configure-emacs-as-a-go-editor-from-scratch
;; From http://tleyden.github.io/blog/2014/05/27/configure-emacs-as-a-go-editor-from-scratch-part-2
;; From http://dominik.honnef.co/posts/2013/03/writing_go_in_emacs
(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :bind
  (:map go-mode-map
        ("M-." . godef-jump))
  :hook (go-mode . ar/go-mode-hook-function)
  :validate-custom
  (gofmt-command "goimports")
  :init
  (defun ar/go-mode-hook-function ()
    "Called when entering `go-mode'."
    (setq-local company-backends '(company-go))
    (setq-local tab-width 2 indent-tabs-mode t)
    (add-hook 'before-save-hook #'gofmt-before-save t t))
  :config
  (setenv "GOPATH" (expand-file-name "~/stuff/active/code/gopath"))

  (add-to-list 'exec-path
               (expand-file-name "~/stuff/active/code/gopath/bin"))

  (add-to-list 'exec-path
               (expand-file-name "~/go/bin"))

  (use-package go-snippets
    :ensure t
    :config
    (go-snippets-initialize))

  (use-package company-go
    :ensure t)

  ;; go get -u github.com/golang/lint/golint
  (use-package golint
    :ensure t)

  (use-package go-eldoc
    :ensure t
    :config
    (go-eldoc-setup))

  (use-package gotest
    :ensure t)

  ;; go get -u golang.org/x/tools/cmd/gorename
  (use-package go-rename
    :ensure t)

  (use-package ob-go
    :ensure t)

  (use-package godoctor
    :ensure t))
