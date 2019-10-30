;;; -*- lexical-binding: t; -*-
(use-package s
  :ensure t)

(use-package ht
  :ensure t)

(use-package async
  :ensure t
  :config
  (async-bytecomp-package-mode +1)
  (dired-async-mode +1))

(use-package validate
  :ensure t
  :config
  (use-package ar-vsetq)

  (defmacro validate-customize-set-variable (variable value &optional comment)
    "Like `customize-set-variable', but throw an error if validation fails.
VALUE is validated against SYMBOL's custom type.

\(fn [SYM VAL] ...)"
    `(unless (boundp ,variable)
       (user-error "Trying to validate a variable that's not defined yet: `%s'.\nYou need to require the package before validating"
                   ,variable))
    `(customize-set-variable ,variable (validate-value ,value (custom-variable-type ,variable)) ,comment)))

;; flet is no longer available. Use noflet as a replacement.
(use-package noflet
  :ensure t
  :after dash)

;; Timestamp and date/time library.
(use-package ts
  :ensure t)
