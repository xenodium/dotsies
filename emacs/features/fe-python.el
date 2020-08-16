;;; -*- lexical-binding: t; -*-

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :validate-custom
  (python-indent-guess-indent-offset-verbose nil))
