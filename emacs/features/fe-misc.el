;;; -*- lexical-binding: t; -*-

(use-package ar-misc
  :bind (("C-x n n" . ar/misc-narrow-or-widen-dwim))
  :commands (ar/misc-clipboard-to-qr
             ar/misc-pick-font
             ar/misc-financial-times-lookup-symbol
             ar/misc-open-clipboard-file
             ar/misc-hash-region
             ar/misc-open-file-at-point
             ar/misc-download-clipboard-url
             ar/misc-diff-last-2-yanks))
