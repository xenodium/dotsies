;;; -*- lexical-binding: t; -*-

(use-package image
  ;; AVIF not recognized by default.
  :mode ("\\.avif\\'" . image-mode)
  ;; emacs-plus compiles with imagemagick, but is no longer needed to
  ;; open other formats like webp. Inhibit imagemagick for those cases
  ;; preferring the built-in implementation.
  :config
  (add-to-list 'imagemagick-types-inhibit 'HEIC)
  (add-to-list 'imagemagick-types-inhibit 'PNG)
  (add-to-list 'imagemagick-types-inhibit 'WEBP)
  (add-to-list 'imagemagick-types-inhibit 'GIF)
  (add-to-list 'imagemagick-types-inhibit 'JPEG)
  (add-to-list 'imagemagick-types-inhibit 'JPG))

(use-package ar-image
  ;; Generate and open an HTML page with all images in current directory.
  ;; Much nicer for browsing pictures.
  :commands ar/image-open-html-for-current-dir)
