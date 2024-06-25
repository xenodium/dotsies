;;; -*- lexical-binding: t; -*-

(use-package image
  :validate-custom
  ;; emacs-plus compiles with imagemagick, but is no longer needed to
  ;; open other formats like webp. Setting imagemagick-types-inhibit
  ;; disables imagemagick usage.
  (imagemagick-types-inhibit t))

(use-package ar-image
  ;; Generate and open an HTML page with all images in current directory.
  ;; Much nicer for browsing pictures.
  :commands ar/image-open-html-for-current-dir)
