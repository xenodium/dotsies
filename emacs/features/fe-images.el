;;; -*- lexical-binding: t; -*-

(use-package image
  :validate-custom
  ;; Enable converting external formats (ie. webp) to internal ones.
  (image-use-external-converter t))

(use-package ar-image
  ;; Generate and open an HTML page with all images in current directory.
  ;; Much nicer for browsing pictures.
  :commands ar/image-open-html-for-current-dir)
