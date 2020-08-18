;;; -*- lexical-binding: t; -*-

;; Search for things modally, using ivy.
(use-package modal-ivy
  ;; Need these accessible outside Emacs.
  :commands (ar/modal-ivy-search-org-links
             ar/modal-ivy-search-short-links
             ar/modal-ivy-clipboard
             ar/modal-ivy-search-short-links
             ar/modal-key-binding-r
             ar/hammerspoon-org-modal-add-todo
             ar/modal-key-binding-i)
  :config
  ;; Personal modal ivy extensions.
  (use-package arp-modal-ivy
    :if (locate-library "arp-modal-ivy"))

  ;; Add TODOs modally.
  (use-package ar-hammerspoon-org-modal))
