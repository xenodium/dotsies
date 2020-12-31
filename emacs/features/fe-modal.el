;;; -*- lexical-binding: t; -*-

;; Search for things modally, using ivy.
(use-package modal-ivy
  ;; Need these accessible outside Emacs.
  :commands (ar/modal-ivy-search-org-links
             ar/modal-ivy-search-short-links
             ar/modal-ivy-clipboard
             ar/hammerspoon-org-modal-add-todo)
  :config
  ;; Add TODOs modally.
  (use-package ar-hammerspoon-org-modal))
