;; Search for things modally, using ivy.
(use-package modal-ivy
  :commands (ar/modal-ivy-search-org-links
             ar/modal-key-binding-i)
  :config
  ;; Personal modal ivy extensions.
  (use-package arp-modal-ivy
    :if (locate-library "arp-modal-ivy")
    :defer 5)
  ;; Add TODOs modally.
  (use-package ar-hammerspoon-org-modal
    :defer 5))
