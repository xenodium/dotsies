;;; -*- lexical-binding: t; -*-
(use-package paradox
  :ensure t
  :commands (paradox-list-packages)
  :bind  (:map paradox-menu-mode-map
               ("b" . paradox-menu-visit-homepage))
  :config
  (fullframe paradox-list-packages paradox-quit-and-close))
