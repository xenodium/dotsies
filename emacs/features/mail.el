(use-package ar-mu4e
  :if (locate-library "ar-mu4e")
  :bind (:map global-map
              ("M-m" . ar/mu4e--view-unread-messages)))
