;; Re-open scratch if killed.
(use-package immortal-scratch
  :ensure t
  :config
  (immortal-scratch-mode))

;; Remember scratch content across sessions.
(use-package persistent-scratch
  :ensure t
  :config
  (persistent-scratch-setup-default))
