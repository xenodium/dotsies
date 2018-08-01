(require 'ar-vsetq)

;; Ivy equivalents to Emacs commands.
(use-package counsel
  :ensure t
  :defer 0.1
  :config
  ;; Smex handles M-x command sorting. Bringing recent commands to the top.
  (use-package smex
    :ensure t)
  ;; Wgrep is used by counsel-ag (to make writeable).
  (use-package wgrep
    :ensure t)
  (counsel-mode +1))

(use-package counsel-projectile
  :ensure t
  :bind ("C-x f" . counsel-projectile-find-file))

(use-package ivy
  :ensure t
  :defer 0.1
  :bind ("C-x C-b" . ivy-switch-buffer)
  :config
  (ar/vsetq ivy-height 40)
  (ar/vsetq ivy-count-format "")
  (ar/vsetq ivy-use-virtual-buffers t)
  (ar/vsetq enable-recursive-minibuffers t)
  (ivy-mode +1))
