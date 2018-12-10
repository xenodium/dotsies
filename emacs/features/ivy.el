(require 'ar-vsetq)

;; Ivy equivalents to Emacs commands.
(use-package counsel
  :ensure t
  :defer 0.1
  :bind (("C-c i" . counsel-semantic-or-imenu))
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
  :bind (("C-x C-b" . ivy-switch-buffer)
         ("C-c C-r" . ivy-resume)
         :map ivy-minibuffer-map
         ("C-g" . ar/ivy-keybqoard-quit-dwim))
  :config
  (ar/vsetq ivy-height 40)
  (ar/vsetq ivy-count-format "")
  (ar/vsetq ivy-use-virtual-buffers t)
  (ar/vsetq enable-recursive-minibuffers t)
  (defun ar/ivy-keybqoard-quit-dwim ()
    "If region active, deactivate. If there's content, minibuffer. Otherwise quit."
    (interactive)
    (cond ((and delete-selection-mode (region-active-p))
           (setq deactivate-mark t))
          ((> (length ivy-text) 0)
           (delete-minibuffer-contents))
          (t
           (minibuffer-keyboard-quit))))
  (ivy-mode +1))

;; Displays yasnippet previous inline when cycling through results.
(use-package ivy-yasnippet
  :ensure t
  :commands ivy-yasnippet
  :config
  (require 'yasnippet)
  (yas-minor-mode))
