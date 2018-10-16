(require 'ar-vsetq)

;; Set font face height. Value is 1/10pt.
(set-face-attribute 'default nil
                    :height 180)

;; Ensure window is maximized after window setup.
(use-package maxframe
  :ensure t
  :hook (window-setup . maximize-frame))

(use-package material-theme
  :ensure t
  :config
  (load-theme 'material t)

  ;; From https://gist.github.com/huytd/6b785bdaeb595401d69adc7797e5c22c#file-customized-org-mode-theme-el
  (custom-set-faces
   '(default ((t (:inherit nil :stipple nil :background "#212121" :foreground "#eeffff" :inverse-video nil
                           :family "mononoki" ;; https://madmalik.github.io/mononoki/
                           :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal
                           :width normal :foundry "nil"))))
   '(font-lock-constant-face ((t (:foreground "#C792EA"))))
   '(eshell-prompt ((t (:foreground "#C3E88D"))))
   '(font-lock-keyword-face ((t (:foreground "#2BA3FF" :slant italic))))
   '(font-lock-preprocessor-face ((t (:inherit bold :foreground "#2BA3FF" :slant italic :weight normal))))
   '(font-lock-string-face ((t (:foreground "#C3E88D"))))
   '(font-lock-type-face ((t (:foreground "#FFCB6B"))))
   '(org-level-1 ((t (:background nil :box nil))))
   '(org-level-2 ((t (:background nil :box nil))))
   '(org-level-3 ((t (:background nil :box nil))))
   '(org-level-4 ((t (:background nil :box nil))))
   '(org-level-5 ((t (:background nil :box nil))))
   '(org-level-6 ((t (:background nil :box nil))))
   '(org-level-7 ((t (:background nil :box nil))))
   '(org-level-8 ((t (:background nil :box nil))))
   '(org-block-begin-line ((t (:background nil :box nil))))
   '(org-block-end-line ((t (:background nil :box nil))))
   '(org-block ((t (:background nil :box nil))))
   '(font-lock-variable-name-face ((t (:foreground "#FF5370"))))
   '(helm-rg-active-arg-face ((t (:foreground "LightGreen"))))
   '(helm-rg-file-match-face ((t (:foreground "LightGreen" :underline t))))
   '(helm-rg-preview-line-highlight ((t (:background "LightGreen" :foreground "black"))))
   '(mode-line ((t (:background "#191919" :box nil))))
   '(term ((t (:foreground "#fafafa")))))

  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line nil :box nil)
    (set-face-attribute 'mode-line-inactive nil :box nil)
    (set-face-attribute 'mode-line-inactive nil :background "#212121" :foreground "#5B6268")))

;; No color for fringe, blends with the rest of the window.
(set-face-attribute 'fringe nil :background nil)

;; Hardcode region theme color.
(set-face-attribute 'region nil
                    :background "#3f464c"
                    :foreground "#eeeeec"
                    :underline nil)

(use-package moody
  :ensure t
  :config
  (setq x-underline-at-descent-line t)

  (setq-default mode-line-format
                '(" "
                  mode-line-front-space
                  mode-line-client
                  mode-line-frame-identification
                  mode-line-buffer-identification " " mode-line-position
                  (vc-mode vc-mode)
                  (multiple-cursors-mode mc/mode-line)
                  " " mode-line-modes
                  mode-line-end-spaces))

  (use-package minions
    :ensure t
    :config
    (minions-mode +1))

  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

;; Use y/n instead of yes/no confirms.
;; From http://pages.sachachua.com/.emacs.d/Sacha.html#sec-1-4-8
(fset 'yes-or-no-p 'y-or-n-p)

(use-package fullframe
  :ensure t
  :commands fullframe)

(use-package menu-bar
  ;; No need to confirm killing buffers.
  :bind ("C-x k" . kill-this-buffer))

(use-package face-remap
  :bind(("C-+" . text-scale-increase)
        ("C--" . text-scale-decrease)))

(use-package nyan-mode :ensure t
  :if (display-graphic-p)
  :config
  (nyan-mode +1))
