;;; -*- lexical-binding: t; -*-

(when (display-graphic-p)
  ;; No title. See init.el for initial value.
  (setq frame-title-format nil)
  ;; Avoid native dialogs.
  (setq use-dialog-box nil))

;; Set font face height. Value is 1/10pt.
(set-face-attribute 'default nil
                    :height 160)

(use-package frame
  :defer 60
  :init
  ;; Mispressing C-z invokes `suspend-frame' (disable).
  (global-unset-key (kbd "C-z"))
  :config
  ;; Enable expanding frame to end of screen.
  (setq frame-resize-pixelwise t))

;; Ensure window is maximized after window setup.
(use-package maxframe
  :ensure t
  :hook (window-setup . maximize-frame))

;; Used when exporting org source blocks.
(use-package github-theme
  :defer 60
  :ensure t)

(use-package material-theme
  :ensure t
  :config
  (load-theme 'material t)
  (load "~/.emacs.d/features/config-material"))

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
    :validate-custom
    (minions-mode-line-lighter "…")
    (minions-mode-line-delimiters '("" . ""))
    :config
    (minions-mode +1))

  ;; Disabled: Trying out clock outside Emacs
  ;; (use-package time
  ;;   :validate-custom
  ;;   (display-time-24hr-format t)
  ;;   (display-time-day-and-date t)
  ;;   (display-time-world-list '(("Europe/Paris" "Paris")
  ;;                              ("Europe/London" "London")
  ;;                              ("America/Los_Angeles" "Los Angeles")))
  ;;   (display-time-string-forms
  ;;    '((format "%s %s %s, %s:%s"
  ;;              dayname
  ;;              monthname day
  ;;              24-hours minutes)))
  ;;   :config
  ;;   (display-time))

  (setq global-mode-string (remove 'display-time-string global-mode-string))

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

(use-package nyan-mode
  :defer 60
  :ensure t
  :if (display-graphic-p)
  :validate-custom
  (nyan-bar-length 10)
  :config
  (nyan-mode +1))

(use-package hide-mode-line
  :ensure t
  :commands hide-mode-line-mode)
