;;; -*- lexical-binding: t; -*-

(when (display-graphic-p)
  ;; No title. See init.el for initial value.
  (setq-default frame-title-format nil)
  ;; Hide the cursor in inactive windows.
  (setq cursor-in-non-selected-windows nil)
  ;; Avoid native dialogs.
  (setq use-dialog-box nil))

;; Set font face height. Value is 1/10pt.
(set-face-attribute 'default nil
                    :height 160)

(use-package frame
  :defer
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
  :defer
  :ensure t)

(use-package material-theme
  :ensure t
  :config
  (load-theme 'material t)

  (with-eval-after-load 'faces
    ;; From https://gist.github.com/huytd/6b785bdaeb595401d69adc7797e5c22c#file-customized-org-mode-theme-el
    (set-face-attribute 'default nil :stipple nil :background "#212121" :foreground "#eeffff" :inverse-video nil
                        ;; :family "Menlo" ;; or Meslo if unavailable: https://github.com/andreberg/Meslo-Font
                        :family "Hack" ;; brew tap homebrew/cask-fonts && brew cask install font-hack
                        ;; :family "mononoki" ;; https://madmalik.github.io/mononoki/ or sudo apt-get install fonts-mononoki
                        :box nil :strike-through nil :overline nil :underline nil :slant 'normal :weight 'normal
                        :width 'normal :foundry "nil")

    ;; Emoji's: welcome back to Emacs
    ;; https://github.crookster.org/emacs27-from-homebrew-on-macos-with-emoji/
    (when (>= emacs-major-version 27)
      (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))

    ;; Hardcode region theme color.
    (set-face-attribute 'region nil :background "#3f464c" :foreground "#eeeeec" :underline nil)
    (set-face-attribute 'mode-line nil :background "#191919" :box nil)

    ;; Styling moody https://github.com/tarsius/moody
    (let ((line (face-attribute 'mode-line :underline)))
      (set-face-attribute 'mode-line nil :overline   line)
      (set-face-attribute 'mode-line-inactive nil :overline   line)
      (set-face-attribute 'mode-line-inactive nil :underline  line)
      (set-face-attribute 'mode-line nil :box nil)
      (set-face-attribute 'mode-line-inactive nil :box nil)
      (set-face-attribute 'mode-line-inactive nil :background "#212121" :foreground "#5B6268")))

  (with-eval-after-load 'font-lock
    (set-face-attribute 'font-lock-constant-face nil :foreground "#C792EA")
    (set-face-attribute 'font-lock-keyword-face nil :foreground "#2BA3FF" :slant 'italic)
    (set-face-attribute 'font-lock-preprocessor-face nil :inherit 'bold :foreground "#2BA3FF" :slant 'italic :weight 'normal)
    (set-face-attribute 'font-lock-string-face nil :foreground "#C3E88D")
    (set-face-attribute 'font-lock-type-face nil :foreground "#FFCB6B")
    (set-face-attribute 'font-lock-variable-name-face nil :foreground "#FF5370"))

  (with-eval-after-load 'em-prompt
    (set-face-attribute 'eshell-prompt nil :foreground "#eeffff"))

  (with-eval-after-load 'company
    (set-face-attribute 'company-preview-search nil :foreground "sandy brown" :background nil)
    (set-face-attribute 'company-preview-common nil :inherit 'default :foreground nil :background "#212121"))

  (with-eval-after-load 'company-box
    (set-face-attribute 'company-box-candidate  nil :inherit 'default :foreground "#eeffff" :background "#212121" :box nil)
    (set-face-attribute 'company-box-background nil :inherit 'default :background "#212121" :box nil)
    (set-face-attribute 'company-box-annotation nil :inherit 'company-tooltip-annotation :background "#212121" :foreground "dim gray")
    (set-face-attribute 'company-box-selection nil :inherit 'company-tooltip-selection :foreground "sandy brown"))

  (with-eval-after-load 'paren
    (set-face-attribute 'show-paren-match nil
                        :background nil
                        :foreground "#FA009A"))

  (with-eval-after-load 'org-indent
    (set-face-attribute 'org-indent nil :background "#212121"))

  (with-eval-after-load 'org-faces
    (set-face-attribute 'org-hide nil :foreground "#212121" :background "#212121" :strike-through nil)
    (set-face-attribute 'org-done nil :foreground "#b9ccb2" :strike-through nil)
    (set-face-attribute 'org-agenda-date-today nil :foreground "#Fb1d84")
    (set-face-attribute 'org-agenda-done nil :foreground "#b9ccb2" :strike-through nil)
    (set-face-attribute 'org-table nil :background nil)
    (set-face-attribute 'org-code nil :background nil)
    (set-face-attribute 'org-level-1 nil :background nil :box nil)
    (set-face-attribute 'org-level-2 nil :background nil :box nil)
    (set-face-attribute 'org-level-3 nil :background nil :box nil)
    (set-face-attribute 'org-level-4 nil :background nil :box nil)
    (set-face-attribute 'org-level-5 nil :background nil :box nil)
    (set-face-attribute 'org-level-6 nil :background nil :box nil)
    (set-face-attribute 'org-level-7 nil :background nil :box nil)
    (set-face-attribute 'org-level-8 nil :background nil :box nil)
    (set-face-attribute 'org-block-begin-line nil :background nil :box nil)
    (set-face-attribute 'org-block-end-line nil :background nil :box nil)
    (set-face-attribute 'org-block nil :background nil :box nil))

  (with-eval-after-load 'mu4e-vars
    (set-face-attribute 'mu4e-header-highlight-face nil :inherit 'default :foreground "sandy brown" :weight 'bold)
    (set-face-attribute 'mu4e-unread-face nil :inherit 'default :weight 'bold :foreground "#2BA3FF" :underline nil))

  ;; No color for fringe, blends with the rest of the window.
  (with-eval-after-load 'fringe
    (set-face-attribute 'fringe nil
                        :foreground (face-foreground 'default)
                        :background (face-background 'default)))

  ;; No color for sp-pair-overlay-face.
  (with-eval-after-load 'smartparens
    (set-face-attribute 'sp-pair-overlay-face nil
                        :foreground (face-foreground 'default)
                        :background (face-background 'default)))

  ;; Trying out line underline (instead of wave).
  (mapatoms (lambda (atom)
              (let ((underline nil))
                (when (and (facep atom)
                           (setq underline
                                 (face-attribute atom
                                                 :underline))
                           (eq (plist-get underline :style) 'wave))
                  (plist-put underline :style 'line)
                  (set-face-attribute atom nil
                                      :underline underline))))))

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
  :defer)

(use-package menu-bar
  ;; No need to confirm killing buffers.
  :bind ("C-x k" . kill-this-buffer))

(use-package face-remap
  :bind(("C-+" . text-scale-increase)
        ("C--" . text-scale-decrease)))

(use-package nyan-mode
  :ensure t
  :defer 20
  :if (display-graphic-p)
  :validate-custom
  (nyan-bar-length 10)
  :config
  (nyan-mode +1))

(use-package hide-mode-line
  :ensure t
  :commands hide-mode-line-mode)
