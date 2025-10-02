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

;; Don't use continuation character.
(setq-default fringe-indicator-alist (delq (assq 'continuation fringe-indicator-alist) fringe-indicator-alist))

(use-package frame
  :defer
  :init
  ;; Mispressing C-z or C-x C-z invokes `suspend-frame' (disable).
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z"))
  :config
  ;; Enable expanding frame to end of screen.
  (setq frame-resize-pixelwise t)
  ;; Remove thin border. Visible since Monterey.
  (set-frame-parameter nil 'internal-border-width 0)
  (set-frame-position (selected-frame) 15 53)
  (set-frame-size (selected-frame)
                  (- (display-pixel-width) 30 16)
                  (- (display-pixel-height) 30 53 15)
                  t))

;; Ensure window is maximized after window setup.
;; Using dimenstions from frame.
;; (use-package maxframe
;;   :ensure t
;;   :hook (window-setup . maximize-frame))

;; Used when exporting org source blocks.
(use-package github-theme
  :defer
  :ensure t)

(use-package material-theme
  :ensure t
  :config
  (load-theme 'material t)
  (ar/load-materialized-tweaks)
  :init
  (defun ar/load-material-org-present-tweaks ()
    (with-eval-after-load 'faces
      (set-face-attribute 'org-link nil :underline nil)
      (set-face-attribute 'org-target nil :underline nil)
      (set-face-attribute 'org-meta-line nil :inherit 'default)
      (set-face-attribute 'org-meta-line nil :inherit 'lisp-extra-font-lock-quoted :foreground 'unspecified :background 'unspecified :box nil)
      (set-face-attribute 'org-level-1 nil :foreground "#ff69b4" :background 'unspecified :box nil)
      (set-face-attribute 'org-level-2 nil :inherit 'lisp-extra-font-lock-quoted :foreground 'unspecified :background 'unspecified :box nil)
      (set-face-attribute 'org-block nil :background "grey11" :box nil)
      (set-face-attribute 'read-multiple-choice-face nil :background "#212121" :box nil :underline nil)
      ))

  (defun ar/drop-material-org-present-tweaks ()
    (with-eval-after-load 'frame
      (set-cursor-color "royal blue"))

    (with-eval-after-load 'faces
      (set-face-attribute 'org-level-1 nil :foreground 'unspecified :background 'unspecified :box nil)
      (set-face-attribute 'org-level-2 nil :inherit nil :foreground 'unspecified :background 'unspecified :box nil)
      (set-face-attribute 'org-block nil :background 'unspecified :box nil)))

  (defun ar/load-materialized-tweaks ()
    (with-eval-after-load 'frame
      (set-cursor-color "orange"))

    (with-eval-after-load 'faces
      ;; From https://gist.github.com/huytd/6b785bdaeb595401d69adc7797e5c22c#file-customized-org-mode-theme-el
      (set-face-attribute 'default nil :stipple nil :background "#212121" :foreground "#eeffff" :inverse-video nil
                          ;; :family "Menlo" ;; or Meslo if unavailable: https://github.com/andreberg/Meslo-Font
                          ;; :family "Hack" ;; brew tap homebrew/cask-fonts && brew cask install font-hack
                          :family "Sarasa Mono J" ;; brew install --cask font-sarasa-gothic
                          ;; :family "JetBrains Mono" ;; brew tap homebrew/cask-fonts && brew install --cask font-jetbrains-mono
                          ;; :family "mononoki" ;; https://madmalik.github.io/mononoki/ or sudo apt-get install fonts-mononoki
                          :box nil :strike-through nil :overline nil :underline nil :slant 'normal :weight 'normal
                          :width 'normal :foundry "nil")

      ;; (set-fontset-font t nil "Sarasa Mono J" nil 'append)
      ;; (set-fontset-font t 'kana (font-spec :family "Sarasa Mono J") nil 'prepend) ;; Hiragana & Katakana
      ;; (set-fontset-font t 'han (font-spec :family "Sarasa Mono J") nil 'prepend)  ;; Kanji

      (when (memq system-type '(darwin))
        ;; Enable rendering SF symbols on macOS.
        (set-fontset-font t nil "SF Pro Display" nil 'append)
        ;; Emoji's.
        (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)))

    (with-eval-after-load 'font-lock
      ;; brew install font-iosevka-aile
      ;; (set-face-attribute 'font-lock-comment-face nil :font "Iosevka Aile")
      (set-face-attribute 'font-lock-comment-face nil :font "Sarasa Mono J"))))

(use-package moody
  :ensure t
  :config
  (setq x-underline-at-descent-line t)

  (setq-default mode-line-format
                '(" "
                  mode-line-front-space
                  mode-line-client
                  mode-line-frame-identification
                  mode-line-buffer-identification
                  (:eval
                   (when (eq major-mode 'image-mode)
                     ;; imagemagick alternative (also shows type and file size).
                     ;; (process-lines "identify" "-format" "[%m %wx%h %b]" (buffer-file-name))
                     (let ((size (image-size (create-image (buffer-file-name) nil nil :scale 1.0) t)))
                       (list (format "%dx%d" (car size) (cdr size))))))
                  " "
                  mode-line-position
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
  :bind ("C-x k" . kill-current-buffer))

(use-package face-remap
  :init
  ;; Prevent trackpad/macOS from messing with font size.
  (global-set-key (kbd "<pinch>") 'ignore)
  (global-set-key (kbd "<C-wheel-up>") 'ignore)
  (global-set-key (kbd "<C-wheel-down>") 'ignore)
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

(use-package ultra-scroll
  :ensure t
  :init
  :defer 20
  :validate-custom
  (scroll-conservatively 3)
  (scroll-margin 0)
  :config
  (ultra-scroll-mode +1))

(use-package winner
  :bind(("M-<escape>" . winner-undo)
        ;; These are below esc on external keyboard
        ("M-`" . winner-undo)
        ("M-~" . winner-redo)
        ;; These two are below esc on Macbook
        ("M-§" . winner-undo)
        ("M-±" . winner-redo))
  :config
  (winner-mode +1))

(use-package keycast
  :ensure t
  :commands keycast-mode-line-mode
  :config
  (setq keycast-mode-line-insert-after 'mode-line-position)
  (setq keycast-mode-line-format "%10s%k%r"))

