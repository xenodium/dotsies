;;; -*- lexical-binding: t; -*-
(require 'ar-vsetq)
(require 'ar-csetq)
(require 'faces)

(when (display-graphic-p)
  ;; No title. See init.el for initial value.
  (setq frame-title-format nil)
  ;; Avoid native dialogs.
  (setq use-dialog-box nil))

;; Set font face height. Value is 1/10pt.
(set-face-attribute 'default nil
                    :height 160)

(use-package frame
  :init
  ;; Mispressing C-z invokes `suspend-frame' (disable).
  (global-unset-key (kbd "C-z"))
  ;; Enable expanding frame to end of screen.
  (setq frame-resize-pixelwise t))

;; Ensure window is maximized after window setup.
(use-package maxframe
  :ensure t
  :hook (window-setup . maximize-frame))

;; Used when exporting org source blocks.
;; (use-package github-theme
;;   :ensure t)

(use-package material-theme
  :ensure t
  :config
  (load-theme 'material t)

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
    (set-face-attribute 'company-preview-common nil :inherit 'company-tooltip :background "#383c44"))

  (with-eval-after-load 'company-box
    (set-face-attribute 'company-box-background nil :inherit 'company-tooltip :background "#383c44" :box '(:line-width 5 :color "grey75" :style released-button))
    (set-face-attribute 'company-box-annotation nil :inherit 'company-tooltip-annotation :background "#383c44" :foreground "dim gray")
    (set-face-attribute 'company-box-selection nil :inherit 'company-tooltip-selection :foreground "sandy brown"))

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

  (with-eval-after-load 'org-indent
    (set-face-attribute 'org-indent nil :background "#212121"))

  (with-eval-after-load 'org-faces
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
    :config
    (ar/csetq minions-mode-line-lighter "…")
    (ar/csetq minions-mode-line-delimiters '("" . ""))
    (minions-mode +1))

  (use-package time
    :config
    (ar/csetq display-time-24hr-format t)
    (ar/csetq display-time-day-and-date t)
    (ar/csetq display-time-world-list '(("Europe/Paris" "Paris")
                                        ("Europe/London" "London")
                                        ("America/Los_Angeles" "Los Angeles")))
    (ar/csetq display-time-string-forms
            '((format "%s %s %s, %s:%s"
                      dayname
                      monthname day
                      24-hours minutes)))
    (display-time))

  (ar/vsetq global-mode-string (remove 'display-time-string global-mode-string))
  (ar/vsetq mode-line-end-spaces
            (list (propertize " " 'display '(space :align-to (- right 19)))
                  'display-time-string))

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
  :ensure t
  :if (display-graphic-p)
  :config
  (ar/csetq nyan-bar-length 10)
  (nyan-mode +1))

;; From https://www.reddit.com/r/emacs/comments/b5n1yh/weekly_tipstricketc_thread/ejessje?utm_source=share&utm_medium=web2x
(progn
  (defun ar/list-faces-for-color (color &optional distance)
  "List faces which use COLOR as fg or bg color.

            Accept colors within DISTANCE which defaults to 0."
  (interactive (list (read-color "Color: ")
                     (and current-prefix-arg
                          (prefix-numeric-value current-prefix-arg))))
  (with-help-window (get-buffer-create (format " *%s*" this-command))
    (dolist (face (sort
                   (ar/list-faces--for-color color distance)
                   (lambda (f1 f2)
                     (string< (symbol-name f1)
                              (symbol-name f2)))))
      (ar/list-faces--print-face face)
      (terpri))))

(defun ar/list-faces--print-face (face)
  "Print face and its parents if any."
  (with-current-buffer standard-output
    (let ((fchain (cdr (ar/list-faces--inheritance-chain face :foreground)))
          (bchain (cdr (ar/list-faces--inheritance-chain face :background))))
      (insert (propertize (format "%s" face) 'face face))
      (cond (fchain
             (dolist (face fchain)
               (insert " > " (propertize (format "%s" face) 'face face))))
            (bchain
             (dolist (face bchain)
               (insert " > " (propertize (format "%s" face) 'face face))))))))

(defun ar/list-faces--inheritance-chain (face attr)
  "Return inheritence change for face and attr."
  (let ((g (face-attribute face attr)))
    (if (and (stringp g)
             (not (string= "unspecified" g)))
        (list face)
      (let ((inherit (face-attribute face :inherit)))
        (when inherit
          (if (facep inherit)
              (cons face
                    (ar/list-faces--inheritance-chain inherit attr))
            (if (consp inherit)
                (cl-dolist (face inherit)
                  (let ((res nil))
                    (when (and (facep face)
                               (setq res (ar/list-faces--inheritance-chain face attr)))
                      (cl-return res)))))))))))


(defun ar/list-faces--attribute (face attr)
  "Get face attribute of face as defined or inherited."
  (let* ((chain (ar/list-faces--inheritance-chain face attr)))
    (cl-dolist (f (nreverse chain))
      (let ((g (face-attribute f attr)))
        (when (and (stringp g)
                   (not (string= "unspecified" g)))
          (cl-return g))))))

(defun ar/list-faces--for-color (color &optional distance)
  "Return all faces with COLOR as fg or bg withing DISTANCE."
  (let ((faces ())
        (distance (or distance 0)))
    (mapatoms (lambda (atom)
                (when (facep atom)
                  (let ((fg (ar/list-faces--attribute atom :foreground))
                        (bg (ar/list-faces--attribute atom  :background)))
                    (when (or (and fg
                                   (<= (color-distance
                                        fg
                                        color)
                                       distance))
                              (and bg
                                   (<= (color-distance
                                        bg
                                        color)
                                       distance)))
                      (push atom faces))))))
    (delete-dups faces))))

(defun ar/pick-font ()
  (interactive)
  (let ((font-name (completing-read "Select font:"
                                    (font-family-list))))
    (if (member font-name (font-family-list))
        (set-face-attribute 'default nil :font font-name)
      (error "'%s' font not found" font-name))))

(use-package hide-mode-line
  :ensure t
  :commands hide-mode-line-mode)
