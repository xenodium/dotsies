;;; init.el --- Emacs initialization entry point.
;;; Commentary:
;; Just another init.el file.
;;; Code:

;; Hide UI (early on).
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'toggle-scroll-bar) (toggle-scroll-bar -1))
(when (fboundp 'tooltip-mode) (tooltip-mode -1))

;; Get rid of splash screens.
;; From http://www.emacswiki.org/emacs/EmacsNiftyTricks
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)

;; No Alarms.
(setq ring-bell-function 'ignore)

;; Guarantee that Emacs never loads outdated byte code files.
(setq load-prefer-newer t)

;; Increase memory threshold for garbage collection.
(setq gc-cons-threshold 20000000)

;; From https://github.com/daschwa/emacs.d
;; Nic says eval-expression-print-level needs to be set to nil (turned off) so
;; that you can always see what's happening.
(setq eval-expression-print-level nil)

;; Ask shell for PATH, MANPATH, and exec-path and update Emacs environment.
;; We do this early on as we assert binaries are installed throughout
;; init.
(eval-after-load
    "~/.emacs.d/downloads/exec-path-from-shell/exec-path-from-shell.el"
  '(progn (require 'exec-path-from-shell)
          (exec-path-from-shell-initialize)))

;; Additional load paths.
(add-to-list 'load-path "~/.emacs.d/ar")

;; Show keystrokes earlier (ie. C-x)
(setq echo-keystrokes 0.1)

;; Prevent split-window-sensibly to split horizontally.
(setq split-width-threshold nil)

;; Automatically uncompress/compress compressed files for reading/writing.
(auto-compression-mode t)

;; Customize vertical window divider:
;; Set symbol for the border.
(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?|))

;; Set font face height. Value is 1/10pt.
(set-face-attribute 'default nil :height 180)

(require 'ar-package)
(ar/package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package molokai-theme :ensure t
  :config
  ;; Set default cursor color.
  (setq default-frame-alist
        '((cursor-color . "#FA009A")))

  (set-face-attribute 'isearch nil
                      :foreground "dim gray"
                      :background "yellow")
  (set-face-attribute 'lazy-highlight nil
                      :foreground "yellow"
                      :background "dim gray")
  (set-face-attribute 'highlight nil
                      :background "default"
                      :foreground "blue"
                      :underline t)
  (set-face-attribute 'region nil
                      :background "#FA009A"
                      :foreground "default"))

;; Playing around with dracula-theme.
;; (use-package dracula-theme :ensure t
;;   :config
;;   (custom-theme-set-faces
;;    'dracula
;;    '(default
;;      ((t (:background "black"))))))

;; Find errors in init.el by bisecting the file.
(use-package bug-hunter :ensure t
  :commands (bug-hunter-init-file))

;; Restart Emacs from Emacs.
(use-package restart-emacs :ensure t
  :commands (restart-emacs))

(use-package esup :ensure t)

(use-package dabbrev
  :config
  ;; Case-sensitive fold search search (ie. M-/ to autocomplete).
  (setq dabbrev-case-fold-search nil))
(use-package ar-auto-correct
  :after (abbrev ispell))

(use-package ar-buffer
  :after (ar-process ar-string goto-addr url url-http)
  ;; No need to confirm killing buffers.
  :bind ([(control x) (k)] . kill-this-buffer))
(use-package ar-dired)
(use-package ar-file
  :after (ar-string simple))
(use-package ar-helm
  :after helm)
(use-package ar-helm-objc
  :after (ar-file ar-helm ar-objc)
  :commands (ar/helm-objc-import-update))
(use-package ar-helm-projectile
  :bind ("<f7>" . ar/helm-projectile-shell-cd))
(use-package ar-helm-org
  :after (helm helm-org org))
(use-package ar-helm-shell
  :after (ar-helm ar-shell shell)
  :config
  (bind-key "M-r" #'ar/helm-shell-search-history shell-mode-map))
(use-package ar-helm-url
  :after helm)
(use-package ar-helm-hotspots-config
  :after (ar-dired ar-helm-org ar-org helm-buffers)
  :bind ("C-x b" . ar/helm-hotspots))
(use-package ar-image
  :after (ar-buffer ar-string)
  :commands (ar/image-open-html-for-current-dir))
(use-package ar-imagemagick
  :after (ar-process cl))
(use-package ar-linux)
;; TODO: Migrate to a config module.
(use-package ar-mode-line
  :demand)
(use-package ar-objc
  :after (ar-buffer ar-file)
  :commands (ar/objc-import
             ar/objc-include))
(use-package ar-org
  :after (ar-file ar-time ar-buffer org))
(use-package ar-org-blog
  :after (ar-file ar-org ar-process)
  :commands (ar/org-blog-insert-image
             ar/org-blog-insert-resized-image))
(use-package ar-ping)
(use-package ar-shell
  :after (cl comint))
(use-package ar-url
  :after (ar-buffer ar-input enlive goto-addr)
  :commands (ar/url-view-links-at))
(use-package ar-osx
  :demand
  :commands (ar/osx-convert-plist-to-xml))
(use-package ar-platform
  :after (ar-osx ar-linux)
  :demand
  :bind (("C-x t" . ar/platform-new-browser-tab)))
(use-package ar-ox-html
  :after (ox-html ar-file)
  :config
  (bind-key [f6] #'ar/ox-html-export)
  (ar/ox-html-setup))
(use-package ar-text
  :bind (("C-c c" . ar/text-capitalize-word-toggle)
         ("C-c r" . set-rectangular-region-anchor)))
(use-package ar-yas
  :after yasnippet)

(use-package abbrev
  :after ar-file
  :config
  (setq abbrev-file-name (ar/file-assert-file-exists "~/stuff/active/code/dots/emacs/abbrev_defs"))
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t))

;; From https://github.com/howardabrams/dot-files/blob/HEAD/emacs-client.org
(defun ar/setup-graphical-fonts ()
  "Setup fonts (on graphical display only."
  (deftheme ar/org-theme "Sub-theme to beautify org mode")
  (let* ((sans-font (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                          ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                          ((x-list-fonts "Verdana")         '(:font "Verdana"))
                          ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                          (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
         (base-font-color  (face-foreground 'default nil 'default))
         (background-color (face-background 'default nil 'default))
         (primary-color    (face-foreground 'mode-line nil))
         (secondary-color  (face-background 'secondary-selection nil 'region))
         (headline        `(:inherit default :foreground ,base-font-color))
         (padding         `(:line-width 5 :color ,background-color)))
    (custom-theme-set-faces 'ar/org-theme
                            `(org-agenda-structure ((t (:inherit default ,@sans-font :height 2.0 :underline nil))))
                            `(org-level-8 ((t (,@headline ,@sans-font))))
                            `(org-level-7 ((t (,@headline ,@sans-font))))
                            `(org-level-6 ((t (,@headline ,@sans-font))))
                            `(org-level-5 ((t (,@headline ,@sans-font))))
                            `(org-level-4 ((t (,@headline ,@sans-font :height 1.1   :box ,padding))))
                            `(org-level-3 ((t (,@headline ,@sans-font :height 1.25  :box ,padding))))
                            `(org-level-2 ((t (,@headline ,@sans-font :height 1.5   :box ,padding))))
                            `(org-level-1 ((t (,@headline ,@sans-font :height 1.75  :box ,padding))))
                            `(org-document-title ((t (,@headline ,@sans-font :height 1.5 :underline nil)))))))

(defun ar/setup-graphical-fringe ()
  "Setup up the fringe (graphical display only)."
  (custom-set-faces '(fringe ((t (:background "#1B1D1E"))))))

(defun ar/setup-graphical-mode-line ()
  "Set up graphical mode line."
  (use-package spaceline :ensure t
    :config
    (use-package spaceline-config
      :config
      (spaceline-toggle-minor-modes-off)
      (spaceline-toggle-buffer-encoding-off)
      (spaceline-toggle-buffer-encoding-abbrev-off)
      (setq powerline-default-separator 'rounded)
      (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
      (spaceline-define-segment line-column
        "The current line and column numbers."
        "l:%l c:%2c")
      (spaceline-define-segment time
        "The current time."
        (format-time-string "%H:%M"))
      (spaceline-define-segment date
        "The current date."
        (format-time-string "%h %d"))
      (spaceline-toggle-time-on)
      (spaceline-emacs-theme 'date 'time))))

(use-package tramp
  :config
  ;; Problem with TRAMP mode
  ;; Control Path too long error
  ;; TMPDIR variable is really large
  ;; http://lists.macosforge.org/pipermail/macports-tickets/2011-June/084295.html
  (setenv "TMPDIR" "/tmp")
  (setq tramp-default-method "ssh"))

;; Based on http://www.pygopar.com/setting-emacs-transparency
(defun ar/set-current-frame-alpha-channel (focused-alpha
                                           unfocused-alpha)
  "Set FOCUSED-ALPHA and UNFOCUSED-ALPHA channels for current frame.
Values between 0 - 100."
  (interactive "nOn Focus: \nnOn Unfocus: ")
  (set-frame-parameter (selected-frame)
                       'alpha
                       (list focused-alpha unfocused-alpha)))

;; TODO: Revisit this.
(defun ar/setup-graphical-display ()
  "Setup graphical display."
  (when (window-system)
    (setq frame-title-format '("Ⓔ ⓜ ⓐ ⓒ ⓢ")) ;; Other fun ones 𝔼𝕞𝕒𝕔𝕤
    (toggle-frame-fullscreen)
    (ar/setup-graphical-mode-line)
    (ar/setup-graphical-fringe)
    (ar/setup-graphical-fonts)))
(ar/setup-graphical-display)

;; Tip of the day.
(use-package totd :ensure t
  :commands (totd)
  :config
  (totd-start))

(use-package speed-type :ensure t)

(use-package restclient :ensure t
  :commands (restclient-mode))

;; Display chars/lines or row/columns in the region.
(use-package region-state :ensure t
  :config (region-state-mode))

;; Safely delete packages.
(use-package package-safe-delete :ensure t
  :commands (package-safe-delete))

;; Formats python buffer with yapf
;; Install with: pip install git+https://github.com/google/yapf.git
(use-package py-yapf :ensure t
  :commands (py-yapf-enable-on-save)
  :config (setq py-yapf-options '("--style={based_on_style: google, indent_width: 2}")))

(use-package helm-pydoc :ensure t
  :commands (helm-pydoc))

;; Needs:
;;   brew install Caskroom/cask/xquartz
;;   brew install wordnet
(use-package synosaurus :ensure t
  :commands (synosaurus-lookup
             synosaurus-choose-and-replace))

(use-package wordnut :ensure t
  :after ar-process
  :config (ar/process-assert-binary-installed "wn" "brew/apt-get install wordnet")
  :commands (wordnut-search
             wordnut-lookup-current-word))

;; Peak into macros by expanding them inline.
(use-package macrostep :ensure t)

(use-package async :ensure t :demand)

(use-package enlive :ensure t)

(use-package dired
  :after (discover fullframe)
  :commands dired-mode
  :config
  ;; Use RET instead of "a" in dired.
  (bind-key "RET" #'dired-find-alternate-file dired-mode-map)
  ;; Use ^ for moving to parent dir.
  (bind-key "^" (lambda ()
                  (interactive)
                  (find-alternate-file "..")) dired-mode-map)
  (fullframe dired quit-window)
  ;; Try to guess the target directory for operations.
  (setq dired-dwim-target t)
  ;; Enable since disabled by default.
  (put 'dired-find-alternate-file 'disabled nil)
  ;; Automatically refresh dired buffers when contents changes.
  (setq dired-auto-revert-buffer t)

  (add-hook 'dired-mode-hook 'discover-mode)
  ;; Hide dired details by default.
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))

(use-package dired-subtree :ensure t
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

(use-package fullframe :ensure t
  :commands (fullframe)
  :config
  (use-package ibuffer
    :config (fullframe ibuffer ibuffer-quit))
  (use-package paradox :ensure t
    :commands (paradox-list-packages)
    :config
    (fullframe paradox-list-packages paradox-quit-and-close))
  (use-package magit :ensure t
    :bind ("C-x g" . magit-status)
    :defer 2
    :config
    ;;  Revert visited buffers silently when pullling, merging, etc.
    (setq magit-revert-buffers 'silent)
    (setq magit-status-buffer-switch-function #'switch-to-buffer)
    (add-to-list 'magit-no-confirm 'stage-all-changes)
    (setq magit-push-always-verify nil)
    (setq magit-last-seen-setup-instructions "2.1.0")
    (fullframe magit-status magit-mode-quit-window))

  ;; A screensaver of sorts
  (use-package zone
    :config
    (zone-when-idle 120)
    (setq zone-programs
          [zone-pgm-whack-chars
           zone-pgm-rotate
           zone-pgm-drip
           zone-pgm-martini-swan-dive]))

  ;; Locomotives zone.
  (use-package zone-sl :ensure t
    :after zone
    :config
    (setq zone-programs (vconcat [zone-pgm-sl] zone-programs)))

  ;; A fireplace? Yeah, I know...
  (use-package fireplace :ensure t)

  (use-package zone-rainbow :ensure t
    :after zone
    :config
    (setq zone-programs (vconcat [zone-rainbow] zone-programs)))

  (use-package zone-select :ensure t
    :after zone)

  ;; A Nyan zone. Well, just because.
  ;; (use-package zone-nyan :ensure t
  ;;   :after zone
  ;;   :config
  ;;   (when (window-system)
  ;;     (setq zone-programs (vconcat [zone-nyan] zone-programs))))

  (use-package discover-my-major :ensure t)

  ;; Make Emacs more discoverable (Handy for dired-mode). Trigger with '?'.
  ;; From http://www.masteringemacs.org/article/discoverel-discover-emacs-context-menus
  (use-package discover :ensure t
    :demand
    :commands (discover-mode)))

;; Disabling while trying out spaceline.
;; (defun ar/setup-graphical-mode-line ()
;;   "Set up graphical mode line."
;;   (use-package rich-minority :ensure t)
;;   ;; Hide all minor modes from mode line.
;;   (add-to-list 'rm-whitelist nil t)
;;   (use-package smart-mode-line :ensure t)
;;   ;; Disabling, to try out dark theme.
;;   ;;  (use-package smart-mode-line-powerline-theme :ensure t)
;;   (setq sml/theme 'dark
;;         sml/mule-info nil
;;         sml/show-remote nil
;;         sml/name-width '(20 . 40)
;;         sml/shorten-modes t
;;         sml/mode-width 'right)
;;   (custom-set-faces
;;    '(mode-line ((t (:background "#2A358D" :foreground "gray60")))))
;;   (add-hook 'after-init-hook #'ar/enable-graphical-time)
;;   (sml/setup))

(defun ar/enable-graphical-time ()
  "Enable graphical time in modeline."
  (interactive)
  (setq display-time-24hr-format t)
  (setq display-time-day-and-date t)
  (display-time) ; Align the time to right
  (setq global-mode-string (remove 'display-time-string global-mode-string))
  (setq mode-line-end-spaces
        (list (propertize " " 'display '(space :align-to (- right 17)))
              'display-time-string)))

;; Ensure window is maximized.
(use-package maxframe :ensure t)
(add-hook 'window-setup-hook 'maximize-frame t)

(use-package elfeed :ensure t
  :config
  (setq elfeed-feeds
        '(("http://feeds.feedburner.com/japaneseruleof7" blog japanese-rule-of-7)
          ("http://rubyronin.com/wp-feed.php" blog the-ruby-ronin)
          ("http://emacsredux.com/atom.xml" blog emacs-redux)
          ("http://www.pygopar.com/rss" blog pygopar)
          ("http://planet.emacsen.org/atom.xml" blog emacs)
          ;; ("http://planet.gnome.org/rss20.xml" blog gnome)
          ("http://sachachua.com/blog/feed" blog sachachua)
          ("http://blog.roteiv.com/atom.xml" blog vietor)
          ("https://news.ycombinator.com/rss" news hackernews)
          ("http://reddit.com/r/emacs/.rss" social reddit)
          ("http://dangrover.com/feed.xml" blog dangrover))))
(use-package elfeed-goodies :ensure t :after elfeed
  :config
  (setq elfeed-goodies/entry-pane-position 'bottom)
  (elfeed-goodies/setup))

;; Start off with elfeed.

(use-package bind-key :ensure t)

;; Enable disabled commands.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(use-package anchored-transpose :ensure t)

;; From http://pages.sachachua.com/.emacs.d/Sacha.html#sec-1-7-3
;; Transpose stuff with M-t
(bind-key "M-t" nil) ;; which used to be transpose-words
(bind-key "M-t r" #'anchored-transpose)
(bind-key "M-t l" #'transpose-lines)
(bind-key "M-t w" #'transpose-words)
(bind-key "M-t t" #'transpose-words)
(bind-key "M-t M-t" #'transpose-words)
(bind-key "M-t s" #'transpose-sexps)
(bind-key "C-+" #'text-scale-increase)
(bind-key "C--" #'text-scale-decrease)

(use-package hackernews :ensure t)

;; Stack Exchange viewer.
(use-package sx :ensure t)

;; Search StackOverflow snippets.
(use-package howdoi :ensure t)

(use-package which-key :ensure t
  :config (which-key-mode))

;; Twitter.
(use-package twittering-mode :ensure t)

(use-package rainbow-delimiters :ensure t)

(use-package hungry-delete :ensure t
  :config (global-hungry-delete-mode))

(global-font-lock-mode)
(global-auto-revert-mode)

;; Auto refresh dired.
;; From http://mixandgo.com/blog/how-i-ve-convinced-emacs-to-dance-with-ruby
(setq global-auto-revert-non-file-buffers t)
;; Be quiet about dired refresh.
(setq auto-revert-verbose nil)

;; Let auto-revert-mode update vc/git info.
;; Need it for mode-line-format to stay up to date.
(setq auto-revert-check-vc-info t)

(use-package expand-region :ensure t
  :bind ("C-c w" . er/expand-region))

;; Visual feedback for query-replace, replace, and multiple cursors.
(use-package visual-regexp :ensure t)

(use-package easy-escape :ensure t
  :config
  ;; TODO: Figure out why face foreground isn't displayed.
  (set-face-attribute 'easy-escape-face nil :foreground "red")
  (setq easy-escape-character ?⑊))

(use-package yasnippet :ensure t
  :config
  (setq yas-indent-line 'fixed)
  (setq yas-snippet-dirs
        '("~/.emacs.d/yasnippets/personal"
          "~/.emacs.d/yasnippets/yasnippet-snippets"))
  (yas-reload-all))

;; Back to helm-swoop for now.
;; (use-package swiper :ensure t)
;; (setq swiper-completion-method 'ivy)

;; (defun ar/prefilled-swiper ()
;;   "Pre-fill swiper input with region."
;;   (interactive)
;;   (if (region-active-p)
;;       (let ((region-text (buffer-substring (region-beginning)
;;                                            (region-end))))
;;         (swiper region-text))
;;     (swiper)))

;; (global-set-key (kbd "C-s")
;;                 #'ar/prefilled-swiper)

;; Alert me when moving cursor inefficiently.
(use-package annoying-arrows-mode :ensure t
  :config (global-annoying-arrows-mode))

;; Remember point/place for each file.
(use-package saveplace :defer t
  :init
  (setq-default save-place t)
  (setq save-place-file (expand-file-name ".places"
                                          user-emacs-directory))
  ;; Different functions available across Emacs versions.
  (cond ((fboundp 'save-place-mode)
         (save-place-mode))
        ((fboundp 'toggle-save-place-globally)
         (toggle-save-place-globally))
        (t
         (error "Neither save-place-mode nor toggle-save-place-globally available"))))

(use-package helm
  :config
  ;; Switch major modes and toggle minor modes.
  (use-package helm-source)
  (use-package helm-mode-manager :ensure t)
  (use-package imenu-anywhere :ensure t)
  (use-package helm-ag :ensure t)
  (use-package fontawesome :ensure t)
  (use-package helm-buffers
    :config
    (setq helm-buffers-favorite-modes (append helm-buffers-favorite-modes
                                              '(picture-mode artist-mode)))
    (setq helm-buffer-max-length 40))
  (use-package helm-files)
  (use-package helm-grep)
  (use-package helm-org)
  (use-package helm-swoop :ensure t
    :bind (("M-C-s" . helm-multi-swoop-all)
           ("M-i" . helm-swoop))
    :commands (helm-swoop))
  (use-package helm-config)
  (use-package recentf
    :init
    (recentf-mode)
    :config
    (setq recentf-max-saved-items 200
          recentf-max-menu-items 15))
  (setq helm-net-prefer-curl t)
  (setq helm-scroll-amount 4) ; scroll 4 lines other window using M-<next>/M-<prior>
  (setq helm-quick-update t)  ; do not display invisible candidates
  (setq helm-idle-delay 0.01) ; be idle for this many seconds, before updating in delayed sources.
  (setq helm-input-idle-delay 0.01) ; be idle for this many seconds, before updating candidate buffer
  (setq helm-ff-search-library-in-sexp t)
  (setq helm-split-window-default-side 'below) ;; open helm buffer below.
  (setq helm-split-window-in-side-p t)
  (setq helm-candidate-number-limit 200)
  (setq helm-ff-skip-boring-files t)
  (setq helm-boring-file-regexp-list
        '("\\.git$" "\\.hg$"
          "\\.svn$" "\\.CVS$"
          "\\._darcs$" "\\.la$"
          "\\.o$" "\\.i$"))
  (setq helm-ff-file-name-history-use-recentf t)
  (setq ido-use-virtual-buffers t)
  (setq helm-buffers-fuzzy-matching t)
  (bind-key "<return>" #'helm-grep-mode-jump-other-window helm-grep-mode-map)
  (bind-key "n" #'helm-grep-mode-jump-other-window-forward helm-grep-mode-map)
  (bind-key "p" #'helm-grep-mode-jump-other-window-backward helm-grep-mode-map)
  (bind-key "C-i" #'helm-execute-persistent-action helm-map) ; make TAB works in terminal
  (bind-key "C-z" #'helm-select-action helm-map) ; list actions using C-z
  (bind-key "M-p" #'helm-previous-source helm-map)
  (bind-key "M-n" #'helm-next-source helm-map)
  (helm-mode 1)
  :bind (("C-x C-f" . helm-find-files)
         ("C-c i" . helm-semantic-or-imenu)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-h a" . helm-apropos)
         ("C-h y" . helm-dash-at-point))
  :commands (helm-buffers-list)
  :ensure t)

(use-package helm-dash :ensure t
  :after (go-mode)
  :config
  ;; View documentation in external browser.
  ;; (setq helm-dash-browser-func #'browse-url)
  ;; View documentation in ewww.
  (setq helm-dash-browser-func #'eww))

(defun ar/projectile-helm-ag ()
  "Search current repo/project using ag."
  (interactive)
  (helm-do-ag (projectile-project-root)))

;; From http://stackoverflow.com/questions/6133799/delete-a-word-without-adding-it-to-the-kill-ring-in-emacs
(defun ar/backward-delete-subword (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point)
                 (progn
                   (subword-backward arg)
                   (point))))
(bind-key "M-DEL" #'ar/backward-delete-subword)
(bind-key "<C-backspace>" #'ar/backward-delete-subword)
(bind-key "C-x C-d" "\C-a\C- \C-e\M-w\C-j\C-y")

(bind-key "C-q" #'previous-buffer)
(bind-key "C-z" #'next-buffer)

;; Save current position to mark ring when jumping to a different place
(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

(defun ar/helm-do-grep-recursive (&optional non-recursive)
  "Like `helm-do-grep', but greps recursively by default.
Optional argument NON-RECURSIVE to shallow-search."
  (interactive "P")
  (let* ((current-prefix-arg (not non-recursive))
         (helm-current-prefix-arg non-recursive))
    (call-interactively #'helm-do-grep)))

;; ggtags code indexing.
;; https://github.com/leoliu/ggtags
;; https://github.com/leoliu/ggtags/wiki/Install-Global-with-support-for-exuberant-ctags
;; Linux
;; Install exuberant ctags from trunk.
;; Install GNU Global using ./configure --with-exuberant-ctags=PATH_TO_CTAGS_EXECUTABLE
;; Mac OS
;; brew install --HEAD ctags
;; brew install global --with-exuberant-ctags
;; http://writequit.org/org/settings.html#sec-1-26
(use-package ggtags :ensure t)
(use-package helm-gtags
  :ensure t
  :bind ("M-." . helm-gtags-dwim))
(helm-gtags-mode 1)

(use-package projectile-sift :ensure t
  :config
  (ar/process-assert-binary-installed "sift" "Install via: \
\"brew install sift\" or download from https://sift-tool.org/download"))

(use-package projectile :ensure t
  :config
  (setq projectile-enable-caching nil)
  ;; C-u magit-status presents list of repositories.
  (setq magit-repo-dirs (mapcar (lambda (dir)
                                  (substring dir 0 -1))
                                ;; Disables "required at runtime" warning for cl package.
                                (with-no-warnings
                                  (remove-if-not (lambda (project)
                                                   (file-directory-p (concat project "/.git/")))
                                                 (projectile-relevant-known-projects))))
        magit-repo-dirs-depth 1))
(projectile-global-mode)

;; Best way (so far) to search for files in repo.
(use-package helm-projectile :ensure t
  :demand
  :bind ("C-x f" . helm-projectile))

(use-package ediff
  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)
  (setq ediff-split-window-function #'split-window-horizontally))

;; ediff-revision cleanup.
;; From http://www.emacswiki.org/emacs/DavidBoon#toc8
(defvar ar/ediff-bwin-config nil
  "Window configuration before ediff.")

(defvar ar/ediff-bwin-reg ?b
  "Register to be set up to hold ar/ediff-bwin-config configuration.")

(defun ar/ediff-bsh ()
  "Function to be called before any buffers or window setup for ediff."
  (remove-hook 'ediff-quit-hook #'ediff-cleanup-mess)
  (window-configuration-to-register ar/ediff-bwin-reg))

(defun ar/ediff-aswh ()
  "Setup hook used to remove the `ediff-cleanup-mess' function.  It causes errors."
  (remove-hook 'ediff-quit-hook #'ediff-cleanup-mess))

(defun ar/ediff-qh ()
  "Function to be called when ediff quits."
  (remove-hook 'ediff-quit-hook #'ediff-cleanup-mess)
  (ediff-cleanup-mess)
  (jump-to-register ar/ediff-bwin-reg))

(add-hook 'ediff-before-setup-hook #'ar/ediff-bsh)
(add-hook 'ediff-after-setup-windows-hook #'ar/ediff-aswh);
(add-hook 'ediff-quit-hook #'ar/ediff-qh)

(use-package whitespace
  :config
  ;; When nil, fill-column is used instead.
  (setq whitespace-line-column nil)
  ;; Highlight empty lines, TABs, blanks at beginning/end, lines
  ;; longer than fill-column, and trailing blanks.
  (setq whitespace-style '(face empty tabs lines-tail trailing))
  (set-face-attribute 'whitespace-line nil
                      :foreground "DarkOrange1"
                      :background "default")
  (global-whitespace-mode))

(defun ar/compile-autoclose (buffer string)
  "Hide successful builds window with BUFFER and STRING."
  (cond ((string-match "finished" string)
         (message "Build finished")
         (run-with-timer 2 nil
                         #'delete-window
                         (get-buffer-window buffer t)))
        (t
         (next-error)
         (message "Compilation exited abnormally: %s" string))))

;; Automatically hide successful builds window.
(setq compilation-finish-functions #'ar/compile-autoclose)

;; Automatically scroll build output.
(setq compilation-scroll-output t)

;; Prevent Extraneous Tabs.
;; From http://www.gnu.org/software/emacs/manual/html_node/eintr/Indent-Tabs-Mode.html
(setq-default indent-tabs-mode nil)

;; Override selection with new text.
(delete-selection-mode)

;; Automatically closes brackets.
(electric-pair-mode)
;; Additional electric pairs.
(setq electric-pair-pairs '((?\{ . ?\})
                            (?\< . ?\>)))
(electric-indent-mode)

;; Highlight matching parenthesis.
(use-package paren :ensure t
  :config
  (show-paren-mode 1)
  ;; Without this matching parens aren't highlighted in region.
  (setq show-paren-priority -50)
  (setq show-paren-delay 0)
  ;; Highlight entire bracket expression.
  (setq show-paren-style 'expression)
  (set-face-attribute 'show-paren-match nil
                      :background "default"
                      :foreground "#FA009A"))

(use-package highlight-symbol :ensure t
  :config
  (set-face-attribute 'highlight-symbol-face nil
                      :background "default"
                      :foreground "#FA009A")
  (setq highlight-symbol-idle-delay 0)
  (setq highlight-symbol-on-navigation-p t))

;; Disabling in favor of highlight-symbol.
;; Automatically highlight all instances of thing at point.
;; (use-package highlight-thing :ensure t
;;   :commands highlight-thing-mode
;;   :config
;;   (set-face-attribute 'highlight-thing nil
;;                       :background "default"
;;                       :foreground "#FA009A"))

;; Partially use path in buffer name.
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

;; Enabling subword mode (ie. navigate cameCase)
;; From http://www.emacswiki.org/emacs/CamelCase
(global-subword-mode t)

(use-package git-timemachine :ensure t)

(use-package linum
  :ensure t
  :config
  (set-face-attribute 'linum nil
                      :background "#1B1D1E")
  (setq linum-format "%4d "))

(use-package git-gutter
  :ensure t
  :bind (("C-c <up>" . git-gutter:previous-hunk)
         ("C-c <down>" . git-gutter:next-hunk)))
(global-git-gutter-mode +1)

;; Disabling, since git-commit-mode conflicts with magit.
;; (use-package git-commit-training-wheels-mode :ensure t
;;   :commands (git-commit-training-wheels-mode))
;; (use-package git-commit-mode :ensure t
;;   :config
;;   (add-hook 'git-commit-mode-hook 'git-commit-training-wheels-mode)
;;   :commands (git-commit-mode))

;; Handy pop-up messages with git info.
(use-package git-messenger :ensure t)

;; Highlights current line.
;; Disabling while I try beacon-mode instead.
;;(use-package hl-line :ensure t)

(use-package beacon :ensure t
  :config (beacon-mode))

;; Disable backup.
;; From: http://anirudhsasikumar.net/blog/2005.01.21.html
(setq backup-inhibited t)

;; Disable auto save.
;; From: http://anirudhsasikumar.net/blog/2005.01.21.html
(setq auto-save-default nil)

;; Avoid creating lock files (ie. .#some-file.el)
(setq create-lockfiles nil)

;;  http://scottmcpeak.com/elisp/scott.emacs.el
;;  ------------------- yes-or-no-p ---------------------
;;  There are a number of situations where Emacs wants to ask me a question,
;;  but the answer is always the same (or, it's easy to get the effect of
;;  the other answer afterwards).  The main example is the question:
;;
;;    File foo.txt has changed on disk.  Reread from disk?
;;
;;  This question is annoying because it gets asked while I'm moving around
;;  in a debugger stack trace, and often don't care about the file I happen
;;  to be at (because I want to move past that frame anyway).  Moreover,
;;  my F12 binding lets me re-read files with a single keystroke, so if I
;;  actually *do* want to re-read it's easy to do.

;;  First, I need the original definition of yes-or-no-p so I can call it
;;  after I've replaced its definition.  In case .emacs gets re-read
;;  after startup, avoid losing the original definition.
(if (fboundp 'orig-yes-or-no-p)
    nil        ; it's already bound, don't re-bind
  (fset 'orig-yes-or-no-p (symbol-function 'yes-or-no-p)))

;;  Now, define my version in terms of `orig-yes-or-no-p'.
(defun yes-or-no-p (prompt)
  "Ask user a yes-or-no question.  Return t if answer is yes, nil if no.
This is a wrapper around `orig-yes-or-no'.
Argument PROMPT to check for additional prompt."
  (if (string-match
       ;;  This message is created in lisp/files.el, and there are four
       ;;  variations.  I'm intentionally matching two of them.
       "File .* changed on disk.  Reread from disk"
       prompt)

      ;;  it's that question; the answer is no, but I *do* want to know
      ;;  that it has changed
      (progn (message "Note: File has changed on disk.") nil)

    ;;  it's a different question; for now, just ask me; I'll probably
    ;;  add more patterns to the above as I think of other questions that
    ;;  I don't want asked
    (orig-yes-or-no-p prompt)))

(use-package git-link :ensure t)

(use-package vc
  :commands (vc-pull)
  :bind ("C-x v f" . vc-pull))

;; Use vc-ediff as default.
(eval-after-load "vc-hooks"
  '(define-key vc-prefix-map "=" #'vc-ediff))

(setq css-indent-offset 2)

(use-package markdown-mode+ :ensure t)
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(setq display-time-world-list '(("Europe/Paris" "Paris")
                                ("Europe/London" "London")
                                ("America/Los_Angeles" "Los Angeles")))

;; From http://wenshanren.org/?p=298#more-298
(defun ar/edit-current-file-as-root ()
  "Edit the file that is associated with the current buffer as root."
  (interactive)
  (if (buffer-file-name)
      (let ((file (concat "/sudo:root@localhost:" (buffer-file-name))))
        (find-file file))
    (message "Current buffer does not have an associated file.")))

(use-package hippie-exp
  :bind ("M-/" . hippie-expand)
  :config
  (setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                           try-expand-dabbrev-visible
                                           try-expand-dabbrev-all-buffers
                                           try-expand-dabbrev-from-kill
                                           try-complete-file-name-partially
                                           try-complete-file-nameac
                                           try-expand-all-abbrevs
                                           try-expand-list
                                           try-expand-line)))

;; Thank you Sacha Chua.
;; From http://pages.sachachua.com/.emacs.d/Sacha.html#sec-1-4-8
(fset 'yes-or-no-p 'y-or-n-p)

;; Ensure files end with newline.
(setq require-final-newline t)

;; From http://www.wisdomandwonder.com/wordpress/wp-content/uploads/2014/03/C3F.html
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode +1)
(setq savehist-save-minibuffer-history +1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

;; Don't let the cursor go into minibuffer prompt.
;; From http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
(setq minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

;; From http://pages.sachachua.com/.emacs.d/Sacha.html#sec-1-5-12
(defun ar/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; Removing accidental use. Don't need compose-mail (yet anyway).
(global-unset-key (kbd "C-x m"))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'ar/smarter-move-beginning-of-line)

;; From http://www.reddit.com/r/emacs/comments/25v0eo/you_emacs_tips_and_tricks/chldury
(defun ar/vsplit-last-buffer ()
  "Vertically splitting the screen and open the previous buffer instead of identical buffers."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun ar/hsplit-last-buffer ()
  "Horizontally splitting the screen and open the previous buffer instead of identical buffers."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

(bind-key "C-x 2" #'ar/vsplit-last-buffer)
(bind-key "C-x 3" #'ar/hsplit-last-buffer)

;; Thank you Bozhidar.
;; From https://github.com/bbatsov/prelude/blob/a52cdc83eeec567b13a8a5719a174dfe294ee739/core/prelude-core.el#L340
(defun ar/swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (if (/= (count-windows) 2)
      (message "You need exactly 2 windows to do this.")
    (let* ((w1 (car (window-list)))
           (w2 (cadr (window-list)))
           (b1 (window-buffer w1))
           (b2 (window-buffer w2))
           (s1 (window-start w1))
           (s2 (window-start w2)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 s2)
      (set-window-start w2 s1)))
  (other-window 1))
(bind-key "C-\\" #'ar/swap-windows)

;; From https://github.com/bbatsov/prelude/blob/a52cdc83eeec567b13a8a5719a174dfe294ee739/core/prelude-core.el#L111
(defun ar/smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

;; Do not auto indent current line when pressing <RET>.
(add-hook 'sgml-mode-hook
          (lambda() (local-set-key (kbd "<RET>")
                                   #'electric-indent-just-newline)))

(defun ar/smart-open-line (arg)
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.
With a prefix ARG open line above the current line."
  (interactive "P")
  (if arg
      (ar/smart-open-line-above)
    (progn
      (move-end-of-line nil)
      (newline-and-indent))))

(bind-key "C-o" #'ar/smart-open-line)

;; From https://github.com/ocodo/.emacs.d/blob/master/custom/handy-functions.el
(defun ar/join-line-or-lines-in-region ()
  "Join this line or the lines in the selected region."
  (interactive)
  (cond ((region-active-p)
         (let ((min (line-number-at-pos (region-beginning))))
           (goto-char (region-end))
           (while (> (line-number-at-pos) min)
             (join-line))))
        (t (call-interactively 'join-line))))

(use-package ace-window :ensure t
  :init (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind (("C-x o" . ace-window))
  :config
  ;; Use larger characters for ace window shortcuts.
  ;; From http://oremacs.com/2015/02/27/ace-window-leading-char
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0))))))

;; Interactively resize current window.
(use-package windsize :ensure t)
(windsize-default-keybindings)

(use-package avy :ensure t)
(use-package key-chord :ensure t)
(key-chord-define-global "jj" #'avy-goto-char-2)

;; From http://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer
(defun ar/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(key-chord-define-global "JJ" #'ar/switch-to-previous-buffer)
(key-chord-define-global "BB" #'other-window)
(key-chord-mode +1)

;; Promising background process runner.
(use-package bpr :ensure t)

;; Needs clang-format installed.
;; See http://blog.hardcodes.de/articles/63/building-clang-format-and-friends-on-osx-mountain-lion
;; See http://clang.llvm.org/docs/ClangFormat.html
(use-package clang-format :ensure t)

(defun ar/swift-mode-hook-function ()
  "Called when entering `swift-mode'."
  (setq-local company-backends '(company-sourcekit)))

(use-package swift-mode :ensure t
  :after company-sourcekit
  :config
  (add-hook 'swift-mode-hook #'ar/swift-mode-hook-function))

(use-package company :ensure t
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  (global-company-mode))

(use-package company-sourcekit :ensure t)

(use-package company-quickhelp :ensure t
  :config
  (company-quickhelp-mode +1))

(use-package company-c-headers :ensure t
  :config
  ;; TODO: Set in mode hook.
  ;; (setq company-backends (delete 'company-semantic company-backends))
  ;; (add-to-list 'company-backends 'company-c-headers)
  (bind-key "<backtab>" #'company-complete))

(use-package company-emoji :ensure t)

;; (add-to-list 'load-path
;;              (concat (getenv "HOME") "/.emacs.d/downloads/rtags/src"))
;; (require 'rtags)
;; (require 'company-rtags)
;; (setq rtags-path
;;       (concat (getenv "HOME") "/.emacs.d/downloads/rtags/bin"))
;; (setq company-backends (delete 'company-clang company-backends))
;; (setq company-rtags-begin-after-member-access t)
;; (setq rtags-completions-enabled t)
;; (add-to-list 'company-backends 'company-rtags)
;; (rtags-diagnostics)

;; NOTE: Needs libclang: Install with "brew install llvm --with-clang"
;; By default, irony-install-server does not find libclang on Mac OS.
;; The implementation invokes cmake for you. Ensure you add:
;; -DCMAKE_PREFIX_PATH=/Users/your-user-name/homebrew/opt/llvm
;; For example:
;; cmake -DCMAKE_PREFIX_PATH=/Users/your-user-name/homebrew/opt/llvm -DCMAKE_INSTALL_PREFIX\=/Users/your-user-name/.emacs.d/irony/ /Users/your-user-name/.emacs.d/elpa/irony-20160106.1223/server && cmake --build . --use-stderr --config Release --target install
(use-package irony :ensure t
  :config
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company-irony :ensure t
  :config
  (add-hook 'objc-mode-hook (lambda ()
                              (setq-local company-backends '((company-irony)))))
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands))

(use-package helm-c-yasnippet :ensure t)

(use-package helm-make :ensure t)

(use-package drag-stuff :ensure t
  :bind (("M-<up>" . drag-stuff-up)
         ("M-<down>" . drag-stuff-down)))

;; displays hex strings representing colors
(use-package rainbow-mode :ensure t)

;; If eclim is your cup of tea.
;; (require 'eclim)
;; (global-eclim-mode)
;; (custom-set-variables
;;  '(eclim-eclipse-dirs '("~/tools/eclipse"))
;;  '(eclim-executable "~/tools/eclipse/eclim"))
;; (require 'eclimd)
;; (require 'company-emacs-eclim)
;; (company-emacs-eclim-setup)
;; (global-company-mode t)

;; Relies on manual installation (ie. make emaXcode).
;; Enable auto-complete to use emaXcode while generating snippets.
;;(use-package auto-complete
; :ensure t)
;;(load "~/.emacs.d/downloads/emaXcode/emaXcode.el")
;;(require 'emaXcode)

;; Still evaluating. Disabled for now.
;; (load "~/.emacs.d/downloads/ox-rss/ox-rss.el")
;; (require 'ox-rss)

;; From http://sakito.jp/emacs/emacsobjectivec.html#xcode
(defun ar/xc:build ()
  "Tell Xcode to build."
  (interactive)
  (do-applescript
   (format
    (concat
     "tell application \"Xcode\" to activate \r"
     "tell application \"System Events\" \r"
     "     tell process \"Xcode\" \r"
     "          key code 11 using {command down} \r"
     "    end tell \r"
     "end tell \r"
     ))))

(defun ar/xc:run ()
  "Tell Xcode to run."
  (interactive)
  (do-applescript
   (format
    (concat
     "tell application \"Xcode\" to activate \r"
     "tell application \"System Events\" \r"
     "     tell process \"Xcode\" \r"
     "          key code 15 using {command down} \r"
     "    end tell \r"
     "end tell \r"
     ))))

;;  Note: For ycmd.
;;  * No need for global_ycm_extra_conf.py
;;    (Use .ycm_extra_conf.py)
;;  * Needs .ycm_extra_conf.py (pointing to compile_commands.json location)
;;    (See emacs/ycmd/.ycm_extra_conf.py)
;;  * Needs compile_commands.json
;;    (See http://blog.patspam.com/2014/vim-objc-code-completion)
;;  * Add objc-mode to company-ycmd--extended-features-modes in company-ycmd.el
;; (use-package ycmd
;; :ensure t)
;;
;; (use-package company-ycmd
;; :ensure t)
;;
;; (setq ycmd-server-command (list "python"
;;                                 (expand-file-name "~/.emacs.d/downloads/ycmd/ycmd")))
;; (setq ycmd--log-enabled t)

;; Consider elpy mode instead. See https://github.com/daschwa/emacs.d
;; Consider company jedi. See https://github.com/syohex/emacs-company-jedi
(use-package anaconda-mode :ensure t
  :commands (anaconda-mode))

(use-package company-anaconda :ensure t)

(use-package python-docstring :ensure t
  :commands (python-docstring-mode))

(defun ar/org-mode-hook-function ()
  "Called when entering org mode."
  (add-hook 'after-change-functions
            #'ar/after-prog-mode-text-change
            t t)
  (toggle-truncate-lines 0)
  (setq show-trailing-whitespace t)
  (set-fill-column 1000)
  (ar/org-src-color-blocks-dark)
  (flyspell-mode)
  (org-bullets-mode 1)
  (yas-minor-mode 1)
  (org-display-inline-images))

(use-package org :ensure t :config
  (add-hook 'org-mode-hook #'ar/org-mode-hook-function))

(use-package ob
  :config
  (setq org-export-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (ditaa . t)
     (dot . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (haskell . nil)
     (ocaml . nil)
     (python . t)
     (ruby . t)
     (screen . nil)
     (sh . t)
     (js . t)
     (sql . nil)
     (sqlite . t))))

;; Plan London Underground journeys.
(use-package org-tfl :ensure t
  :after org)

(use-package ox-html)

;; From http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun ar/narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or defun,
whichever applies first. Narrowing to org-src-block actually
calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is
already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if you
         ;; don't want it.
         (cond ((ignore-errors (org-edit-src-code))
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

(bind-key "C-x n n" #'ar/narrow-or-widen-dwim)

;; Enable searching info via info-lookup-symbol (ie. C-h S).
(use-package pydoc-info :ensure t)

(defun ar/python-mode-hook-function ()
  "Called when entering `python-mode'."
  (setq python-indent-offset 2)
  (anaconda-mode)
  (eldoc-mode +1)
  ;; FIXME python-docstring-mode currently broken
  ;; (python-docstring-mode +1)
  (setq-local company-backends '(company-anaconda))
  (py-yapf-enable-on-save))
(add-hook 'python-mode-hook #'ar/python-mode-hook-function)

(use-package objc-font-lock
  :ensure t
  :init (setq objc-font-lock-background-face nil))

(use-package dummy-h-mode :ensure t)
(add-to-list 'auto-mode-alist '("\\.h\\'" . dummy-h-mode))

(use-package go-eldoc :ensure t
  :commands go-eldoc-setup)

(use-package gotest :ensure t)

(use-package go-snippets :ensure t)

(use-package go-mode :ensure t)
;; Requires gocode daemon. Install with:
;; go get -u github.com/nsf/gocode
;; go get -u code.google.com/p/rog-go/exp/cmd/godef
;; go get -u code.google.com/p/go.tools/cmd/goimports
;; Useful info at:
;; From http://tleyden.github.io/blog/2014/05/22/configure-emacs-as-a-go-editor-from-scratch
;; From http://tleyden.github.io/blog/2014/05/27/configure-emacs-as-a-go-editor-from-scratch-part-2
;; From http://dominik.honnef.co/posts/2013/03/writing_go_in_emacs
(defun ar/go-mode-hook-function ()
  "Called when entering `go-mode'."
  (helm-dash-activate-docset "Go")
  (setq-local company-backends '(company-go))
  (company-mode)
  (go-eldoc-setup)
  (setq tab-width 2 indent-tabs-mode 1)
  (add-hook 'before-save-hook #'gofmt-before-save))

(use-package company-go :ensure t
  :config
  (add-hook 'go-mode-hook #'ar/go-mode-hook-function))

(use-package golint :ensure t)

;; From http://endlessparentheses.com/faster-pop-to-mark-command.html?source=rss
(defadvice pop-to-mark-command (around ensure-new-position activate)
  "Continue popping mark until the cursor is actually moved."
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))
(setq set-mark-command-repeat-pop t)

(defun ar/split-camel-region ()
  "Splits camelCaseWord to camel case word."
  (interactive)
  (let ((case-fold-search nil))
    (while (re-search-forward "[A-Z]" (region-end) t)
      (replace-match (format " %s"
                             (downcase (match-string 0)))
                     t nil))))
(bind-key "C-c l" #'ar/split-camel-region)

;; Simplify lisp navigation/editing (ie. slurp/barf).
;; Disabling lispy for the time being (affecting imenu).
;; (use-package lispy :ensure t
;;   :config
;;   (bind-key "M-i" #'helm-swoop lispy-mode-map))

;; M-. elisp navigation.
(use-package elisp-slime-nav :ensure t)

;; Evaluate line on the fly and overlay result.
(use-package litable :ensure t)

;; Edit HTML templates in Javascript code (automatically escape).
(use-package string-edit :ensure t)

(defun ar/add-functions-to-mode-hooks (hook-functions hooks)
  "Add HOOK-FUNCTIONS to mode HOOKS."
  (dolist (hook hooks)
    (dolist (hook-function hook-functions)
      (add-hook hook hook-function))))

(defun ar/emacs-lisp-mode-hook-function ()
  "Called when entering `emacs-lisp-mode'."
  (helm-dash-activate-docset "Emacs Lisp")
  ;; Pretty print output to *Pp Eval Output*.
  (local-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)
  ;; Disabling lispy for the time being (affecting imenu).
  ;; (lispy-mode 1)
  ;; (setq-local company-backends '((company-yasnippet company-dabbrev-code company-emoji company-capf company-keywords company-files)))
  (setq-local company-backends '((company-yasnippet
                                  company-dabbrev-code
                                  company-keywords
                                  company-files
                                  company-emoji
                                  company-capf)))
  ;; (add-to-list 'company-backends 'company-yasnippet)
  (eldoc-mode)
  (set-fill-column 70)
  (turn-on-elisp-slime-nav-mode))

(ar/add-functions-to-mode-hooks '(ar/emacs-lisp-mode-hook-function)
                                '(emacs-lisp-mode-hook
                                  ielm-mode-hook))

;; Super handy for highlighting bookmarks.
(use-package bookmark+ :ensure t)

(defun ar/save-point-to-register ()
  "Save point to register."
  (interactive)
  (point-to-register 9999))

(defun ar/jump-to-saved-point-register ()
  "Jumps cursor to register 9999's value."
  (interactive)
  (jump-to-register 9999))
(bind-key "C-c `" #'ar/jump-to-saved-point-register)

(defun ar/after-prog-mode-text-change (beg end len)
  "Execute for all text modifications in `prog-mode'.
Argument BEG beginning.
Argument END end.
Argument LEN Length."
  ;; Saving point enables jumping back to last change at any time.
  (ar/save-point-to-register))

(defun ar/clang-format-buffer ()
  "Clang format current buffer."
  (interactive)
  (clang-format (point-min)
                (point-max)))

;; Disabled. Figure out the right helm-xcdoc-document-path.
;; (use-package helm-xcdoc :ensure t
;;   :config
;;   (setq
;;    helm-xcdoc-command-path (ar/file-assert-file-exists "/Applications/Xcode.app/Contents/Developer/usr/bin/docsetutil")
;;    helm-xcdoc-document-path (ar/file-assert-file-exists "/Applications/Xcode.app/Contents/Developer/Documentation/DocSets/com.apple.adc.documentation.iOS.docset")))

(defun ar/objc-mode-hook-function ()
  "Called when entering `objc-mode'."
  (add-hook 'before-save-hook
            #'ar/clang-format-buffer
            nil
            'make-it-local)
  (objc-font-lock-mode)
  (helm-dash-activate-docset "iOS")
  (set-fill-column 100)
  ;; NOTE: Disabling while trying irony out
  ;; (setq-local company-backends
  ;;      ;; List with multiple back-ends for mutual inclusion.
  ;;      '(( ;;company-ycmd
  ;;         company-yasnippet
  ;;         company-gtags
  ;;         company-dabbrev-code
  ;;         company-files)))
  ;;(ycmd-mode)

  ;; List targets with xcodebuild -list
  ;; List SDKS with xcodebuild -sdk -version, for example:
  ;; iPhoneSimulator7.1.sdk - Simulator - iOS 7.1 (iphonesimulator7.1)
  ;; SDKVersion: 7.1
  ;; Path: /Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator7.1.sdk
  ;; PlatformPath: /Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform
  ;; ProductBuildVersion: 11D167
  ;; ProductCopyright: 1983-2014 Apple Inc.
  ;; ProductName: iPhone OS
  ;; ProductVersion: 7.1
  (setq-local compile-command
       "xcodebuild -sdk iphonesimulator7.1 -target MyTarget")
  (local-set-key (kbd "<f7>")
                 #'ar/xc:build)
  (local-set-key (kbd "<f8>")
                 #'ar/xc:run)
  (key-chord-define (current-local-map) ";;" "\C-e;"))
(add-hook 'objc-mode-hook #'ar/objc-mode-hook-function)

(defun ar/java-mode-hook-function ()
  "Called when entering `java-mode'."
  (bind-key [f6] java-mode-map)
  ;; 2-char indent for java.
  (defvar c-basic-offset)
  (setq c-basic-offset 2)
  (set-fill-column 100))

(add-hook 'java-mode-hook #'ar/java-mode-hook-function)

(use-package tldr :ensure t)

;; Produce HTML from CSS-like selectors. TODO: Enable for HTML mode.
(use-package emmet-mode :ensure t)

(use-package nodejs-repl :ensure t)

(defun ar/js2-mode-hook-function ()
  "Called when entering `js2-mode'."
  (requirejs-mode)
  (js2-imenu-extras-setup)
  (setq-local js2-basic-offset 2)
  (setq company-tooltip-align-annotations t)
  (setq company-tern-meta-as-single-line t)
  (setq company-tern-property-marker "")
  (tern-mode 1)
  ;; Moving about by list and expression.
  ;; From http://jbm.io/2014/01/react-in-emacs-creature-comforts/
  (modify-syntax-entry ?< "(>")
  (modify-syntax-entry ?> ")<"))

(use-package html-check-frag :ensure t
  :config
  (add-hook 'html-mode-hook (lambda ()
                              (html-check-frag-mode 1))))

(use-package requirejs :ensure t)

(use-package js2-mode :ensure t
  :after requirejs-emacs
  ;; Enable for node
  ;; :interpreter "node"
  :config
  ;; Enable for node
  ;; (ar/process-assert-binary-installed "node")
  (add-hook #'js2-mode-hook #'ar/js2-mode-hook-function))

(defun ar/dart-mode-hook-function ()
  "Called when entering `dart-mode'."
  (flycheck-mode))

(use-package dart-mode :ensure t
  :config
  ;; TODO: Add analysis server path.
  (add-hook 'dart-mode-hook #'ar/dart-mode-hook-function))

(use-package json-mode :ensure t)

;; Needs
;; npm install -g eslint-plugin-flowtype
;; npm install -g eslint
;; TODO: Add eslint flycheck.

;; Needs
;; npm install -g jscs
;; npm install -g esprima-fb
;; and .jscsrc with:
;; {
;;   "preset": "google",
;;   "esnext": true
;; }
;; Not working on Linux. Disabled.
;;(use-package jscs :ensure t)

;; I prefer sentences to end with one space instead.
(setq sentence-end-double-space nil)

(use-package flyspell
  :after ar-auto-correct
  :config (bind-key "C-M-i" #'ar/auto-correct-ispell-word-then-abbrev flyspell-mode-map))

(use-package fill-column-indicator :ensure t
  :commands (turn-on-fci-mode))

(defun ar/web-mode-hook-function ()
  "Called when entering `js2-mode'."
  (js2-imenu-extras-setup)
  (setq-local web-mode-code-indent-offset 2)
  (setq-local web-mode-markup-indent-offset 2)
  (setq-local web-mode-sql-indent-offset 2)
  (setq-local web-mode-css-indent-offset 2)
  (setq-local indent-tabs-mode nil)
  (setq-local standard-indent 2)
  (set-fill-column 70)
  ;; Not working on Linux. Disabled.
  ;; (jscs-fix-run-before-save)
  ;; Moving about by list and expression.
  ;; From http://jbm.io/2014/01/react-in-emacs-creature-comforts/
  (modify-syntax-entry ?< "(>")
  (modify-syntax-entry ?> ")<"))

;; Work in progress.
(use-package web-mode :ensure t
  :config
  (setq web-mode-code-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it))
  (add-hook #'web-mode-hook #'ar/web-mode-hook-function))

(use-package company-tern :ensure t
  ;; :config
  ;; TODO add in mode hook.
  ;; (add-to-list 'company-backends 'company-tern)
  )

(defun ar/js-mode-hook-function ()
  "Called when entering `js-mode'."
  (setq-local company-tooltip-align-annotations t)
  (setq-local company-tern-meta-as-single-line t)
  (setq-local company-tern-property-marker "")
  (setq-local js-indent-level 2))

(add-hook 'js-mode-hook #'ar/js-mode-hook-function)

;; From https://github.com/howardabrams/dot-files/blob/HEAD/emacs-client.org
(use-package color-theme-sanityinc-tomorrow :ensure t)

;; From https://github.com/howardabrams/dot-files/blob/HEAD/emacs-client.org
;; (ar/change-theme 'color-theme-sanityinc-tomorrow-night
;;                  'ar/org-src-color-blocks-dark)

(use-package centered-cursor-mode :ensure t
  :pin melpa
  :init
  ;; Workaround to use centered-cursor-mode in --nw.
  (defvar mouse-wheel-mode nil))

(defun ar/company-fci-workaround ()
  "Enable a workaround to disable fci while company-completing."
  (defvar-local company-fci-mode-on-p nil
    "Keep track if fci-mode if currently on.")
  ;; Disable fci if needed.
  (add-hook 'company-completion-started-hook (lambda (&rest ignore)
                                               (when (boundp 'fci-mode)
                                                 (setq company-fci-mode-on-p fci-mode)
                                                 (when fci-mode (fci-mode -1)))))
  ;; Re-enable fci if needed.
  (add-hook 'company-completion-finished-hook (lambda (&rest ignore)
                                                (when company-fci-mode-on-p (fci-mode 1))))
  ;; Re-enable fci if needed.
  (add-hook 'company-completion-cancelled-hook (lambda (&rest ignore)
                                                 (when company-fci-mode-on-p (fci-mode 1)))))
(defun ar/prog-mode-hook-function ()
  "Called when entering all programming modes."
  (add-hook 'after-change-functions
            #'ar/after-prog-mode-text-change
            t t)
  (let ((m prog-mode-map))
    (define-key m [f6] #'recompile))
  (highlight-symbol-mode +1)
  (highlight-symbol-nav-mode +1)
  (setq show-trailing-whitespace t)
  ;; Spellcheck comments and documentation
  ;; From http://mwolson.org/projects/emacs-config/init.el.html
  (flyspell-prog-mode)
  (rainbow-delimiters-mode)
  ;;(hl-line-mode)
  (rainbow-mode)
  (centered-cursor-mode)
  ;; Language-aware editing commands. Useful for imenu-menu.
  (semantic-mode 1)
  (turn-on-fci-mode)
  (ar/company-fci-workaround)
  (yas-minor-mode 1))

(defun ar/markdown-mode-hook-function ()
  "Called when entering `markdown-mode'."
  (setq-local markdown-indent-on-enter nil)
  (local-set-key (kbd "RET")
                 #'electric-newline-and-maybe-indent))

(ar/add-functions-to-mode-hooks '(ar/prog-mode-hook-function)
                                '(prog-mode-hook))

(ar/add-functions-to-mode-hooks '(ar/prog-mode-hook-function
                                  ar/markdown-mode-hook-function)
                                '(markdown-mode-hook))

;; Select help window by default.
(setq help-window-select t)

;; No need to confirm killing buffers.
(global-set-key [(control x) (k)]
                #'kill-this-buffer)

(use-package shell-pop :ensure t
  :init
  ;; Customize shell-pop.
  (setq shell-pop-term-shell "/bin/bash")
  ;; Trying shell out. Disabling ansi-term for now.
  ;; (setq shell-pop-shell-type '("ansi-term"
  ;;                              "terminal"
  ;;                              (lambda
  ;;                                nil (ansi-term shell-pop-term-shell))))
  (setq shell-pop-window-position "full")
  ;; Do not auto cd to working directory.
  (setq shell-pop-autocd-to-working-dir nil)
  :bind (([f5] . shell-pop)))

(defun ar/shell-mode-hook-function ()
  "Called when entering shell mode."
  ;; Enable company completion on TAB when in shell mode.
  ;; (company-mode)
  ;; (bind-key "TAB" #'company-manual-begin shell-mode-map)
  )

(use-package shell
  :commands shell-mode
  :init
  (add-hook #'shell-mode-hook #'ar/shell-mode-hook-function))

(defun ar/term-mode-hook-function ()
  "Called when entering term mode."
  ;; Don't need trailing spaces highlighted in terminal.
  (setq-local whitespace-style '(face empty tabs)))

(add-hook 'term-mode-hook #'ar/term-mode-hook-function)

;; From http://endlessparentheses.com/a-comment-or-uncomment-sexp-command.html
(defun ar/uncomment-sexp (&optional n)
  "Uncomment a sexp around point."
  (interactive "P")
  (let* ((initial-point (point-marker))
         (p)
         (end (save-excursion
                (when (elt (syntax-ppss) 4)
                  (re-search-backward comment-start-skip
                                      (line-beginning-position)
                                      t))
                (setq p (point-marker))
                (comment-forward (point-max))
                (point-marker)))
         (beg (save-excursion
                (forward-line 0)
                (while (= end (save-excursion
                                (comment-forward (point-max))
                                (point)))
                  (forward-line -1))
                (goto-char (line-end-position))
                (re-search-backward comment-start-skip
                                    (line-beginning-position)
                                    t)
                (while (looking-at-p comment-start-skip)
                  (forward-char -1))
                (point-marker))))
    (unless (= beg end)
      (uncomment-region beg end)
      (goto-char p)
      ;; Indentify the "top-level" sexp inside the comment.
      (while (and (ignore-errors (backward-up-list) t)
                  (>= (point) beg))
        (skip-chars-backward (rx (syntax expression-prefix)))
        (setq p (point-marker)))
      ;; Re-comment everything before it. 
      (ignore-errors
        (comment-region beg p))
      ;; And everything after it.
      (goto-char p)
      (forward-sexp (or n 1))
      (skip-chars-forward "\r\n[:blank:]")
      (if (< (point) end)
          (ignore-errors
            (comment-region (point) end))
        ;; If this is a closing delimiter, pull it up.
        (goto-char end)
        (skip-chars-forward "\r\n[:blank:]")
        (when (= 5 (car (syntax-after (point))))
          (delete-indentation))))
    ;; Without a prefix, it's more useful to leave point where
    ;; it was.
    (unless n
      (goto-char initial-point))))

(defun ar/comment-sexp--raw ()
  "Comment the sexp at point or ahead of point."
  (pcase (or (bounds-of-thing-at-point 'sexp)
             (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (bounds-of-thing-at-point 'sexp)))
    (`(,l . ,r)
     (goto-char r)
     (skip-chars-forward "\r\n[:blank:]")
     (comment-region l r)
     (skip-chars-forward "\r\n[:blank:]"))))

;; From http://endlessparentheses.com/a-comment-or-uncomment-sexp-command.html
(defun ar/comment-or-uncomment-sexp (&optional n)
  "Comment the sexp at point and move past it.
If already inside (or before) a comment, uncomment instead.
With a prefix argument N, (un)comment that many sexps."
  (interactive "P")
  (if (or (elt (syntax-ppss) 4)
          (< (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (point))
             (save-excursion
               (comment-forward 1)
               (point))))
      (ar/uncomment-sexp n)
    (dotimes (_ (or n 1))
      (ar/comment-sexp--raw))))

(defun ar/comment-dwim ()
  "Comment current line or region."
  (interactive)
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (when (region-active-p)
      (setq start (save-excursion
                    (goto-char (region-beginning))
                    (beginning-of-line)
                    (point))
            end (save-excursion
                  (goto-char (region-end))
                  (end-of-line)
                  (point))))
    (comment-or-uncomment-region start end)))
(bind-key "M-;" #'ar/comment-dwim)

(defun ar/new-file-with-snippet (name extension mode snippet-name &optional interactive-snippet-p)
  "Create file with NAME, EXTENSION, MODE, SNIPPET-NAME, and optional INTERACTIVE-SNIPPET-P."
  (find-file (format "%s%s" name extension))
  (funcall mode)
  (insert snippet-name)
  (yas-expand-from-trigger-key)
  (unless interactive-snippet-p
    (yas-exit-all-snippets)))

(defun ar/new-objc-file ()
  "Create and `yas-expand' Objective-C interface header/implementation files."
  (interactive)
  (let ((interface-name (read-from-minibuffer "Interface name: ")))
    (ar/new-file-with-snippet interface-name
                              ".h"
                              'objc-mode
                              "inter")
    (ar/new-file-with-snippet interface-name
                              ".m"
                              'objc-mode
                              "impl")))

(defun ar/find-all-dired-current-dir ()
  "Invokes `find-dired' for current dir."
  (interactive)
  (let ((dir (if buffer-file-name
                 (file-name-directory buffer-file-name)
               ".")))
    (find-dired dir "'(' -name .svn -o -name .git ')' -prune -o -type f")))

;; Quickly undo pop-ups or other window configurations.
(use-package winner :ensure t
  :init (winner-mode 1)
  :config
  (setq winner-boring-buffers
        (append winner-boring-buffers '("*helm M-x*"
                                        "helm mini*"
                                        "*helm projectile*")))
  :bind (("<escape>" . winner-undo)
         ("C-c <right>" . winner-redo)
         ("C-c <left>" . winner-undo)))

(use-package helm-descbinds :ensure
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds)))

(use-package auto-compile :ensure t
  :demand
  :config
  (auto-compile-on-load-mode 1)
  (auto-compile-on-save-mode 1))

;; Collaborate with clipboard.
(setq x-select-enable-clipboard t)
;; More expected region behaviour.
(transient-mark-mode t)

;;  Make a shell script executable automatically on save.
;;  From https://github.com/bbatsov/prelude
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;;  Automatically indent yanked code
;;  From http://www.emacswiki.org/emacs/AutoIndentation#toc3
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode '(emacs-lisp-mode
                                     lisp-mode
                                     markdown-mode
                                     clojure-mode
                                     scheme-mode
                                     haskell-mode
                                     ruby-mode
                                     rspec-mode
                                     python-mode
                                     c-mode
                                     c++-mode
                                     objc-mode
                                     latex-mode
                                     plain-tex-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

(bind-key "C-x C-r" #'eval-region)

(use-package goto-addr)
(defun ar/helm-buffer-url-candidates ()
  "Generate helm candidates for all URLs in buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((helm-candidates '())
          (url))
      (while (re-search-forward goto-address-url-regexp
                                nil t)
        (setq url
              (buffer-substring-no-properties (match-beginning 0)
                                              (match-end 0)))
        (add-to-list 'helm-candidates
                     (cons url
                           url)))
      helm-candidates)))

(defun ar/helm-buffer-urls ()
  "Narrow down and open a URL in buffer."
  (interactive)
  (helm :sources `(((name . "Buffer URLs")
                    (candidates . ,(ar/helm-buffer-url-candidates))
                    (action . (lambda (url)
                                (browse-url url)))))))

;;  From http://oremacs.com/2015/01/05/youtube-dl
(defun ar/youtube-download ()
  "Download youtube video from url in clipboard."
  (interactive)
  (let* ((url (current-kill 0))
         (default-directory "~/Downloads")
         (proc (get-buffer-process (ansi-term "/bin/bash"))))
    (term-send-string
     proc
     (concat "cd ~/Downloads && youtube-dl " url "\n"))))

(defun ar/view-clipboard-buffer ()
  "View clipboard buffer."
  (interactive)
  (let* ((clipboard-content (current-kill 0))
         (clipboard-buffer (get-buffer-create "*Clipboard*")))
    (switch-to-buffer clipboard-buffer)
    (erase-buffer)
    (insert clipboard-content)
    (prog-mode)
    (toggle-truncate-lines 0)))
(bind-key "C-c y" #'ar/view-clipboard-buffer)

;;  Save Emacs state from one session to another.
;;  Disabling. Trial without it.
;; (desktop-save-mode 1)
;;  Number of buffers to restore immediately.
;; (setq desktop-restore-eager 10)

(defun ar/desktop-save ()
  "Write the desktop save file to ~/.emacs.d."
  (desktop-save (expand-file-name "~/.emacs.d/")))

;; Is this what's locking things up?
;; (run-with-idle-timer 300 t 'ar/desktop-save)

(defun ar/load-all-files (pattern)
  "Load all files found by PATTERN, ie. (ar/load-all-files '~/*.el')."
  (dolist (file (file-expand-wildcards pattern))
    (load file)))

(use-package google-translate :ensure t)

;; From http://ergoemacs.org/emacs/emacs_copy_file_path.html
(defun ar/copy-file-path (&optional φdir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
If `universal-argument' is called, copy only the dir path.
Version 2015-01-14
URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'
Optional argument ΦDIR-PATH-ONLY-P if copying buffer directory."
  (interactive "P")
  (let ((fPath
         (if (equal major-mode 'dired-mode)
             default-directory
           (buffer-file-name))))
    (kill-new
     (if (equal φdir-path-only-p nil)
         fPath
       (file-name-directory fPath)))
    (message "File path copied: %s" fPath)))

(defun ar/open-file-at-point ()
  "Open the file path at point.
If there is text selection, uses the text selection for path.
If the path starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
Path may have a trailing “:‹n›” that indicates line number.
If so, jump to that line number.
If path does not have a file extention, automatically try with “.el” for elisp
files.
This command is similar to `find-file-at-point' but without prompting for
confirmation.

URL `http://ergoemacs.org/emacs/emacs_open_file_path_fast.html'"
  (interactive)
  (let ((ξpath (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (let (p0 p1 p2)
                   (setq p0 (point))
                   ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
                   (skip-chars-backward "^  \"\t\n`'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\`")
                   (setq p1 (point))
                   (goto-char p0)
                   (skip-chars-forward "^  \"\t\n`'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\'")
                   (setq p2 (point))
                   (goto-char p0)
                   (buffer-substring-no-properties p1 p2)))))
    (if (string-match-p "\\`https?://" ξpath)
        (browse-url ξpath)
      (progn ; not starting “http://”
        (if (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\'" ξpath)
            (progn
              (let (
                    (ξfpath (match-string 1 ξpath))
                    (ξline-num (string-to-number (match-string 2 ξpath))))
                (if (file-exists-p ξfpath)
                    (progn
                      (find-file ξfpath)
                      (goto-char 1)
                      (forward-line (1- ξline-num)))
                  (progn
                    (when (y-or-n-p (format "File doesn't exist: %s.  Create? " ξfpath))
                      (find-file ξfpath))))))
          (progn
            (if (file-exists-p ξpath)
                (find-file ξpath)
              (if (file-exists-p (concat ξpath ".el"))
                  (find-file (concat ξpath ".el"))
                (when (y-or-n-p (format "File doesn't exist: %s.  Create? " ξpath))
                  (find-file ξpath ))))))))))

(use-package flycheck :ensure t)

(use-package flycheck-tip :ensure t
  :config
  (flycheck-tip-use-timer 'verbose))

;; No Objective-C 'other file' support out of the box. Fix that.
(setq cc-other-file-alist
      `(("\\.cpp$" (".hpp" ".h"))
        ("\\.h$" (".c" ".cpp" ".m" ".mm"))
        ("\\.hpp$" (".cpp" ".c"))
        ("\\.m$" (".h"))
        ("\\.mm$" (".h"))))

;; Usually, both `C-x C-m' and `C-x RET' invoke the
;; `mule-keymap', but that's a waste of keys. Here we put it
;; _just_ under `C-x RET'.
;; From http://endlessparentheses.com/multiple-cursors-keybinds.html
(define-key ctl-x-map (kbd "<return>") mule-keymap)

(defun ar/find-dired-current-dir ()
  "Find files from current location."
  (interactive)
  (helm-find t))

;; Ensure clipboard makes it into kill ring even if killing other text.
(setq save-interprogram-paste-before-kill t)

(use-package multiple-cursors :ensure t
  :bind (("C-c a" . mc/mark-all-like-this-dwim)
         ("C-c n" . mc/mark-more-like-this-extended)
         ("M-1" . mc/mark-next-like-this)
         ("M-!" . mc/unmark-next-like-this)
         ("M-2" . mc/mark-previous-like-this)
         ("M-@" . mc/unmark-previous-like-this)))

(use-package phi-search :ensure t)
(use-package phi-search-mc :ensure t
  :config
  (phi-search-mc/setup-keys))

;; Modify multiple cursors.
(use-package broadcast :ensure t)

(defun ar/numeric-clipboard-or-prompt (prompt)
  "Return an integer from clipboard or PROMPT."
  (let* ((clipboard (current-kill 0))
         (number (if (ar/string-numeric-p clipboard)
                     clipboard
                   (read-string (format "%s: "
                                        prompt)))))
    number))

(defun ar/alpha-numeric-clipboard-or-prompt (prompt)
  "Return an alphanumeric string from clipboard or PROMPT."
  (let* ((clipboard (current-kill 0))
         (alpha-num-string (if (ar/string-alpha-numeric-p clipboard)
                     clipboard
                   (read-string (format "%s: "
                                        prompt)))))
    alpha-num-string))

(use-package hydra :ensure t)
(setq hydra-is-helpful t)

;; Shows keyboard macros as Emacs lisp.
(use-package elmacro :ensure t)

(defhydra hydra-vc-log-edit (:color blue :hint nil)
  "
_u_pdate _r_eview comments
_t_ypo
"
  ("u" (lambda ()
         (interactive)
         (insert "Updating")
         (log-edit-done)))
  ("t" (lambda ()
         (interactive)
         (insert "Fixing typo")
         (log-edit-done)))
  ("r" (lambda ()
         (interactive)
         (insert "Addressing review comments")
         (log-edit-done)))
  ("q" nil "quit"))
(add-hook 'vc-git-log-edit-mode-hook #'hydra-vc-log-edit/body)

(defhydra hydra-git-commit (:color blue :hint nil)
  "
_u_pdate _r_eview comments
_t_ypo
"
  ("u" (lambda ()
         (interactive)
         (insert "Updating")
         (git-commit-commit)))
  ("t" (lambda ()
         (interactive)
         (insert "Fixing typo")
         (git-commit-commit)))
  ("r" (lambda ()
         (interactive)
         (insert "Addressing review comments")
         (git-commit-commit)))
  ("q" nil "quit"))

(defhydra hydra-apropos (:color blue :hint nil)
  "
_a_propos        _c_ommand
_d_ocumentation  _l_ibrary
_v_ariable       _u_ser-option
^ ^          valu_e_"
  ("a" apropos)
  ("d" apropos-documentation)
  ("v" apropos-variable)
  ("c" apropos-command)
  ("l" apropos-library)
  ("u" apropos-user-option)
  ("e" apropos-value))
(bind-key "C-h h" #'hydra-apropos/body)

(defhydra hydra-goto-line (:pre (progn
                                  (global-git-gutter-mode -1)
                                  (linum-mode 1))
                           :post (progn
                                   (linum-mode -1)
                                   (global-git-gutter-mode +1))
                           :color blue)
  "goto"
  ("g" goto-line "line")
  ("c" goto-char "char")
  ("q" nil "quit"))
(bind-key "M-g" #'hydra-goto-line/body)

(defhydra hydra-open-c-mode (:color blue)
  "open"
  ("o" ff-find-other-file "other")
  ("e" ar/platform-open-in-external-app "externally")
  ("u" ar/open-file-at-point "url at point")
  ("b" ar/file-open-build-file "build file")
  ("q" nil "cancel"))

(defhydra hydra-open (:color blue)
  "
Open: _p_oint _e_xternally
      _u_rls
"
  ("e" ar/platform-open-in-external-app nil)
  ("p" ar/open-file-at-point nil)
  ("u" ar/helm-buffer-urls nil)
  ("q" nil "cancel"))

(defhydra hydra-open-prog-mode (:color blue)
  "open"
  ("o" ff-find-other-file "other")
  ("e" ar/platform-open-in-external-app "externally")
  ("u" ar/open-file-at-point "url at point")
  ("b" ar/file-open-build-file "build file")
  ("q" nil "cancel"))

(defun ar/hydra-open-dwim ()
  "Choose \"open\" hydra based on current mode."
  (interactive)
  (cond ((derived-mode-p 'c-mode) (hydra-open-c-mode/body))
        ((derived-mode-p 'prog-mode) (hydra-open-prog-mode/body))
        (t (hydra-open/body))))

(bind-key "C-c o" #'ar/hydra-open-dwim)

(defhydra hydra-search (:color blue)
  "search"
  ("d" helm-do-ag "search directory")
  ("r" ar/projectile-helm-ag "search repository")
  ("f" ar/find-dired-current-dir "find file")
  ("a" ar/find-all-dired-current-dir "find all files")
  ("q" nil "quit"))
(bind-key "C-c s" #'hydra-search/body)

(defhydra hydra-git-gutter (:pre (git-gutter-mode 1))
  "
Git: _n_ext     _s_tage  _d_iff
     _p_revious _k_ill _q_uit
"
  ("n" git-gutter:next-hunk nil)
  ("p" git-gutter:previous-hunk nil)
  ("s" git-gutter:stage-hunk nil)
  ("k" (lambda ()
         (interactive)
         (git-gutter:revert-hunk)
         (call-interactively #'git-gutter:next-hunk)) nil)
  ("d" git-gutter:popup-hunk nil)
  ("q" nil nil :color blue))
(bind-key "C-c g" #'hydra-git-gutter/body)

(defhydra hydra-quick-insert (:color blue)
  "
Quick insert: _w_eb bookmark _b_acklog bookmark
              _t_odo _d_one
"
  ("w" ar/helm-org-add-bookmark nil)
  ("b" ar/helm-org-add-backlog-link nil)
  ("t" ar/org-add-todo nil)
  ("d" ar/org-add-done nil)
  ("q" nil nil :color blue))
(bind-key "C-c x" #'hydra-quick-insert/body)

(defhydra hydra-sort (:color blue)
  "
Sort: _l_ines _o_rg list
      _b_lock"
  ("l" ar/buffer-sort-lines-ignore-case nil)
  ("o" org-sort-list nil)
  ("b" ar/buffer-sort-current-block nil)
  ("q" nil nil :color blue))
;; Not great. Conflicts with company search.
;; (bind-key "M-s" #'hydra-sort/body)

(defhydra hydra-jingle (:color red)
  "jingle"
  ("SPC" jingle-play-stop-song "play/stop")
  ("n" jingle-next-song "next")
  ("p" jingle-previous-song "previous")
  ("s" jingle-search-songs "search")
  ("i" jingle-display-current-song-info "song info")
  ("r" jingle-toggle-random-playback "random")
  ("q" nil "quit" :color blue))
(global-set-key (kbd "C-c m") #'hydra-jingle/body)

(use-package git-commit-mode
  :commands (git-commit-commit)
  :config
  (bind-key "C-c x" #'hydra-git-commit/body git-commit-mode-map))

(defun ar/org-insert-youtube-video ()
  "Insert a youtube video to current org file."
  (interactive)
  (insert (format "[[youtube:%s][%s]]"
                  (ar/alpha-numeric-clipboard-or-prompt "youtube video id")
                  (read-string "description: "))))

;; From http://oremacs.com/2015/03/07/hydra-org-templates
(defun ar/hot-expand (str)
  "Expand org template STR."
  (insert str)
  (org-try-structure-completion))

(defhydra hydra-org-template (:color blue :hint nil)
  "
_c_enter  _q_uote    _L_aTeX:
_l_atex   _e_xample  _i_ndex:
_a_scii   _v_erse    _I_NCLUDE:
_s_rc     ^ ^        _H_TML:
_h_tml    ^ ^        _A_SCII:
_y_outube
"
  ("s" (ar/hot-expand "<s"))
  ("e" (ar/hot-expand "<e"))
  ("q" (ar/hot-expand "<q"))
  ("v" (ar/hot-expand "<v"))
  ("c" (ar/hot-expand "<c"))
  ("l" (ar/hot-expand "<l"))
  ("h" (ar/hot-expand "<h"))
  ("a" (ar/hot-expand "<a"))
  ("L" (ar/hot-expand "<L"))
  ("i" (ar/hot-expand "<i"))
  ("I" (ar/hot-expand "<I"))
  ("H" (ar/hot-expand "<H"))
  ("A" (ar/hot-expand "<A"))
  ("y" (ar/org-insert-youtube-video))
  ("<" self-insert-command "ins")
  ("o" nil "quit"))

;; Display hydra-org-template if < inserted at BOL.
(define-key org-mode-map "<"
  (lambda () (interactive)
    (if (looking-back "^")
        (hydra-org-template/body)
      (self-insert-command 1))))

(use-package smerge-mode)
(defhydra hydra-smerge (:color amaranth)
  "git smerge"
  ("n" smerge-next "next")
  ("p" smerge-prev "previous")
  ("m" (lambda ()
         (interactive)
         (smerge-keep-mine)
         (smerge-next)) "keep mine")
  ("o" (lambda ()
         (interactive)
         (smerge-keep-other)
         (smerge-next)) "keep other")
  ("b" (lambda ()
         (interactive)
         (smerge-keep-base)
         (smerge-next)) "keep base")
  ("a" (lambda ()
         (interactive)
         (smerge-keep-all)
         (smerge-next)) "keep all")
  ("q" nil "quit"))

(defun ar/smerge-mode-hook-function ()
  "Called when entering smerge mode."
  (hydra-smerge/body))
(add-hook 'smerge-mode-hook #'ar/smerge-mode-hook-function)

(defun ar/try-smerge ()
  "Activate smerge on conflicts."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))
(add-hook 'find-file-hook #'ar/try-smerge t)

(use-package cl
  :init
  ;; Ignore running processes when closing Emacs
  ;; From http://oremacs.com/2015/01/04/dired-nohup
  (defadvice save-buffers-kill-emacs
      (around no-query-kill-emacs activate)
    "Prevent \"Active processes exist\" query on exit."
    (flet ((process-list ())) ad-do-it))
  :commands (flet))

(use-package define-word :ensure t
  :commands (define-word-at-point define-word))

(use-package profiler)
(defun ar/profiler-start-cpu ()
  "Start cpu profiler."
  (interactive)
  (profiler-start 'cpu))

(defhydra hydra-profile (:color blue)
  "profiling"
  ("b" ar/profiler-start-cpu "begin")
  ("r" profiler-report "report")
  ("e" profiler-stop "end")
  ("q" nil "quit"))
(bind-key "C-c 1" #'hydra-profile/body)

;; (global-set-key
;;  (kbd "C-c y")
;;  (defhydra hydra-root (:color blue)
;;    "cheatsheet"
;;    ("C-c s" hydra-search/body "search")
;;    ("q" nil "quit")))

;; Override default flycheck triggers
(setq flycheck-check-syntax-automatically
      '(save idle-change mode-enabled)
      flycheck-idle-change-delay 0.8)

(add-hook 'after-init-hook #'global-flycheck-mode)

;; Handle youtube org links in the form of [[youtube:XjKtkEMUYGc][Some description]]
;; Based on http://endlessparentheses.com/embedding-youtube-videos-with-org-mode-links.html
(org-add-link-type
 "youtube"
 (lambda (handle)
   (browse-url (concat "https://www.youtube.com/watch?v=" handle)))
 (lambda (path desc backend)
   (cl-case backend
     (html (format
            "<iframe width='420'
                     height='315'
                     src='https://www.youtube.com/embed/%s'
                     frameborder='0'
                     allowfullscreen>%s
             </iframe>"
            path (or desc "")))
     (latex (format "\href{%s}{%s}" path (or desc "video"))))))

(use-package org
  :ensure t
  :defer t
  :init
  (setq org-todo-keywords
        '((sequence
           "TODO"
           "DONE"
           "OBSOLETE"
           "CANCELLED")))
  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "red" :weight bold))
          ("DONE" . (:foreground "green" :weight bold))
          ("OBSOLETE" . (:foreground "blue" :weight bold))
          ("CANCELLED" . (:foreground "gray" :weight bold))))
  (setq org-refile-targets '((nil :regexp . "Week of")))
  (setq org-ellipsis "⤵")
  (setq org-fontify-emphasized-text +1)
  ;; Fontify code in code blocks.
  (setq org-src-fontify-natively t)
  ;; When exporting anything, do not insert in kill ring.
  (setq org-export-copy-to-kill-ring nil)
  ;; Display images inline when running in GUI.
  (setq org-startup-with-inline-images (display-graphic-p))
  (setq org-src-tab-acts-natively t)
  ;; Prevent inadvertently editing invisible areas in Org.
  (setq org-catch-invisible-edits 'error)
  (setq org-image-actual-width t)
  (setq org-hide-emphasis-markers t)
  ;; All Org leading stars become invisible.
  (setq org-hide-leading-stars t)
  ;; Skip Org's odd indentation levels (1, 3, ...).
  (setq org-odd-levels-only t)
  ;; Disable auto isearch within org-goto.
  (setq org-goto-auto-isearch nil)
  ;; Enable RET to follow Org links.
  (setq org-return-follows-link t))

;; Required by code block syntax highlighting.
(use-package htmlize :ensure t)

(ignore-errors (use-package org-beautify-theme :ensure t))

(use-package org-bullets :ensure t
  :config
  (setq org-bullets-bullet-list
        '("◉" "◎" "⚫" "○" "►" "◇")))

;; From http://emacsredux.com/blog/2015/01/18/clear-comint-buffers/
(defun ar/comint-clear-buffer ()
  "Clear the content of shell/REPL."
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

;; Wonderful weather forecast.
(use-package sunshine :ensure t)
(when (window-system)
  (setq sunshine-show-icons t))
(setq sunshine-units 'metric)
(setq sunshine-location "London, GB")

;; From https://github.com/daschwa/emacs.d
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single
line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2)))))

;; From https://github.com/daschwa/emacs.d
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single
line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-beginning-position 2)))))

;; From http://www.reddit.com/r/emacs/comments/2amn1v/isearch_selected_text
(defadvice isearch-mode (around isearch-mode-default-string (forward &optional regexp op-fun recursive-edit word-p) activate)
  "Enable isearch to start with current selection."
  (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
      (progn
        (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
        (deactivate-mark)
        ad-do-it
        (if (not forward)
            (isearch-repeat-backward)
          (goto-char (mark))
          (isearch-repeat-forward)))
    ad-do-it))

;; Open gyp files in prog-mode.
(add-to-list 'auto-mode-alist '("\\.gyp\\'" . prog-mode))

;; For plantuml see https://zhangweize.wordpress.com/2010/09/20/update-plantuml-mode
;; (use-package  puml-mode :ensure t)

(defun ar/update-blog-timestamp-at-point ()
  "Update blog entry timestamp at point."
  (interactive)
  (ar/org-update-drawer "MODIFIED"
                        (format-time-string "[%Y-%m-%d %a]")))

(defun ar/org-confirm-babel-evaluate (lang body)
  "Do not confirm org babel evaluation for LANG and BODY."
  (and
   (not (string= lang "emacs-lisp"))
   (not (string= lang "plantuml"))
   (not (string= lang "python"))))

(use-package org-src)

(use-package ob-plantuml
  :after org-src
  :config
  ;; Use fundamental mode when editing plantuml blocks with C-c '
  (add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))
  (setq org-confirm-babel-evaluate 'ar/org-confirm-babel-evaluate)
  (cond ((ar/osx-p)
         (setq org-plantuml-jar-path "~/homebrew/Cellar/plantuml/8018/plantuml.8018.jar")
         (setenv "GRAPHVIZ_DOT" (expand-file-name "~/homebrew/bin/dot")))
        (t
         (message "Warning: Could not find plantuml.8018.jar")
         (message "Warning: Could not find $GRAPHVIZ_DOT location"))))

;; Avoid native dialogs when running graphical.
(when (boundp 'use-dialog-box)
  (setq use-dialog-box nil))

(use-package server
  :defer 2
  :config
  (unless (server-running-p)
    (server-start)))

(ar/load-all-files "~/.emacs.d/local/*.el")

(provide 'init)
;;; init.el ends here
