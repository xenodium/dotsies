;;; init.el --- Emacs initialization entry point.
;;; Commentary:
;; Just another init.el file.
;;; Code:

;; Hide UI (early on).
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'toggle-scroll-bar) (toggle-scroll-bar -1))

;; https://oremacs.com/2015/01/17/setting-up-ediff
;; Macro for setting custom variables.
;; Similar to custom-set-variables, but more like setq.
(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))

;; Get rid of splash screens.
;; From http://www.emacswiki.org/emacs/EmacsNiftyTricks
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)

;; No Alarms.
(setq ring-bell-function 'ignore)

;;; Transparent titlebar
;; https://github.com/d12frosted/homebrew-emacs-plus/blob/master/Formula/emacs-plus.rb#L98
;; https://github.com/d12frosted/homebrew-emacs-plus/issues/55
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Properties-in-Mode.html#Properties-in-Mode
(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)) ; nil for dark text
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

;; Guarantee that Emacs never loads outdated byte code files.
(setq load-prefer-newer t)

(defun ar/byte-compile-elpa-dir ()
  "Byte-recompile all elpa packages."
  (interactive)
  (byte-recompile-directory (concat user-emacs-directory "elpa") 0))

;; Put off GC until 10MB of allocation or 5s of idle time.
(setq gc-cons-threshold (* 10 1024 1024))
(setq gc-cons-percentage 0.2)
(run-with-idle-timer 5 t #'garbage-collect)
(setq garbage-collection-messages t)

(setq auto-window-vscroll nil)

;; From https://github.com/daschwa/emacs.d
;; Nic says eval-expression-print-level needs to be set to nil (turned off) so
;; that you can always see what's happening.
(setq eval-expression-print-level nil)

;; Ask shell for PATH, MANPATH, and exec-path and update Emacs environment.
;; We do this early on as we assert binaries are installed throughout
;; init.
(load-file (expand-file-name "~/.emacs.d/downloads/exec-path-from-shell/exec-path-from-shell.el"))
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(defun ar/append-exec-path ()
  "Browse to a directory and add its path to `exec-path' and $PATH."
  (interactive)
  (let ((path (read-directory-name "Add to PATH: ")))
    (setenv "PATH" (concat (getenv "PATH") ":" path))
    (print (getenv "PATH"))
    (setq exec-path
          (append (list path) exec-path))
    (print exec-path)))

;; Additional load paths.
(add-to-list 'load-path "~/.emacs.d/ar")
(add-to-list 'load-path "~/.emacs.d/external")

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

;; I've inadvertedly exited Emacs far too many times.
;; Ask for confirmation.
(setq confirm-kill-emacs 'yes-or-no-p)

;; No need to keep duplicates in prompt history.
(setq history-delete-duplicates t)

(setq user-full-name "Álvaro Ramírez"
      user-mail-address "")

(require 'ar-package)
(ar/package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-enable-imenu-support t)
(require 'use-package)

(use-package validate :ensure t)

;; Ensure window is maximized after window setup.
(use-package maxframe :ensure t
  :hook (window-setup . maximize-frame))

;; flet is no longer available. Use noflet as a replacement.
(use-package noflet :ensure t)

(use-package async :ensure t
  :config
  (dired-async-mode +1)
  (async-bytecomp-package-mode +1))

(use-package beginend :ensure t
  :defer t
  :config
  (beginend-global-mode))

;; From https://www.reddit.com/r/emacs/comments/8qkkh9/poll_theme_activation_on_loading/e0k7j4v
(defun ar/load-theme (&rest args)
  "Like `load-theme', but disables all themes before loading the new one."
  ;; The `interactive' magic is for creating a future-proof passthrough.
  (interactive (advice-eval-interactive-spec
                (cadr (interactive-form #'load-theme))))
  (mapcar #'disable-theme custom-enabled-themes)
  (apply (if (called-interactively-p 'any) #'funcall-interactively #'funcall)
         #'load-theme args))

(use-package base16-theme :ensure t
  :config
  (load-theme 'base16-atelier-heath t)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#695d69")))

;; (use-package danneskjold-theme :ensure t)
;; (use-package molokai-theme :ensure t)

(defun ar/set-font (font-name)
  "Set font with FONT-NAME."
  (if (member font-name (font-family-list))
      (set-face-attribute 'default nil :font font-name)
    (message "Warning: '%s' font not found" font-name)))

(defun ar/pick-font ()
  (interactive)
  (ar/set-font (completing-read "Select font:"
                                (font-family-list))))

;; Always use a box cursor.
(setq-default cursor-type 'box)

;; Get from https://github.com/adobe-fonts/source-code-pro
(ar/set-font "Source Code Pro")

;; Additional theme overrides
;; Set default cursor color.
(add-to-list 'default-frame-alist
             '(cursor-color . "#FA009A"))
;; Set font face height. Value is 1/10pt.
(set-face-attribute 'default nil
                    :height 180
                    :foreground "#dcdcdc")
(set-face-attribute 'fringe nil
                    :background nil)
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
                    :foreground "default")

;; Find errors in init.el by bisecting the file.
(use-package bug-hunter :ensure t
  :commands (bug-hunter-init-file))

;; Restart Emacs from Emacs.
(use-package restart-emacs :ensure t
  :commands (restart-emacs))

(use-package esup :ensure t
  :commands (esup))

;; Handles escaping regexes from input. For example: no need for \(\)
(use-package pcre2el :ensure t
  :config
  (pcre-mode +1))

(use-package helm
  ;; Save current position to mark ring when jumping to a different place
  :hook  (helm-goto-line-before . helm-save-current-pos-to-mark-ring)
  :ensure t
  :config
  (use-package helm-utils)
  (use-package helm-elisp)
  ;; Helm now defaults to 'helm-display-buffer-in-own-frame. Override this behavior.
  (validate-setq helm-show-completion-display-function #'helm-default-display-buffer)
  (validate-setq helm-scroll-amount 4) ; scroll 4 lines other window using M-<next>/M-<prior>
  (validate-setq helm-input-idle-delay 0.01) ; be idle for this many seconds, before updating candidate buffer
  (validate-setq helm-split-window-default-side 'below) ;; open helm buffer below.
  (validate-setq helm-split-window-in-side-p t)
  (validate-setq helm-candidate-number-limit 200)

  (use-package helm-net
    :defer t
    :config
    (validate-setq helm-net-prefer-curl t))

  (use-package helm-imenu
    :defer t
    :config
    (use-package imenu
      :config
      ;; Automatically rescan for imenu changes.
      (set-default 'imenu-auto-rescan t))
    (use-package imenu-anywhere :ensure t))

  ;; Switch major modes and toggle minor modes.
  (use-package helm-source
    :defer t)

  (use-package helm-mode-manager :ensure t
    :defer t)

  (use-package helm-ag :ensure t
    :defer t
    :config
    (defun ar/helm-ag-insert (arg)
      ;; Helm-ag and insert match.
      (interactive "P")
      (defun ar/insert-candidate (candidate)
        (move-beginning-of-line 1)
        (unless (eolp)
          (kill-line))
        ;; Drop file:line:column. For example:
        ;; arc_hostlink.c:13:2:#include <linux/fs.h>
        ;; => #include <linux/fs.h>
        (insert (replace-regexp-in-string "^[^ ]*:" "" candidate))
        (indent-for-tab-command))
      (let ((helm-source-do-ag (helm-build-async-source "Silver Searcher inserter"
                                 :init 'helm-ag--do-ag-set-command
                                 :candidates-process 'helm-ag--do-ag-candidate-process
                                 :action 'ar/insert-candidate
                                 :nohighlight t
                                 :requires-pattern 3
                                 :candidate-number-limit 9999
                                 :keymap helm-do-ag-map)))
        (call-interactively #'ar/helm-ag)))
    (cond ((executable-find "rg")
           (validate-setq helm-ag-base-command "rg --vimgrep --no-heading --ignore-case"))
          ((executable-find "pt")
           (validate-setq helm-ag-base-command "pt -e --nocolor --nogroup"))
          ((executable-find "ag")
           (validate-setq helm-ag-base-command "ag --nocolor --nogroup"))
          (t
           (validate-setq helm-ag-base-command "ack --nocolor --nogroup"))))

  (use-package helm-buffers
    :config
    (validate-setq helm-buffer-max-length 40)
    (use-package ido)
    (validate-setq ido-use-virtual-buffers t)
    ;; Remote checking is slow. Disable.
    (validate-setq helm-buffer-skip-remote-checking t)
    (validate-setq helm-buffers-fuzzy-matching t))

  (use-package helm-files
    :config
    (validate-setq helm-ff-file-name-history-use-recentf t)
    (validate-setq helm-ff-search-library-in-sexp t)
    (validate-setq helm-ff-skip-boring-files t)
    (mapc (lambda (regexp)
            (add-to-list 'helm-boring-file-regexp-list
                         regexp))
          '("\\.DS_Store$" "\\._darcss$" "\\.la$" "\\.o$" "\\.i$")))

  (use-package helm-grep
    :bind (:map helm-grep-mode-map
                ("<return>" . helm-grep-mode-jump-other-window)
                ("n" . helm-grep-mode-jump-other-window-forward)
                ("p" . helm-grep-mode-jump-other-window-backward)))

  (use-package helm-org)

  (use-package helm-swoop :ensure t
    :config
    ;; Patching helm-swoop-pattern and helm-swoop-split-window-function until fixed upstream.
    ;; See https://github.com/ShingoFukuyama/helm-swoop/pull/125
    (setq helm-swoop-pattern "")
    (validate-setq helm-swoop-split-window-function
                   (lambda ($buf &optional resume)
                     (if helm-swoop-split-with-multiple-windows
                         (funcall helm-swoop-split-direction)
                       (when (one-window-p)
                         (funcall helm-swoop-split-direction)))
                     (other-window 1)
                     (switch-to-buffer $buf)))
    :bind (("M-C-s" . helm-multi-swoop-all)
           ("M-i" . helm-swoop))
    :commands (helm-swoop))

  (use-package helm-config)

  (use-package helm-eshell
    :init
    (defun helm-eshell-mode-hook-func ()
      (bind-key "M-r" #'helm-eshell-history eshell-mode-map))
    :hook (eshell-mode . helm-eshell-mode-hook-func)
    :after helm-files)

  (defun ar/helm-keyboard-quit-dwim (&optional arg)
    "First time clear miniuffer. Quit thereafter."
    (interactive "P")
    (if (> (length (minibuffer-contents)) 0)
        (call-interactively 'helm-delete-minibuffer-contents)
      (helm-keyboard-quit)))

  (helm-mode +1)

  (use-package helm-c-yasnippet
    :defer t
    :ensure t)

  (use-package helm-make :ensure t
    :defer t)

  (use-package helm-company :ensure t
    :defer t)

  ;; Best way (so far) to search for files in repo.
  (use-package helm-projectile :ensure t
    :bind ("C-x f" . helm-projectile))

  (use-package helm-gtags
    :defer t
    :ensure t
    :config
    (helm-gtags-mode +1))

  (use-package helm-dash :ensure t
    :commands (helm-dash)
    :config
    ;; View documentation in external browser.
    ;; (validate-setq helm-dash-browser-func #'browse-url)
    ;; View documentation in ewww.
    (validate-setq helm-dash-browser-func #'eww))

  (use-package helm-pydoc :ensure t
    :commands (helm-pydoc))

  (use-package helm-describe-modes :ensure t
    :commands (helm-describe-modes))

  (use-package helm-tramp :ensure t
    :commands (helm-tramp))

  (use-package helm-descbinds :ensure
    :bind (("C-h b" . helm-descbinds)
           ("C-h w" . helm-descbinds)))

  (use-package helm-xcdoc :ensure t
    :defer t)

  :bind (("C-x C-f" . helm-find-files)
         ("C-c i" . helm-semantic-or-imenu)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-h a" . helm-apropos)
         ("C-h y" . helm-dash-at-point)
         :map helm-map
         ("C-i" . helm-execute-persistent-action) ; make TAB works in terminal
         ("C-z" . helm-select-action) ; list actions using C-z
         ("M-p" . helm-previous-source)
         ("M-n" . helm-next-source)
         ("C-g" . ar/helm-keyboard-quit-dwim)))

;; From https://gitlab.com/to1ne/temacco/commit/eb2ba7fe4d03c7c9540c595b213a18ba950b3b20
;; "brew install sqlparse" gives you sqlformat.
(defun ar/format-sql ()
  "Format the SQL in region using the sqlformat tool.
  If no region is active, the whole file is formatted."
  (interactive)
  (let ((start (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max))))
    (shell-command-on-region start end "sqlformat -r -" nil t)))

;; make ELisp regular expressions more readable.
(use-package easy-escape :ensure t
  :hook (emacs-lisp-mode . easy-escape-minor-mode)
  :config
  ;; TODO: Figure out why face foreground isn't displayed.
  (set-face-attribute 'easy-escape-face nil :foreground "red")
  (validate-setq easy-escape-character ?⑊))

;; Logs commands in a separate buffer. Handy for screenscasts.
(use-package command-log-mode :ensure t)

;; Disabling while trying out smartparens.
;; ;; Automatically closes brackets.
;; (electric-pair-mode)
;; ;; Additional electric pairs.
;; (validate-setq electric-pair-pairs '((?\{ . ?\})
;;                             (?\< . ?\>)))

(use-package smartparens :ensure t
  ;; Add to minibuffer also.
  :hook (minibuffer-setup . smartparens-mode)
  :config
  (require 'smartparens-config)
  (require 'smartparens-html)
  (require 'smartparens-python)
  (smartparens-global-strict-mode +1)
  ;; I prefer keeping C-w to DWIM kill, provided by
  ;; `ar/kill-region-advice-fun'. Removing remap.
  (define-key smartparens-strict-mode-map [remap kill-region] nil)

  (defun ar/smartparens-wrap-square-bracket (arg)
    "[] equivalent of `paredit-wrap-round'."
    (interactive "P")
    (save-excursion
      (unless (sp-point-in-symbol)
        (backward-char))
      (sp-wrap-with-pair "["))
    (insert " "))
  (define-key smartparens-mode-map (kbd "M-]") #'ar/smartparens-wrap-square-bracket)
  :bind
  (:map smartparens-strict-mode-map
        ("C-c <right>" . sp-forward-slurp-sexp)
        ("C-c <left>" . sp-forward-barf-sexp)
        ("M-[" . sp-rewrap-sexp)))

(defun ar/sp-backward-delete-char-advice-fun (orig-fun &rest r)
  "Play nice with `hungry-delete-backward' in ORIG-FUN and R."
  (if (and (looking-back "[[:space:]\n]\\{2,\\}" (- (point) 2))
           (boundp 'hungry-delete-mode)
           hungry-delete-mode)
      (call-interactively 'hungry-delete-backward)
    (apply orig-fun r)))

(advice-add 'sp-backward-delete-char
            :around
            'ar/sp-backward-delete-char-advice-fun)

(defun ar/sp-delete-char-advice-fun (orig-fun &rest r)
  "Play nice with `hungry-delete-forward' in ORIG-FUN and R."
  (if (and (looking-at "[[:space:]\n]\\{2,\\}")
           (boundp 'hungry-delete-mode)
           hungry-delete-mode)
      (call-interactively 'hungry-delete-forward)
    (apply orig-fun r)))

(advice-add 'sp-delete-char
            :around
            'ar/sp-delete-char-advice-fun)

(use-package dabbrev
  :config
  ;; Case-sensitive fold search search (ie. M-/ to autocomplete).
  (validate-setq dabbrev-case-fold-search nil))

(use-package ar-auto-correct)

(use-package color-picker
  :commands color-picker)

;; Unused. Disabling for now.
;; (use-package scimax-string)

(use-package ar-assert)
(use-package ar-string)
(use-package ar-buffer)
(use-package ar-dired
  :after f)
(use-package ar-file)
(use-package ar-bazel
  :after s)
(use-package ar-alist)

(use-package ar-frame
  :commands ar/frame-set-current-frame-alpha-channel)

(use-package ar-git)
(use-package ar-helm
  :after helm)

(use-package ar-helm-objc
  :after helm
  :commands (ar/helm-objc-import-update))

(use-package ar-helm-projectile
  :bind ("<f7>" . ar/helm-projectile-shell-cd))

(use-package ar-helm-org
  :after (helm helm-org org org-cliplink))

(use-package ar-helm-shell
  :bind (:map shell-mode-map
              ("M-r" . ar/helm-shell-search-history)))

(use-package ar-helm-url
  :after helm)

(use-package ar-helm-hotspots-config
  :demand ;; There are files in ar/load-all-files needing ar-helm-hotspots-config.
  :after (f helm-buffers)
  :bind (("C-x C-b" . ar/helm-hotspots)
         ("C-x b" . ar/helm-hotspots)))

(use-package ar-image
  :commands (ar/image-open-html-for-current-dir))

(use-package ar-imagemagick)

(use-package ar-ios-sim
  :after f dash)

(use-package ar-linux)

(use-package ar-objc
  :commands (ar/objc-import
             ar/objc-include))

(use-package ar-process)

(use-package ar-org
  :after org)

(use-package ar-hammerspoon-org-modal
  :after ar-org)

(use-package ar-org-blog
  :commands (ar/org-blog-insert-image
             ar/org-blog-insert-resized-image))

(use-package company-hammerspoon
  :after company)

(use-package ar-shell)
(use-package ar-sudo)

(use-package ar-url
  :commands (ar/url-view-links-at)
  :config
  (use-package enlive :ensure t))

(use-package ar-osx
  :commands ar/osx-convert-plist-to-xml)

(use-package ar-platform
  :demand
  :bind (("C-x t" . ar/platform-new-browser-tab)))

(use-package ar-ox-html
  :after (org ox-html)
  :config
  (ar/ox-html-setup)
  :bind (:map org-mode-map
              ([f6] . ar/ox-html-export)))

(use-package ar-text
  :bind (("C-c c" . ar/text-capitalize-word-toggle)
         ("C-c r" . set-rectangular-region-anchor)))

(use-package ar-magit
  :after magit)

(use-package ar-typescript)
(use-package ar-font)
(use-package ar-compile)

(use-package company-swimports
  :after company)

(use-package company-escaped-files
  :after s)

(use-package company-grep
  :after s)

(use-package company-rfiles
  :after company)

(use-package company-bash-history
  :after company)

(use-package company-projectile-cd
  :after company)

(use-package flycheck-swiftlint
  :after flycheck)

(use-package modal-ivy :after ivy)

;; Easy access to links in buffer (using avy).
(use-package link-hint :ensure t
  :commands (link-hint-open-link))

(use-package bazel-mode
  :mode ("BUILD\\'" . bazel-mode)
  :init
  (defun ar/bazel-mode-hook-fun ()
    (ar/buffer-run-for-saved-file-name "buildifier" "BUILD")
    (setq-local company-backends '(company-bazel company-rfiles)))
  :after company-grep
  :hook (bazel-mode . ar/bazel-mode-hook-fun))

(use-package last-change-jump
  :defer t ;; We want this global mode started soon after init.
  :config
  (global-last-change-jump-mode +1)
  :bind ("C-c `" . last-change-jump))

;; underscore -> UPCASE -> CamelCase conversion of names
(use-package string-inflection :ensure t)

;; Migation Marker
(use-package interaction-log :ensure t
  :defer t
  :commands ar/interation-log-show
  :config
  (interaction-log-mode +1)
  (defun ar/interation-log-show()
    (interactive)
    (display-buffer ilog-buffer-name)))

(use-package abbrev
  :config
  (validate-setq abbrev-file-name "~/stuff/active/code/dots/emacs/abbrev_defs")
  (validate-setq save-abbrevs 'silently)
  (setq-default abbrev-mode t))

(use-package nyan-mode :ensure t
  :if (display-graphic-p)
  :config
  (nyan-mode +1))

;; Disabling. Not used.
;; (use-package fontawesome :ensure t
;;   :after ar-font
;;   :defer 10
;;   :config
;;   (ar/font-assert-installed "FontAwesome" "Install ttf from http://fontawesome.io."))

;; Disabling for now.
;; (use-package all-the-icons :ensure t
;;   :config
;;   (ar/font-assert-installed "dev-icons" "Install ttf from https://github.com/domtronn/all-the-icons.el/tree/master/fonts")
;;   (ar/font-assert-installed "file-icons" "Install ttf from https://github.com/domtronn/all-the-icons.el/tree/master/fonts")
;;   (ar/font-assert-installed "FontAwesome" "Install ttf from http://fontawesome.io.")
;;   (ar/font-assert-installed "octicons" "Install ttf from https://octicons.github.com.")
;;   (ar/font-assert-installed "Weather Icons" "Install ttf from https://erikflowers.github.io/weather-icons.")
;;   (ar/font-assert-installed "font-mfizz" "Install ttf from https://github.com/fizzed/font-mfizz/blob/master/dist.")
;;   (ar/font-assert-installed "icomoon" "Install ttf from https://github.com/vorillaz/devicons/tree/master/fonts."))

(defun ar/mode-icons-supported-p-advice-fun (orig-fun &rest r)
  "`mode-icons-supported-p' is expensive. Cache it."
  (if (boundp 'ar/mode-icons-supported-p)
      ar/mode-icons-supported-p
    (defvar ar/mode-icons-supported-p (apply orig-fun r))
    ar/mode-icons-supported-p))

;; Yay mode icons!
;; Disabling. It's slow.
;; (use-package mode-icons
;;   :ensure t
;;   :config
;;   (advice-add 'mode-icons-supported-p
;;               :around
;;               'ar/mode-icons-supported-p-advice-fun)
;;   (when (window-system)
;;     (mode-icons-mode +1)))

(use-package tramp
  :config
  ;; Problem with TRAMP mode
  ;; Control Path too long error
  ;; TMPDIR variable is really large
  ;; http://lists.macosforge.org/pipermail/macports-tickets/2011-June/084295.html
  (validate-setq tramp-verbose 10)
  (setenv "TMPDIR" "/tmp")
  (validate-setq tramp-default-method "ssh")
  (defalias 'ar/exit-tramp 'tramp-cleanup-all-buffers))

(when (display-graphic-p)
  ;; Enable if you'd like to start as fullscreen.
  ;; (set-frame-parameter nil 'fullscreen 'fullboth)
  (validate-setq frame-title-format '("Ⓔ ⓜ ⓐ ⓒ ⓢ")))

;; Tip of the day.
(use-package totd :ensure t
  :commands (totd)
  :config
  (totd-start))

(use-package speed-type :ensure t
  :commands (speed-type-text
             speed-type-region
             speed-type-buffer))

(use-package restclient :ensure t
  :commands restclient-mode)

;; Display chars/lines or row/columns in the region.
(use-package region-state :ensure t
  :defer 10
  :config (region-state-mode))

;; Safely delete packages.
(use-package package-safe-delete :ensure t
  :commands package-safe-delete)

;; Formats python buffer with yapf
;; Install with: pip install git+https://github.com/google/yapf.git
(use-package py-yapf :ensure t
  :hook (python-mode . py-yapf-enable-on-save)
  :config
  (validate-setq py-yapf-options '("--style={based_on_style: google, indent_width: 2}")))

(use-package sicp :ensure t)

(defun ar/format-info-mode ()
  "Opening .info files does not automatically set things up. Give it a little help."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (kill-buffer (current-buffer))
    (info file-name)))

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

(defun ar/dired-mark-all ()
  (interactive)
  (dired-mark-files-regexp ""))

(use-package dired
  :hook (dired-mode . dired-hide-details-mode)
  :commands dired-mode
  :init
  (defun ar/file-find-alternate-parent-dir ()
    "Open parent dir."
    (interactive)
    (find-alternate-file ".."))
  :config
  ;; For dired-jump.
  (use-package dired-x)
  ;; Adding human readable units and sorted by date.
  (validate-setq dired-listing-switches "-Alht")
  ;; Try to guess the target directory for operations.
  (validate-setq dired-dwim-target t)
  ;; Enable since disabled by default.
  (put 'dired-find-alternate-file 'disabled nil)
  ;; Automatically refresh dired buffers when contents changes.
  (validate-setq dired-auto-revert-buffer t)
  :bind (:map global-map
              ("C-l" . dired-jump))
  :bind (:map dired-mode-map
              ("j" . dired-next-line)
              ("k" . dired-previous-line)
              ;; Go to parent directory.
              ("^" . ar/file-find-alternate-parent-dir)
              ("RET" . dired-find-file)
              ("P" . peep-dired)
              ("i" . dired-hide-details-mode)
              ("C-l". dired-jump)
              ("M" . ar/dired-mark-all)))

(use-package dired-aux
  :commands dired-mode
  :config
  ;; Make "Z" shortcut available in dired to extract iOS ipa zips.
  (add-to-list 'dired-compress-file-suffixes '("\\.ipa\\'" "" "unzip -o -d %o %i")))

(use-package peep-dired
  :ensure t
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

(use-package dired-subtree :ensure t
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<backtab>" . dired-subtree-cycle)))

(defun ar/join-previous-sexp ()
  (interactive)
  (save-excursion
    (call-interactively #'backward-sexp)
    (call-interactively #'hungry-delete-backward)))

(bind-key "<C-M-backspace>" #'ar/join-previous-sexp)

(use-package with-editor :ensure t
  :hook ((eshell-mode . with-editor-export-editor)
         (term-exec . with-editor-export-editor)
         (shell-mode . with-editor-export-editor)))

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
    :config
    (add-to-list 'magit-no-confirm 'stage-all-changes)
    (fullframe magit-status magit-mode-quit-window))

  (use-package rotate :ensure t)

  ;; A screensaver of sorts
  (use-package zone
    :config
    (zone-when-idle 120)
    ;; (validate-setq zone-programs
    ;;                [zone-pgm-whack-chars
    ;;                 zone-pgm-rotate
    ;;                 zone-pgm-drip
    ;;                 zone-pgm-martini-swan-dive])
    (validate-setq zone-programs []))

  (use-package zone-words
    :config
    (validate-setq zone-programs (vconcat [zone-words] zone-programs)))

  ;; ;; Locomotives zone.
  ;; (use-package zone-sl :ensure t
  ;;   :after zone
  ;;   :config
  ;;   (validate-setq zone-programs (vconcat [zone-pgm-sl] zone-programs)))

  ;; A fireplace? Yeah, I know...
  (use-package fireplace :ensure t)

  ;; (use-package zone-rainbow :ensure t
  ;;   :after zone
  ;;   :config
  ;;   (validate-setq zone-programs (vconcat [zone-rainbow] zone-programs)))

  (use-package zone-select :ensure t)

  ;; A Nyan zone. Well, just because.
  ;; (use-package zone-nyan :ensure t
  ;;   :config
  ;;   (when (window-system)
  ;;     (validate-setq zone-programs (vconcat [zone-nyan] zone-programs))))

  (use-package discover-my-major :ensure t)

  ;; Make Emacs more discoverable (Handy for dired-mode). Trigger with '?'.
  ;; From http://www.masteringemacs.org/article/discoverel-discover-emacs-context-menus
  (use-package discover :ensure t
    :hook (dired-mode . discover-mode)))

(defun ar/enable-graphical-time ()
  "Enable graphical time in modeline."
  (interactive)
  (validate-setq display-time-24hr-format t)
  (validate-setq display-time-day-and-date t)
  (display-time) ; Align the time to right
  (validate-setq global-mode-string (remove 'display-time-string global-mode-string))
  (validate-setq mode-line-end-spaces
                 (list (propertize " " 'display '(space :align-to (- right 17)))
                       'display-time-string)))

(defun ar/open-youtube-url (url)
  "Download and open youtube URL."
  ;; Check for URLs like:
  ;; https://www.youtube.com/watch?v=rzQEIRRJ2T0
  ;; https://youtu.be/rzQEIRRJ2T0
  (assert (string-match-p "^https://\\(www\\.\\)?youtu\\(\\.be\\|be\\.com\\)" url)
          nil "Not a youtube URL: %s" url)
  (message "Downloading: %s" url)
  (async-start
   `(lambda ()
      (shell-command-to-string
       (format "youtube-dl --newline --exec \"open -a VLC {}\" -o \"~/Downloads/%%(title)s.%%(ext)s\" %s" ,url)))
   `(lambda (output)
      (if (string-match-p "ERROR:" output)
          (message "%s" output)
        (message "Opened: %s" ,url)))))

(use-package elfeed :ensure t
  :commands elfeed
  :after centered-cursor-mode
  :config
  (defun ar/elfeed-set-style ()
    ;; Separate elfeed lines for readability.
    (validate-setq line-spacing 25))
  :hook ((elfeed-search-mode . centered-cursor-mode)
         (elfeed-search-mode . ar/elfeed-set-style))
  :config
  (defun ar/elfeed-open-youtube-video ()
    (interactive)
    (let ((link (elfeed-entry-link elfeed-show-entry)))
      (when link
        (ar/open-youtube-url link))))

  (validate-setq elfeed-feeds
                 '(
                   ("http://200ok.ch/atom.xml" blog emacs tech 200ok)
                   ("http://ben-evans.com/benedictevans?format=RSS" blog tech Ben-Evans)
                   ("http://blog.davep.org/feed.xml" blog emacs tech davep)
                   ("http://blog.josephholsten.com/feed.xml" blog hammerspoon tech Libera-Ideoj)
                   ("http://cestlaz.github.io/rss.xml" blog emacs Zamansky)
                   ("http://cmsj.net/feed.xml" blog hammerspoon tech Chris-Jones)
                   ("http://dangrover.com/feed.xml" blog dangrover emacs tech)
                   ("http://emacsredux.com/atom.xml" blog emacs emacs-redux)
                   ("http://feeds.bbci.co.uk/news/uk/rss.xml?edition=uk" news BBCUK)
                   ("http://feeds.bbci.co.uk/news/world/rss.xml?edition=uk" news BBCWorld)
                   ("http://feeds.feedburner.com/japaneseruleof7" blog japan japanese-rule-of-7)
                   ("http://francismurillo.github.io/hacker/feed.xml" blog tech emacs francismurillo)
                   ("http://francismurillo.github.io/watcher/feed.xml" blog anime francismurillo)
                   ("http://irreal.org/blog/?feed=rss2" blog emacs tech Irreal)
                   ("http://kundeveloper.com/feed" blog emacs tech KunDeveloper)
                   ("http://nullprogram.com/feed" blog emacs tech Chris-Wellons)
                   ("http://planet.emacsen.org/atom.xml" blog emacs tech emacsen)
                   ("http://prodissues.com/feeds/all.atom.xml" blog emacs tech  Prodissues)
                   ("http://reddit.com/r/emacs/.rss" social reddit emacs)
                   ("http://rubyronin.com/wp-feed.php" blog japan the-ruby-ronin)
                   ("http://sachachua.com/blog/feed" blog emacs tech sachachua)
                   ("http://sdegutis.com/blog/atom.xml" blog tech StevenDegutis)
                   ("http://tech.memoryimprintstudio.com/feed" blog emacs tech MemoryImprintStudio)
                   ("http://www.brool.com/index.xml" blog emacs Brool)
                   ("http://www.gonsie.com/blorg/feed.xml" blog emacs dev)
                   ("http://www.ict4g.net/adolfo/feed.xml" blog tech dev Adolfo)
                   ("http://www.modernemacs.com/index.xml" blog emacs tech ModernEmacs)
                   ("http://www.sastibe.de/index.xml" blog emacs SebastianSchweer)
                   ("http://www.thisiscolossal.com/feed" blog tech Colossal)
                   ("http://zzamboni.org/index.xml" blog hammerspoon tech Diego-Martín-Zamboni)
                   ("https://babbagefiles.blogspot.com/feeds/posts/default" blog emacs tech)
                   ("https://blog.danielgempesaw.com/rss" blog emacs tech DanielGempesaw)
                   ("https://changelog.complete.org/feed" blog emacs tech JohnGoerzen)
                   ("https://copyninja.info/feeds/all.atom.xml" blog tech dev copyninja)
                   ("https://elephly.net/feed.xml" blog emacs Elephly)
                   ("https://emacsist.github.io/index.xml" blog emacs tech emacsist)
                   ("https://emacsnotes.wordpress.com/feed" blog tech emacs)
                   ("https://feeds.feedburner.com/codinghorror" blog tech Coding-Horror)
                   ("https://ghuntley.com/rss" blog tech ghuntley ghuntley)
                   ("https://hacks.mozilla.org/feed" blog tech Mozilla)
                   ("https://harryrschwartz.com/atom.xml" bloc emacs HarryRSchwartz)
                   ("https://hasanyavuz.ozderya.net/?feed=rss2" blog emacs HasanYavuz)
                   ("https://increment.com/feed.xml" blog dev Increment)
                   ("https://kdecherf.com/feeds/blog.atom.xml" blog tech dev kdecherf-blog)
                   ("https://kdecherf.com/feeds/le-kdecherf.atom.xml" blog tech dev kdecherf)
                   ("https://manuel-uberti.github.io/feed.xml" blog emacs ManuelUberti)
                   ("https://martinralbrecht.wordpress.com/feed" blog emacs tech MartinAlbrecht)
                   ("https://matt.hackinghistory.ca/feed/" blog emacs MattPrice)
                   ("https://medium.com/feed/@mwfogleman" blog tech emacs meditation MichaelFogleman)
                   ("https://news.ycombinator.com/rss" news hackernews tech)
                   ("https://ogbe.net/blog.xml" blog emacs tech DennisOgbe)
                   ("https://piware.de/post/index.xml" blog tech)
                   ("https://sciencebasedmedicine.org/feed" blog medicine ScienceBasedMedicine)
                   ("https://scripter.co/posts/index.xml" blog emacs tech dev)
                   ("https://swiftnews.curated.co/issues.rss" blog swift tech ShiftNewsCurated)
                   ("https://swiftweekly.github.io/feed.xml" blog swift tech SwiftWeekly)
                   ("https://webgefrickel.de/blog/feed" blog tech dev SteffenRademacker)
                   ("https://wincent.com/blog.rss" blog tech dev wincent)
                   ("https://writequit.org/posts.xml" blog tech emacs writequit)
                   ("https://www.bytedude.com/feed.xml" blog emacs MarcinS)
                   ("https://www.hasecke.eu/index.xml" blog emacs tech hasecke)
                   ("https://www.johndcook.com/blog/comments/feed" blog emacs JohnDCook)
                   ("https://www.ogre.com/blog/feed" blog dev Ogre)
                   ("https://ytrss.co/feed/UCkRmQ_G_NbdbCQMpALg6UPg" youtube emacs EmacsRocks)
                   ("https://ytrss.co/feed/UCxkMDXQ5qzYOgXPRnOBrp1w" youtube emacs Zamansky)
                   ("http://akkartik.name/feeds.xml" blog tech KartikAgaram)
                   ))

  (defun ar/elfeed-view-filtered (filter)
    "Filter the elfeed-search buffer to show feeds tagged with FILTER."
    (interactive)
    (elfeed)
    (unwind-protect
        (let ((elfeed-search-filter-active :live))
          (setq elfeed-search-filter filter))
      (elfeed-search-update :force)))

  (defun ar/elfeed-view-emacs ()
    "Filter the elfeed-search buffer to show emacs-tagged feeds."
    (interactive)
    (ar/elfeed-view-filtered "@6-months-ago +unread +emacs"))

  (defun ar/elfeed-view-news ()
    "Filter the elfeed-search buffer to show news-tagged feeds."
    (interactive)
    (ar/elfeed-view-filtered "@6-months-ago +unread +news")))

(use-package elfeed-goodies :ensure t
  :after elfeed
  :config
  (validate-setq elfeed-goodies/entry-pane-position 'bottom)
  (validate-setq elfeed-goodies/tag-column-width 35)
  (elfeed-goodies/setup))

;; Suggests elisp methods based on inputs and outputs.
(use-package suggest :ensure t
  :commands suggest)

;; Semantic code search for emacs lisp.
(use-package elisp-refs :ensure t
  :mode ("\\.el\\'" . emacs-lisp-mode))

(use-package bind-key :ensure t)

;; Enable disabled commands.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(use-package anchored-transpose
  :commands anchored-transpose)

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

(use-package hackernews :ensure t
  :commands hackernews)

;; Stack Exchange viewer.
(use-package sx :ensure t
  :commands sx-search)

(use-package which-key :ensure t
  :config
  (which-key-mode))

;; Twitter.
(use-package twittering-mode :ensure t
  :commands twittering-mode)

(use-package rainbow-delimiters :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package hungry-delete :ensure t
  :config (global-hungry-delete-mode))

(use-package font-core :config
  (global-font-lock-mode))

(use-package jit-lock :config
  :config
  ;; Allow font-lock-mode to do background parsing
  (setq jit-lock-defer-time nil
        ;; jit-lock-stealth-nice 0.1
        jit-lock-stealth-time 1
        jit-lock-stealth-verbose nil))

(use-package autorevert
  :config
  ;; Auto refresh dired.
  ;; From http://mixandgo.com/blog/how-i-ve-convinced-emacs-to-dance-with-ruby
  (csetq global-auto-revert-non-file-buffers t)
  ;; Be quiet about dired refresh.
  (csetq auto-revert-verbose nil)
  (global-auto-revert-mode))

;; Let auto-revert-mode update vc/git info.
;; Need it for mode-line-format to stay up to date.
;; See https://github.com/magit/magit/wiki/magit-update-uncommitted-buffer-hook
;; See https://github.com/magit/magit/blob/master/Documentation/magit.org#the-mode-line-information-isnt-always-up-to-date

(use-package vc-hooks
  :config
  (csetq vc-handled-backends '(Git))
  (csetq auto-revert-check-vc-info nil)
  :bind (:map vc-prefix-map
              ;; Use vc-ediff as default.
              ("=" . vc-ediff)))

(use-package expand-region :ensure t
  :config
  ;; Workaround fixing expand-region:
  ;; https://github.com/magnars/expand-region.el/issues/220
  (validate-setq shift-select-mode nil)
  :bind ("C-c w" . er/expand-region))

;; Visual feedback for query-replace, replace, and multiple cursors.
(use-package visual-regexp :ensure t
  :commands (vr/mc-mark vr/query-replace vr/replace))

(use-package yasnippet :ensure t
  :defer 10
  :config
  (use-package ar-yas)

  (use-package yasnippet-snippets :ensure t)

  (validate-setq yas-indent-line 'fixed)
  (validate-setq yas-snippet-dirs
                 '("~/.emacs.d/yasnippets/personal"))
  (yas-reload-all)
  (use-package ivy :ensure t)

  ;; Display's yasnippet previous inline when cycling through results.
  (use-package ivy-yasnippet :ensure t
    :commands ivy-yasnippet)

  ;; Use aya-create and aya-expand to
  ;; Create a throw-away yasnippet for say:
  ;; This is the ~rhythm of the ~night
  (use-package auto-yasnippet :ensure t
    :commands (aya-create aya-expand)))

;; Back to helm-swoop for now.
;; (use-package swiper :ensure t)
;; (validate-setq swiper-completion-method 'ivy)

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
;; #slow
;; (use-package annoying-arrows-mode :ensure t
;;   :config (global-annoying-arrows-mode))

;; COMEBACK
;; Remember point/place for each file.
(use-package saveplace
  :defer 10
  :config
  (setq-default save-place t)
  (validate-setq save-place-file (expand-file-name ".places"
                                                   user-emacs-directory))
  (save-place-mode))

(defun ar/projectile-helm-ag ()
  "Search current repo/project using ag."
  (interactive)
  (helm-do-ag (projectile-project-root)))

(defun ar/helm-ag (arg)
  "Helm-ag search remembering last location.  With ARG, forget the last location."
  (interactive "P")
  (defvar ar/helm-ag--default-locaction nil)
  (when (or arg (not ar/helm-ag--default-locaction))
    (validate-setq ar/helm-ag--default-locaction
                   (read-directory-name "search in: " default-directory nil t)))
  (helm-do-ag ar/helm-ag--default-locaction))

;; Differentiate C-i key binding from TAB.
(define-key input-decode-map (kbd "C-i") (kbd "H-i"))

(use-package prog-mode
  :config
  (defun ar/yank-line-below ()
    "Yank to line below."
    (interactive)
    (save-excursion
      (move-end-of-line nil)
      (newline)
      (yank))
    (next-line))

  ;; From https://github.com/bbatsov/prelude/blob/a52cdc83eeec567b13a8a5719a174dfe294ee739/core/prelude-core.el#L111
  (defun ar/smart-open-line-above ()
    "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
    (interactive)
    (move-beginning-of-line nil)
    (newline-and-indent)
    (forward-line -1)
    (indent-according-to-mode))

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
  :bind
  (:map prog-mode-map
        ("H-i" . ar/helm-ag-insert)
        ("M-C-y" . ar/yank-line-below)
        ("M-<return>" . ar/smart-open-line-above)
        ("C-<return>" . ar/smart-open-line)))

;; From http://stackoverflow.com/questions/6133799/delete-a-word-without-adding-it-to-the-kill-ring-in-emacs
(defun ar/text-backward-delete-subword (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point)
                 (progn
                   (subword-backward arg)
                   (point))))
(bind-key "M-DEL" #'ar/text-backward-delete-subword)
(bind-key "<C-backspace>" #'ar/text-backward-delete-subword)

(defun ar/duplicate-line ()
  "Duplicate current line and paste below."
  (interactive)
  (let ((line-text (buffer-substring (line-beginning-position)
                                     (line-end-position))))
    (end-of-line)
    (newline)
    (insert line-text)))

(bind-key "C-x C-d" #'ar/duplicate-line)

(bind-key "C-z" #'ar/dired-split-downloads-to-current)

;; Calendar client.
;; Disabling. Not using.
;; (use-package calfw :ensure t)

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

(use-package projectile-sift :ensure t
  :config
  (ar/process-assert-binary-installed "sift" "Install via: \
\"brew install sift\" or download from https://sift-tool.org/download"))

(use-package projectile :ensure t
  :config
  (validate-setq projectile-enable-caching t)
  (projectile-mode))

(use-package ediff
  :init
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
  ;; Automatically highlight first change.
  :hook ((ediff-startup . ediff-next-difference)
         (ediff-before-setup . ar/ediff-bsh)
         (ediff-after-setup-windows . ar/ediff-aswh);
         (ediff-quit . ar/ediff-qh))
  :config
  (csetq ediff-window-setup-function #'ediff-setup-windows-plain)
  (csetq ediff-split-window-function #'split-window-horizontally)

  (use-package outline
    :after outline
    ;; Ensure ediff expands org files.
    :hook (ediff-prepare-buffer . outline-show-all))

  ;; Expand org files when ediffing.
  (add-hook 'ediff-prepare-buffer-hook
            (lambda ()
              (when (eq major-mode 'org-mode)
                (visible-mode +1)  ; default 0
                (setq-local truncate-lines nil)  ; no `org-startup-truncated' in hook
                (setq-local org-hide-leading-stars t))))

  ;; From https://stackoverflow.com/questions/9656311/conflict-resolution-with-emacs-ediff-how-can-i-take-the-changes-of-both-version
  (defun ar/ediff-copy-both-to-C ()
    "Ediff copy A and B to C."
    (interactive)
    (ediff-copy-diff ediff-current-difference nil 'C nil
                     (concat
                      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

  ;; From https://scripter.co/do-ediff-as-i-mean/
  (defun ar/ediff-dwim ()
    "Do ediff as I mean.

If a region is active, call `ediff-regions-wordwise'.
Else if the frame has 2 windows with identical major modes,
  - Do `ediff-files' if the buffers are associated to files and the buffers
    have not been modified.
  - Do `ediff-buffers' otherwise.
Else if the current is a file buffer with a VC backend, call `vc-ediff'
Else call `ediff-buffers'."
    (interactive)
    (let* ((num-win (safe-length (window-list)))
           (bufa (get-buffer (buffer-name)))
           (filea (buffer-file-name bufa))
           (modea (with-current-buffer bufa major-mode))
           bufb fileb modeb)
      (save-excursion
        (other-window 1)
        (setq bufb (get-buffer (buffer-name)))
        (setq fileb (buffer-file-name bufb))
        (setq modeb (with-current-buffer bufb major-mode)))
      (cond
       ;; If a region is selected
       ((region-active-p)
        (call-interactively #'ediff-regions-wordwise))
       ;; Else if 2 windows with same major modes
       ((and (= 2 num-win)
             (eq modea modeb))
        (if ;; If either of the buffers is not associated to a file,
            ;; or if either of the buffers is modified
            (or (null filea)
                (null fileb)
                (buffer-modified-p bufa)
                (buffer-modified-p bufb))
            (progn
              (message "Running (ediff-buffers \"%s\" \"%s\") .." bufa bufb)
              (ediff-buffers bufa bufb))
          (progn
            (message "Running (ediff-files \"%s\" \"%s\") .." filea fileb)
            (ediff-files filea fileb))))
       ;; Else if file in current buffer has a vc backend
       ((and filea
             (vc-registered filea))
        (call-interactively #'vc-ediff))
       ;; Else call `ediff-buffers'
       (t
        (call-interactively #'ediff-buffers))))))

(use-package whitespace
  ;; Automatically remove whitespace on saving.
  :hook ((before-save . whitespace-cleanup)
         (prog-mode . whitespace-mode))
  :config
  ;; When nil, fill-column is used instead.
  (validate-setq whitespace-line-column nil)
  ;; Highlight empty lines, TABs, blanks at beginning/end, lines
  ;; longer than fill-column, and trailing blanks.
  (validate-setq whitespace-style '(face empty tabs lines-tail trailing))
  (set-face-attribute 'whitespace-line nil
                      :foreground "DarkOrange1"
                      :background "default"))

(defun ar/compile-autoclose (buffer string)
  "Hide successful builds window with BUFFER and STRING."
  (cond ((string-match "finished" string)
         (message "Build finished")
         (run-with-timer 2 nil
                         #'delete-window
                         (get-buffer-window buffer t)))
        (t
         (next-error)
         (when (equal major-mode 'objc-mode)
           (next-error))
         (message "Compilation exited abnormally: %s" string))))

(use-package compile
  :config
  ;; TODO: Shouldn't this
  ;; Automatically hide successful builds window.
  (setq compilation-finish-functions #'ar/compile-autoclose))

;; Automatically scroll build output.
(csetq compilation-scroll-output t)

;; Prevent Extraneous Tabs.
;; From http://www.gnu.org/software/emacs/manual/html_node/eintr/Indent-Tabs-Mode.html
(setq-default indent-tabs-mode nil)

(use-package delsel
  :config
  ;; Override selection with new text.
  (delete-selection-mode +1))

(use-package electric
  :config
  (electric-indent-mode +1))

;; Highlight matching parenthesis.
(use-package paren :ensure t
  :config
  (show-paren-mode +1)
  ;; Without this matching parens aren't highlighted in region.
  (validate-setq show-paren-priority -50)
  (validate-setq show-paren-delay 0)
  ;; Highlight entire bracket expression.
  (validate-setq show-paren-style 'expression)
  (set-face-attribute 'show-paren-match nil
                      :background "default"
                      :foreground "#FA009A"))

(use-package highlight-symbol :ensure t
  :config
  (set-face-attribute 'highlight-symbol-face nil
                      :background "default"
                      :foreground "yellow")
  (validate-setq highlight-symbol-idle-delay 0.2)
  (validate-setq highlight-symbol-on-navigation-p t))

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
  (validate-setq uniquify-buffer-name-style 'forward))

;; Enabling subword mode (ie. navigate cameCase)
;; From http://www.emacswiki.org/emacs/CamelCase
(global-subword-mode t)

(use-package git-timemachine :ensure t)

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
(use-package hl-line :ensure t
  :hook (prog-mode . hl-line-mode))

;; Momentarily highlights cursor on scrolling events.
;; Disabling. Not needed when using hl-line.
;; (use-package beacon :ensure t
;;   :config (beacon-mode))

;; Highlights yanked/pasted text until next operation.
(use-package volatile-highlights :ensure t
  :config (volatile-highlights-mode t))

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
  :config
  (validate-setq vc-follow-symlinks t)
  :bind ("C-x v f" . vc-pull))

(csetq css-indent-offset 2)

(use-package markdown-mode :ensure t
  :mode (("\\.text\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)))
(use-package markdown-mode+ :ensure t)

(use-package time :config
  (csetq display-time-world-list '(("Europe/Paris" "Paris")
                                   ("Europe/London" "London")
                                   ("America/Los_Angeles" "Los Angeles"))))

(use-package sudo-edit :ensure t)

(defun ar/hippie-expand-advice-fun (orig-fun &rest r)
  "Disable `case-fold-search' in ORIG-FUN and R."
  (let ((case-fold-search nil))
    (apply orig-fun r)))

(use-package hippie-exp
  :bind ("M-/" . hippie-expand)
  :config
  ;; Make hippie expand respect case sensitivity.
  (advice-add 'hippie-expand
              :around
              'ar/hippie-expand-advice-fun)
  (validate-setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                                    try-expand-dabbrev-visible
                                                    try-expand-dabbrev-all-buffers
                                                    try-expand-dabbrev-from-kill
                                                    try-complete-file-name-partially
                                                    try-complete-file-name
                                                    ;; From word before point according to all abbrev tables.
                                                    try-expand-all-abbrevs
                                                    try-expand-list
                                                    try-expand-line
                                                    ;; From entire line in a different buffer.
                                                    try-expand-line-all-buffers)))

;; Thank you Sacha Chua.
;; From http://pages.sachachua.com/.emacs.d/Sacha.html#sec-1-4-8
(fset 'yes-or-no-p 'y-or-n-p)

(use-package files
  :config
  ;; Disable backup.
  ;; From: http://anirudhsasikumar.net/blog/2005.01.21.html
  (validate-setq backup-inhibited t)
  ;; Ensure files end with newline.
  (csetq require-final-newline t)
  ;; Disable auto save.
  ;; From: http://anirudhsasikumar.net/blog/2005.01.21.html
  (csetq auto-save-default nil))

;; From http://www.wisdomandwonder.com/wordpress/wp-content/uploads/2014/03/C3F.html
(use-package savehist
  :config
  (validate-setq savehist-file "~/.emacs.d/savehist")
  (savehist-mode +1)
  (validate-setq savehist-save-minibuffer-history t)
  (validate-setq history-length 1000)
  (validate-setq savehist-additional-variables
                 '(kill-ring
                   search-ring
                   regexp-search-ring)))

(use-package recentf
  :config
  (validate-setq recentf-exclude '("/auto-install/" ".recentf" "/repos/" "/elpa/"
                                   "\\.mime-example" "\\.ido.last" "COMMIT_EDITMSG"
                                   ".gz" "~$" "/tmp/" "/ssh:" "/sudo:" "/scp:"))
  (validate-setq recentf-max-saved-items 200
                 recentf-max-menu-items 50)
  (recentf-mode))

;; Don't let the cursor go into minibuffer prompt.
;; From http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
(csetq minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

;; Not ready for consumption.
;; (defun ar/minibuffer-keyboard-quit-dwim (&optional arg)
;;   (interactive "P")
;;   (if (eq (window-buffer (minibuffer-window))
;;           (current-buffer))
;;       (if (len (minibuffer-contents))
;;           (delete-minibuffer-contents)
;;         (keyboard-quit))
;;     (keyboard-quit)))
;; (define-key minibuffer-local-map (kbd "C-g") #'ar/minibuffer-keyboard-quit-dwim)

;; Smarter move to beginning/end of line.
(use-package mwim
  :ensure t
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

;; Removing accidental use. Don't need compose-mail (yet anyway).
(global-unset-key (kbd "C-x m"))

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

(use-package sgml-mode
  :after electric
  :bind (:map sgml-mode-map
              ;; Do not auto indent current line when pressing <RET>.
              ("<RET>" . electric-indent-just-newline)))

(defun ar/open-line ()
  "Insert an empty line after current line.  Keep existing position."
  (interactive)
  (save-mark-and-excursion
    (end-of-line)
    (newline)))

(bind-key "C-o" #'ar/open-line)

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

(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

;; Add git state highlighting to dired (a la K for zsh).
;; (use-package dired-k
;;   :ensure t
;;   :config
;;   (add-hook 'dired-initial-position-hook 'dired-k))

;; Useful for diffing directories.
(use-package ztree :ensure t)

(use-package ace-mc :ensure t)

(use-package ace-window :ensure t
  :bind (("C-x o" . ace-window))
  :config
  (validate-setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  ;; Use larger characters for ace window shortcuts.
  ;; From http://oremacs.com/2015/02/27/ace-window-leading-char
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0))))))

;; Interactively resize current window.
(use-package windsize :ensure t)
(windsize-default-keybindings)

(use-package key-chord :ensure t
  :config
  (key-chord-define-global "BB" #'other-window)
  (key-chord-mode +1))

(use-package avy :ensure t
  :after key-chord
  :init
  (key-chord-define-global "jj" #'avy-goto-char-2)
  :commands (avy-goto-char-2)
  :bind (("M-s" . avy-goto-word-1)))

;; From http://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer
(defun ar/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(key-chord-define-global "JJ" #'ar/switch-to-previous-buffer)

;; Promising background process runner.
(use-package bpr :ensure t)

;; Needs clang-format installed.
;; See http://blog.hardcodes.de/articles/63/building-clang-format-and-friends-on-osx-mountain-lion
;; See http://clang.llvm.org/docs/ClangFormat.html
(use-package clang-format :ensure t)

(use-package swift-mode :ensure t
  :init
  (defun ar/swift-mode-hook-function ()
    "Called when entering `swift-mode'."
    (add-to-list 'flycheck-checkers 'swiftlint)
    (setq-local flycheck-swiftlint-config-file
                (concat (file-name-as-directory
                         (locate-dominating-file (buffer-file-name) ".swiftlint.yml"))
                        ".swiftlint.yml"))
    (defun ar/--after-swift-save ()
      (call-process "swiftformat" nil "*swiftformat*" t "--indent" "2" buffer-file-name)
      (call-process "swiftlint" nil "*swiftlint*" t "autocorrect"
                    "--config" flycheck-swiftlint-config-file
                    "--path" buffer-file-name))

    ;; Don't forget to set sourcekit-project for the project.
    ;; (setq sourcekit-project "some/project.xcodeproj")
    (setq-local company-backends '((company-sourcekit
                                    company-swimports
                                    company-yasnippet
                                    company-dabbrev-code
                                    company-keywords
                                    company-files
                                    company-emoji
                                    company-capf)))

    (add-hook 'after-save-hook 'ar/--after-swift-save nil t))
  :hook (swift-mode . ar/swift-mode-hook-function)
  :after company-sourcekit
  :after flycheck
  :config
  (csetq swift-mode:basic-offset 2))

(use-package lua-mode :ensure t)

(use-package company-lua :ensure t)

(use-package company :ensure t
  :config
  (use-package company-dabbrev
    :config
    (validate-setq company-dabbrev-downcase nil)
    (validate-setq company-dabbrev-ignore-case nil))
  (use-package company-dabbrev-code
    :config
    (validate-setq company-dabbrev-code-ignore-case nil))
  (validate-setq company-idle-delay 0.2)
  (validate-setq company-show-numbers t)
  (validate-setq company-minimum-prefix-length 2)
  (validate-setq company-tooltip-align-annotations t)

  (global-company-mode)
  :bind
  (:map global-map
        ("<backtab>" . company-complete))
  :bind
  (:map company-search-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous))
  :bind
  (:map company-active-map
        ("C-l" . company-show-location)
        ("C-s" . company-filter-candidates)
        ("C-d" . company-show-doc-buffer)
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)))

(use-package company-shell :ensure t)

;; Smarter shell completion.
(use-package pcmpl-homebrew :ensure t)
(use-package pcmpl-git :ensure t)
(use-package pcomplete-extension :ensure t)
(use-package pcmpl-pip :ensure t)

;; Enhanced help buffers.
(use-package helpful :ensure t
  :bind
  ("C-h c" . helpful-command)
  ("C-h f" . helpful-function)
  ("C-h v" . helpful-variable))

(defun --around-sourcekit-project(f &rest r)
  "Advice around sourcekit-project, apply F and R."
  (defvar --sourcekit-project-cache (apply f r))
  (unless --sourcekit-project-cache
    (setq sourcekit-project (read-directory-name "What Xcode project? "))
    ;; Selecting an xcodeproj with read-directory-name yields a path ending with /. Remove it.
    (when (s-ends-with? "/" sourcekit-project)
      (setq sourcekit-project (s-left -1 sourcekit-project)))
    (setq --sourcekit-project-cache  sourcekit-project))
  --sourcekit-project-cache)

(use-package sourcekit :ensure t
  :config
  (advice-add 'sourcekit-project :around
              '--around-sourcekit-project)
  ;; (validate-setq sourcekit-sourcekittendaemon-executable
  ;;                (ar/assert (executable-find "sourcekittend")
  ;;                           "No sourcekittend found"))
  )

(use-package company-sourcekit :ensure t)

(use-package company-quickhelp :ensure t
  :config
  (company-quickhelp-mode +1))

(use-package company-c-headers :ensure t)

(use-package company-emoji :ensure t)

;; Slow. Disabling.
;; (use-package emojify :ensure t
;;   :config
;;   (global-emojify-mode +1))

(use-package objc-mode
  :init
  (defun ar/objc-mode-hook-function ()
    "Called when entering `objc-mode'."
    ;; Hook is run twice. Avoid:
    ;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=16759
    (unless (boundp 'objc-mode-hook-did-run)
      (ar/clang-format-toggle-automatic)
      (objc-font-lock-mode)
      (helm-dash-activate-docset "iOS")
      (set-fill-column 100)

      (validate-setq company-grep-grep-flags "--type objc --no-line-number --color never --no-filename --smart-case --regexp")
      (validate-setq company-grep-grep-format-string "^#import\\s*\".*%s")
      (validate-setq company-grep-grep-trigger "import \"")
      (validate-setq company-grep-grep-cleanup-fun (lambda (items)
                                                     (mapcar (lambda (item)
                                                               (ar/string-match item "import +\"\\(.*\\)\"" 1))
                                                             items)))
      (setq-local company-backends '((company-grep company-files company-yasnippet company-keywords company-clang)))

      ;; (setq-local company-backends '((company-rtags)))
      ;; NOTE: Disabling while trying irony out
      ;; (setq-local company-backends
      ;;      ;; List with multiple back-ends for mutual inclusion.
      ;;      '(( ;;company-ycmd
      ;;         company-yasnippet
      ;;         company-gtags
      ;;         company-dabbrev-code
      ;;         company-files)))
      ;;(ycmd-mode)
      (setq-local objc-mode-hook-did-run t)))
  :hook (objc-mode . ar/objc-mode-hook-function)
  :bind (:map objc-mode-map
              ([f6] . recompile)))

;; Disabling rtags
;; (use-package rtags :ensure t
;;   :bind
;;   (:map
;;    objc-mode-map
;;    ("M-." . rtags-find-symbol-at-point))
;;   :config
;;   ;; Work in progress.
;;   ;; (use-package flycheck-rtags)
;;   (validate-setq rtags-autostart-diagnostics t) ;; For company support.
;;   (validate-setq rtags-completions-enabled t) ;; For company support.
;;   (validate-setq rtags-path "~/stuff/active/code/rtags/bin")
;;   (validate-setq rtags-use-helm t)
;;   ;; TODO: Change to subtle colors.
;;   (set-face-attribute 'rtags-warnline nil
;;                       :background nil)
;;   (set-face-attribute 'rtags-errline nil
;;                       :background nil))

;; Work in progress.
;; (defun ar/flycheck-rtags-setup ()
;;   (interactive)
;;   (flycheck-select-checker 'rtags)
;;   (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
;;   (setq-local flycheck-check-syntax-automatically nil))

;; Needed for endlessparentheses's hack.
(use-package cider :ensure t)

;; From http://endlessparentheses.com/eval-result-overlays-in-emacs-lisp.html
(defun endless/eval-overlay (value point)
  (cider--make-result-overlay (format "%S" value)
    :where point
    :duration 'command)
  ;; Preserve the return value.
  value)

(advice-add 'eval-region :around
            (lambda (f beg end &rest r)
              (endless/eval-overlay
               (apply f beg end r)
               end)))

(advice-add 'edebug-eval-defun :around
            (lambda (f &rest r)
              (endless/eval-overlay
               (apply f r)
               (point))))

(advice-add 'pp-eval-expression :filter-return
            (lambda (r)
              (endless/eval-overlay r (point))))

(advice-add 'eval-last-sexp :filter-return
            (lambda (r)
              (endless/eval-overlay r (point))))

(advice-add 'eval-defun :filter-return
            (lambda (r)
              (endless/eval-overlay
               r
               (save-excursion
                 (end-of-defun)
                 (point)))))

;; (add-to-list 'load-path
;;              (concat (getenv "HOME") "/.emacs.d/downloads/rtags/src"))
;; (require 'rtags)
;; (require 'company-rtags)
;; (validate-setq rtags-path
;;       (concat (getenv "HOME") "/.emacs.d/downloads/rtags/bin"))
;; (setq-local company-backends (delete 'company-clang company-backends))
;; (validate-setq company-rtags-begin-after-member-access t)
;; (validate-setq rtags-completions-enabled t)
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
  ;; Disabling. Too slow on large projects.
  ;; (add-hook 'objc-mode-hook 'irony-mode)
  ;; (add-hook 'irony-mode-hook (lambda ()
  ;;                              ;; Irony can be slow on large compilation databases.
  ;;                              ;; Experimenting with delay here, since it's most annoying
  ;;                              ;; when opening files (UI blocks for 5 seconds).
  ;;                              (setq-local ar/irony-cdb-sutosetup-timer
  ;;                                          (run-with-idle-timer 3 nil
  ;;                                                               (lambda ()
  ;;                                                                 (irony-cdb-autosetup-compile-options)
  ;;                                                                 (message "irony setup for %s" (buffer-name)))))
  ;;                              (add-hook 'kill-buffer-hook
  ;;                                        (lambda ()
  ;;                                          (cancel-timer ar/irony-cdb-sutosetup-timer))
  ;;                                        t t)))
  )

(use-package company-irony :ensure t
  ;; :config
  ;; Disabling irony. Slow in large projects.
  ;; (add-hook 'objc-mode-hook (lambda ()
  ;;                             (setq-local company-backends '((company-irony)))))
  ;; (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  )

(use-package drag-stuff :ensure t
  :bind (("M-<up>" . drag-stuff-up)
         ("M-<down>" . drag-stuff-down)))

;; displays hex strings representing colors
(use-package rainbow-mode :ensure t)

;; If eclim is your cup of tea.
;; (require 'eclim)
;; (global-eclim-mode)
;; (csetq eclim-eclipse-dirs '("~/tools/eclipse"))
;; (csetq eclim-executable "~/tools/eclipse/eclim")
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
;; (validate-setq ycmd-server-command (list "python"
;;                                 (expand-file-name "~/.emacs.d/downloads/ycmd/ycmd")))
;; (validate-setq ycmd--log-enabled t)

;; Consider elpy mode instead. See https://github.com/daschwa/emacs.d
;; Consider company jedi. See https://github.com/syohex/emacs-company-jedi
(use-package anaconda-mode :ensure t
  :commands (anaconda-mode))

(use-package company-anaconda :ensure t)

;; View, browse, rotate, manipulate images with picpocket.
(use-package picpocket :ensure t)

;; From http://zzamboni.org/post/my-emacs-configuration-with-commentary
(defun ar/org-reformat-buffer ()
  (interactive)
  (when (y-or-n-p "Really format current buffer? ")
    (let ((document (org-element-interpret-data (org-element-parse-buffer))))
      (erase-buffer)
      (insert document)
      (goto-char (point-min)))))

(defun ar/org-mark-done ()
  "Mark current item as DONE and refile."
  (interactive)
  (save-excursion
    (org-todo "DONE")
    (end-of-line)
    (insert " ")
    (org-insert-time-stamp (current-time))
    (org-refile)))

(use-package org-cliplink :ensure t)

(use-package org-crypt
  :config
  (org-crypt-use-before-save-magic)
  (csetq org-crypt-disable-auto-save nil)
  (csetq org-tags-exclude-from-inheritance (quote ("crypt")))
  ;;  Set to nil to use symmetric encryption.
  (csetq org-crypt-key nil))

;; To print ASCII art from text like:
;;   __ _      _     _
;;  / _(_)__ _| |___| |_
;; |  _| / _` | / -_)  _|
;; |_| |_\__, |_\___|\__|
;;       |___/
(use-package figlet :ensure t
  :commands figlet
  :config
  (validate-setq figlet-default-font "small")
  (validate-setq figlet-options (list "-w 160")))

(use-package ob-swift :ensure t
  :after ob
  :config
  (add-to-list 'org-babel-load-languages '(swift . t)))

(use-package ob-objc)

(use-package gnuplot :ensure t)

(use-package ob
  :config
  (validate-setq org-export-babel-evaluate nil)
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
     (objc . t)
     (shell . t)
     (js . t)
     (sql . nil)
     (sqlite . t))))

;; Plan London Underground journeys.
;; (use-package org-tfl :ensure t
;;   :after org)

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

(use-package niceify-info :ensure t)

;; Enable searching info via info-lookup-symbol (ie. C-h S).
(use-package pydoc-info :ensure t)

(use-package elpy :ensure t
  :hook (python-mode . elpy-enable))

;; Disabled anaconda in favor of elpy.
;; (defun ar/python-mode-hook-function-anaconda ()
;;   "Called when entering `python-mode'."
;;   (validate-setq python-indent-offset 2)
;;   ;; Ensure we have an inferior Python process running.
;;   (run-python "/usr/bin/python -i")
;;   (anaconda-mode)
;;   (eldoc-mode +1)
;;   ;; FIXME python-docstring-mode currently broken
;;   ;; (python-docstring-mode +1)
;;   (setq-local company-backends '(company-anaconda))
;;   (py-yapf-enable-on-save))

(use-package python-docstring :ensure t
  :hook (python-mode . python-docstring-mode))

(use-package python
  :init
  (defun ar/python-mode-hook-function ()
    "Called when entering `python-mode'."
    (validate-setq python-indent-offset 2)
    (validate-setq python-indent-guess-indent-offset nil))
  :hook (python-mode . ar/python-mode-hook-function))

(use-package objc-font-lock
  :ensure t
  :config
  ;; Overriding faces not properly displayed in exported org files.
  (set-face-attribute 'font-lock-function-name-face nil :foreground "#dcdcdc")
  (set-face-attribute 'objc-font-lock-function-name nil :foreground "#dcdcdc")
  (validate-setq objc-font-lock-background-face nil))

(use-package dummy-h-mode
  :mode (("\\.h\\'" . dummy-h-mode)))

(use-package go-mode :ensure t
  :hook (go-mode . ar/go-mode-hook-function)
  :mode ("\\.go\\'" . go-mode)
  :config
  (csetq gofmt-command "goimports")

  (use-package go-snippets :ensure t
    :config
    (go-snippets-initialize))

  (use-package company-go :ensure t)
  ;; go get -u github.com/golang/lint/golint
  (use-package golint :ensure t)

  (use-package go-eldoc :ensure t
    :config
    (go-eldoc-setup))

  (use-package gotest :ensure t)

  ;; go get -u golang.org/x/tools/cmd/gorename
  (use-package go-rename :ensure t)

  (use-package godoctor :ensure t)
  ;; Requires gocode daemon. Install with:
  ;; go get -u golang.org/x/tools/cmd/...
  ;; go get -u github.com/nsf/gocode
  ;; go get -u github.com/rogpeppe/godef
  ;; go get -u golang.org/x/tools/cmd/goimports
  ;; Useful info at:
  ;; From http://tleyden.github.io/blog/2014/05/22/configure-emacs-as-a-go-editor-from-scratch
  ;; From http://tleyden.github.io/blog/2014/05/27/configure-emacs-as-a-go-editor-from-scratch-part-2
  ;; From http://dominik.honnef.co/posts/2013/03/writing_go_in_emacs
  (defun ar/go-mode-hook-function ()
    "Called when entering `go-mode'."
    (helm-dash-activate-docset "Go")
    (setq-local company-backends '(company-go))
    (validate-setq tab-width 2 indent-tabs-mode t)
    (add-hook 'before-save-hook #'gofmt-before-save t t))
  :bind
  (:map go-mode-map
        ("M-." . godef-jump)))

;; From http://endlessparentheses.com/faster-pop-to-mark-command.html?source=rss
(defadvice pop-to-mark-command (around ensure-new-position activate)
  "Continue popping mark until the cursor is actually moved."
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))
(csetq set-mark-command-repeat-pop t)

(defun ar/split-camel-region ()
  "Splits camelCaseWord to camel case word."
  (interactive)
  (let ((case-fold-search nil))
    (while (re-search-forward "[A-Z]" (region-end) t)
      (replace-match (format " %s"
                             (downcase (match-string 0)))
                     t nil))))

;; Simplify lisp navigation/editing (ie. slurp/barf).
;; Disabling lispy for the time being (affecting imenu).
;; (use-package lispy :ensure t
;;   :config
;;   (bind-key "M-i" #'helm-swoop lispy-mode-map))

;; M-. elisp navigation.
(use-package elisp-slime-nav :ensure t)

(use-package package-lint :ensure t)

;; Edit Emacs variables/state inline.
(use-package refine :ensure t)

;; Evaluate line on the fly and overlay result.
(use-package litable :ensure t)

;; Edit HTML templates in Javascript code (automatically escape).
(use-package string-edit :ensure t
  :mode (("\\.js\\'" . rjsx-mode)
         ("\\.jsx\\'" . rjsx-mode)))

(defun ar/add-functions-to-mode-hooks (hook-functions hooks)
  "Add HOOK-FUNCTIONS to mode HOOKS."
  (dolist (hook hooks)
    (dolist (hook-function hook-functions)
      (add-hook hook hook-function))))

;; Display information about function or variable in minibuffer.
(use-package eldoc
  :after pos-tip
  :config
  (validate-setq eldoc-idle-delay 0.2)
  ;; https://www.topbug.net/blog/2016/11/03/emacs-display-function-or-variable-information-near-point-cursor
  (defun ar/eldoc-display-message (format-string &rest args)
    "Display eldoc message near point as well as minibuffer."
    (when format-string
      ;; Disabling for now. It slows down scrolling while flashing empty pos tip.
      ;; (pos-tip-show (apply 'format format-string args))
      (funcall 'eldoc-minibuffer-message format-string args)))

  (validate-setq eldoc-message-function #'ar/eldoc-display-message))

(defun ar/emacs-lisp-mode-hook-function ()
  "Called when entering `emacs-lisp-mode'."
  (bind-key "RET" 'comment-indent-new-line emacs-lisp-mode-map)
  ;; (helm-dash-activate-docset "Emacs Lisp")
  ;; Pretty print output to *Pp Eval Output*.
  (local-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)
  ;; Disabling lispy for the time being (affecting imenu).
  ;; (lispy-mode +1)
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

(defun ar/clang-format-buffer ()
  "Clang format current buffer."
  (interactive)
  (clang-format (point-min)
                (point-max)))

;; Disabled. Figure out the right helm-xcdoc-document-path.
;; (use-package helm-xcdoc :ensure t
;;   :config
;;   (validate-setq
;;    helm-xcdoc-command-path (ar/file-assert-file-exists "/Applications/Xcode.app/Contents/Developer/usr/bin/docsetutil")
;;    helm-xcdoc-document-path (ar/file-assert-file-exists "/Applications/Xcode.app/Contents/Developer/Documentation/DocSets/com.apple.adc.documentation.iOS.docset")))

(defun ar/clang-format-toggle-automatic ()
  "Toggle automatic clang formatting on save."
  (interactive)
  (if (member #'ar/clang-format-buffer
              before-save-hook)
      (remove-hook #'before-save-hook
                   #'ar/clang-format-buffer
                   t)
    (add-hook 'before-save-hook
              #'ar/clang-format-buffer
              nil
              'make-it-local))
  (print before-save-hook))

(use-package smart-jump
  :ensure t
  :config
  (smart-jump-setup-default-registers)
  (setq dumb-jump-selector 'helm)
  :bind ("M-." . smart-jump-go))

;; Growl-workalike for Emacs.
;; (use-package alert :ensure t
;;   :config
;;   (csetq alert-default-style 'osx-notifier))

(use-package cc-mode
  :init
  (defun ar/java-mode-hook-function ()
    "Called when entering `java-mode'."
    ;; 2-char indent for java.
    (defvar c-basic-offset)
    (validate-setq c-basic-offset 2)
    (set-fill-column 100))
  :hook (java-mode . ar/java-mode-hook-function))

(use-package immortal-scratch :ensure t
  :config
  (immortal-scratch-mode))

(use-package persistent-scratch :ensure t
  :config
  (persistent-scratch-setup-default))

(use-package tldr :ensure t)

;; Produce HTML from CSS-like selectors. TODO: Enable for HTML mode.
(use-package emmet-mode :ensure t)

(use-package nodejs-repl :ensure t)
(use-package js-comint :ensure t)
(use-package js-import :ensure t)

(use-package indium :ensure t)

(use-package prettier-js :ensure t
  :config
  (validate-setq prettier-js-args
                 '(
                   "--trailing-comma" "all"
                   )))

(use-package tide :ensure t
  :init
  (defun ar/setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (validate-setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (prettier-js-mode +1)
    (setq-local company-backends '(company-tide
                                   (company-dabbrev-code
                                    company-gtags
                                    company-etags
                                    company-keywords)
                                   company-files
                                   company-dabbrev))
    (company-mode +1))
  :after web-mode
  :config
  (add-hook 'js2-mode-hook #'ar/setup-tide-mode)
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (ar/setup-tide-mode))))
  :mode (("\\.tsx\\'" . web-mode)))

(use-package company-flow :ensure t)

(use-package flycheck-flow :ensure t)

(defun ar/js2-mode-hook-function ()
  "Called when entering `js2-mode'."
  ;; Enable for requirejs.
  ;; (requirejs-mode)
  (js2-imenu-extras-setup)
  (setq-local js2-basic-offset 2)
  (validate-setq company-tooltip-align-annotations t)
  (validate-setq company-tern-meta-as-single-line t)
  (setq-local company-backends '(company-tern
                                 (company-dabbrev-code
                                  company-gtags
                                  company-etags
                                  company-keywords)
                                 company-files
                                 company-dabbrev))
  (tern-mode +1)
  ;; Moving about by list and expression.
  ;; From http://jbm.io/2014/01/react-in-emacs-creature-comforts/
  (modify-syntax-entry ?< "(>")
  (modify-syntax-entry ?> ")<"))

(use-package html-check-frag :ensure t
  :hook (html-mode . html-check-frag-mode))

(use-package rjsx-mode :ensure t
  :hook (js2-mode . ar/js2-mode-hook-function)
  :mode (("\\.js\\'" . rjsx-mode)
         ("\\.jsx\\'" . rjsx-mode)))

;; Not using. Disabling.
;; (use-package requirejs :ensure t)

;; Disabling in favor of rjsx-mode.
;; (use-package js2-mode :ensure t
;;   :mode (("\\.js\\'" . js2-mode)
;;         ("\\.jsx\\'" . js2-mode))
;;   :after requirejs-emacs
;;   ;; Enable for node
;;   ;; :interpreter "node"
;;   :config
;;   ;; Enable for node
;;   ;; (ar/process-assert-binary-installed "node")
;;   (add-hook #'js2-mode-hook #'ar/js2-mode-hook-function))

(use-package protobuf-mode :ensure t
  :config
  (defun ar/reindex-proto-fields ()
    "From within a proto message, reindex all proto field tags."
    (interactive)
    (save-excursion
      (save-restriction
        (narrow-to-defun)
        (goto-char (point-min))
        (let ((counter 1))
          (while (search-forward-regexp "\\(\\(\\(optional\\)\\|\\(required\\)\\).*= *\\)[1-9]+" nil t)
            (replace-match (format "\\1%d" counter) t nil)
            (setq counter (+ counter 1))))))))

(use-package dart-mode :ensure t)

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
(csetq sentence-end-double-space nil)

(use-package shr-color
  :config
  ;; These help to render HTML email in mu4e.
  (validate-setq shr-color-visible-luminance-min 70)
  (validate-setq shr-color-visible-distance-min 5))

(use-package flyspell
  :bind (:map
         flyspell-mode-map
         ("C-M-i" . ar/auto-correct-word-then-abbrev))
  :init
  ;; From http://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html
  (defun ar/auto-correct-word-then-abbrev (p)
    "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev.  Otherwise it will
be global."
    (interactive "P")
    (let (bef aft)
      (save-excursion
        (while (if (setq bef (thing-at-point 'word))
                   ;; Word was corrected or used quit.
                   (if (ispell-word nil 'quiet)
                       nil ; End the loop.
                     ;; Also end if we reach `bob'.
                     (not (bobp)))
                 ;; If there's no word at point, keep looking
                 ;; until `bob'.
                 (not (bobp)))
          (backward-word))
        (setq aft (thing-at-point 'word)))
      (if (and aft bef (not (equal aft bef)))
          (let ((aft (downcase aft))
                (bef (downcase bef)))
            (define-abbrev
              (if p local-abbrev-table global-abbrev-table)
              bef aft)
            (message "\"%s\" now expands to \"%s\" %sally"
                     bef aft (if p "loc" "glob")))
        (user-error "No typo at or before point"))))
  :config
  (use-package abbrev)
  (use-package ispell))

;; Maybe helps with #slow flyspell in org mode.
;; Seem to interfere with mu4e. Disabling momentarily.
;; (use-package flyspell-lazy :ensure t
;;   :after flyspell
;;   :config
;;   (flyspell-lazy-mode +1))

;; #slow
;; (use-package fill-column-indicator :ensure t
;;   :commands (turn-on-fci-mode))

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
  :hook (web-mode . ar/web-mode-hook-function)
  :config
  (validate-setq web-mode-code-indent-offset 2)
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it)))

(defun ar/tern-delete-process ()
  "Delete tern.jsp process."
  (interactive)
  (delete-process "Tern"))

(use-package tern :ensure t)

(use-package company-tern :ensure t
  :config
  (validate-setq company-tern-meta-as-single-line t))

(use-package js
  :config
  (setq js-indent-level 2))

;; From https://github.com/howardabrams/dot-files/blob/HEAD/emacs-client.org
(use-package color-theme-sanityinc-tomorrow :ensure t)

;; From https://github.com/howardabrams/dot-files/blob/HEAD/emacs-client.org
;; (ar/change-theme 'color-theme-sanityinc-tomorrow-night
;;                  'ar/org-src-color-blocks-dark)

(use-package aggressive-indent :ensure t
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package highlight-quoted :ensure t
  :hook (prog-mode . highlight-quoted-mode))

(use-package centered-cursor-mode :ensure t
  :pin melpa
  :init
  ;; Workaround to use centered-cursor-mode in --nw.
  (defvar mouse-wheel-mode nil))

(use-package focus :ensure t)

(defun ar/company-fci-workaround ()
  "Enable a workaround to disable fci while company-completing."
  (defvar-local company-fci-mode-on-p nil
    "Keep track if fci-mode if currently on.")
  ;; Disable fci if needed.
  (add-hook 'company-completion-started-hook (lambda (&rest ignore)
                                               (when (boundp 'fci-mode)
                                                 (validate-setq company-fci-mode-on-p fci-mode)
                                                 (when fci-mode (fci-mode -1)))))
  ;; Re-enable fci if needed.
  (add-hook 'company-completion-finished-hook (lambda (&rest ignore)
                                                (when company-fci-mode-on-p (fci-mode +1))))
  ;; Re-enable fci if needed.
  (add-hook 'company-completion-cancelled-hook (lambda (&rest ignore)
                                                 (when company-fci-mode-on-p (fci-mode +1)))))
(defun ar/prog-mode-hook-function ()
  "Called when entering all programming modes."
  (let ((m prog-mode-map))
    (define-key m [f6] #'recompile))
  (prettify-symbols-mode +1)
  (highlight-symbol-mode +1)
  (highlight-symbol-nav-mode +1)
  (validate-setq show-trailing-whitespace t)
  ;; Spellcheck comments and documentation
  ;; From http://mwolson.org/projects/emacs-config/init.el.html
  (flyspell-prog-mode)
  ;;(hl-line-mode)
  (rainbow-mode)
  (centered-cursor-mode)
  ;; Language-aware editing commands. Useful for imenu-menu.
  (semantic-mode +1)
  ;; #slow
  ;; (turn-on-fci-mode)
  ;; (ar/company-fci-workaround)
  )

(defun ar/markdown-mode-hook-function ()
  "Called when entering `markdown-mode'."
  (setq-local markdown-indent-on-enter nil)
  (set-fill-column 80)
  (local-set-key (kbd "RET")
                 #'electric-newline-and-maybe-indent))

(ar/add-functions-to-mode-hooks '(ar/prog-mode-hook-function)
                                '(prog-mode-hook))

(ar/add-functions-to-mode-hooks '(ar/prog-mode-hook-function
                                  ar/markdown-mode-hook-function)
                                '(markdown-mode-hook))

;; Select help window by default.
(csetq help-window-select t)

(use-package comint
  :config
  ;; Ensure shell prompts are read-only.
  (validate-setq comint-prompt-read-only t))

(use-package openwith :ensure t
  :config
  (csetq openwith-associations
         (cond
          ((ar/osx-p)
           '(("\\.\\(dmg\\|doc\\|docs\\|xls\\|xlsx\\)$"
              "open" (file))
             ("\\.\\(mp4\\|mp3\\|webm\\|avi\\|flv\\|mov\\)$"
              "open" ("-a" "VLC" file))))
          ((ar/linux-p)
           '(("\\.\\(mp4\\|mp3\\|webm\\|avi\\|flv\\|mov\\)$"
              "xdg-open" (file))))))
  (openwith-mode +1))

(use-package menu-bar
  ;; No need to confirm killing buffers.
  :bind ("C-x k" . kill-this-buffer))

;; Looks up commands/topics on cheat.sh.
(use-package cheat-sh :ensure t)

(use-package shell-pop :ensure t
  :config
  (defun ar/shell-pop (shell-pop-autocd-to-working-dir)
    "Shell pop with arg to cd to working dir. Else use existing location."
    (interactive "P")
    ;; shell-pop-autocd-to-working-dir is defined in shell-pop.el.
    ;; Using lexical binding to override.
    (if (string= (buffer-name) shell-pop-last-shell-buffer-name)
        (shell-pop-out)
      (shell-pop-up shell-pop-last-shell-buffer-index)))

  (csetq shell-pop-window-position "full")
  (csetq shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell shell-pop-term-shell))))

  (validate-setq shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell))))

  ;; Customize shell-pop.
  ;; Disabling while trying eshell out.
  ;; (validate-setq shell-pop-term-shell "/bin/bash")

  ;; Trying shell out. Disabling ansi-term for now.
  ;; (validate-setq shell-pop-shell-type '("ansi-term"
  ;;                              "terminal"
  ;;                              (lambda
  ;;                                nil (ansi-term shell-pop-term-shell))))
  (validate-setq shell-pop-window-position "full")
  :bind (([f5] . ar/shell-pop)))

(csetq company-clang-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/clang++")
(csetq company-clang-arguments
       `(
         "-isysroot" "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk"
         "-I" "/usr/include/c++/4.2.1"
         "-target" "arm64-apple-darwin"
         ;; "-target" "x86_64-apple-darwin"
         "-I" "/usr/local/lib/ocaml/"))
(setq flycheck-c/c++-clang-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/clang++")

(use-package shrink-path
  :ensure t)

(use-package eshell
  :commands eshell
  :init
  (defun ar/eshell-mode-hook-function ()
    ;; Turn off semantic-mode in eshell buffers.
    (semantic-mode -1)
    (smartparens-strict-mode +1)
    (eshell-smart-initialize)
    (setq-local global-hl-line-mode nil)
    (setq-local company-backends '((company-projectile-cd company-escaped-files)))
    ;; comint-magic-space needs to be whitelisted to ensure we receive company-begin events in eshell.
    (setq-local company-begin-commands (append company-begin-commands (list 'comint-magic-space)))
    (bind-key "C-l" #'ar/eshell-cd-to-parent eshell-mode-map)
    (bind-key "<backtab>" #'company-complete eshell-mode-map)
    (bind-key "<tab>" #'company-complete eshell-mode-map))
  :hook (eshell-mode . ar/eshell-mode-hook-function)
  :config
  (use-package em-hist)
  (use-package em-glob)
  (use-package esh-mode
    :config
    ;; Why is validate-setq not finding it?
    (csetq eshell-scroll-to-bottom-on-input 'all)
    (defun eshell/clear ()
      "Alias to clear (destructive) eshell content."
      (interactive)
      (let ((inhibit-read-only t))
        (erase-buffer))))
  (use-package em-dirs)
  (use-package em-smart)

  ;; Avoid "WARNING: terminal is not fully functional."
  ;; http://mbork.pl/2018-06-10_Git_diff_in_Eshell
  (setenv "PAGER" "cat")

  (validate-setq eshell-where-to-jump 'begin)
  (validate-setq eshell-review-quick-commands nil)
  (validate-setq eshell-smart-space-goes-to-end t)

  (validate-setq eshell-history-size (* 10 1024))
  (validate-setq eshell-hist-ignoredups t)
  (validate-setq eshell-error-if-no-glob t)
  (validate-setq eshell-glob-case-insensitive t)
  (validate-setq eshell-list-files-after-cd nil)

  (add-hook 'eshell-mode-hook
            (lambda ()
              (add-to-list 'eshell-visual-commands "ssh")
              (add-to-list 'eshell-visual-commands "tail")
              (add-to-list 'eshell-visual-commands "top")))

  (defun ar/eshell-cd-to-parent ()
    (interactive)
    (goto-char (point-max))
    (insert "cd ..")
    (eshell-send-input nil t))

  (use-package ar-eshell-config
    :after validate
    :after eshell
    :after shrink-path))

(defun eshell/ec (file-path)
  "Alias to open FILE-PATH."
  (find-file file-path))

(defun eshell/e (file-path)
  "Alias to open FILE-PATH."
  (find-file file-path))

(defun eshell/a ()
  "Change PWD to active dir."
  (eshell/cd "~/stuff/active"))

(defun eshell/c ()
  "Change PWD to active dir."
  (eshell/cd "~/stuff/active/code/"))

(use-package eshell-autojump :ensure t)

(defun ar/shell-mode-hook-function ()
  "Called when entering shell mode."
  ;; Enable company completion on TAB when in shell mode.
  ;; (company-mode)
  ;; (bind-key "TAB" #'company-manual-begin shell-mode-map)
  (setq-local company-backends '((company-projectile-cd
                                  ;; company-bash-history
                                  ;; company-rfiles
                                  ;; company-shell
                                  ))))

;; This is a hack. Let's see how it goes.
(defun ar/shell-directory-tracker (str)
  "Overrides `shell-directory-tracker behavior.  Ignore STR and call `dirs instead."
  (dirs))

(use-package shell
  ;; :init
  ;; (advice-add 'shell-directory-tracker
  ;;             :override
  ;;             'ar/shell-directory-tracker)
  :hook (shell-mode . ar/shell-mode-hook-function))

;; ;; comint-magic-space needs to be whitelisted to ensure we still receive company-begin events.
;; (add-to-list 'company-begin-commands 'comint-magic-space)
;; :bind
;; (:map smartparens-strict-mode-map
;;       ("SPC" . comint-magic-space))

(use-package term
  :init
  (defun ar/term-mode-hook-function ()
    "Called when entering term mode."
    ;; Don't need trailing spaces highlighted in terminal.
    (setq-local whitespace-style '(face empty tabs)))
  :hook (term-mode . ar/term-mode-hook-function))

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
                (validate-setq p (point-marker))
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
        (validate-setq p (point-marker)))
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
      (validate-setq start (save-excursion
                             (goto-char (region-beginning))
                             (beginning-of-line)
                             (point))
                     end (save-excursion
                           (goto-char (region-end))
                           (end-of-line)
                           (point))))
    (comment-or-uncomment-region start end)))
(bind-key "M-;" #'ar/comment-dwim)

(defun ar/comment-dwim-next-line ()
  "Like `ar/comment-dwim', but also move to next line."
  (interactive)
  (call-interactively #'ar/comment-dwim)
  (next-line))
(bind-key "C-M-;" #'ar/comment-dwim-next-line)

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
  :config
  (validate-setq winner-dont-bind-my-keys t)
  (defun ar/dwim-key-esc ()
    "Do what I mean when pressing ESC."
    (interactive)
    (cond ((string-equal major-mode 'shell-mode)
           (keyboard-escape-quit))
          ((string-equal major-mode 'term-mode)
           (term-send-raw-meta))
          (t
           (winner-undo))))
  (validate-setq winner-boring-buffers
                 (append winner-boring-buffers '("*helm M-x*"
                                                 "helm mini*"
                                                 "*helm projectile*")))
  (winner-mode +1))

(use-package auto-compile :ensure t
  :config
  (auto-compile-on-load-mode +1)
  (auto-compile-on-save-mode +1))

;; Collaborate with clipboard.
(csetq select-enable-clipboard t)
;; More expected region behaviour.
(transient-mark-mode t)

(use-package executable
  ;;  Make a shell script executable automatically on save.
  ;;  From https://github.com/bbatsov/prelude
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

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
        (validate-setq url
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

(defun ar/open-youtube-clipboard-url ()
  "Download youtube video from url in clipboard."
  (interactive)
  (ar/open-youtube-url (current-kill 0)))

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
;; (desktop-save-mode +1)
;;  Number of buffers to restore immediately.
;; (validate-setq desktop-restore-eager 10)

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

(use-package powerthesaurus :ensure t)

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

;; Disabling rich text clipboard support (Used on macOX).
;; (unless (ar/linux-p)
;;   ;; No linux support.
;;   (use-package highlight2clipboard :ensure t))

(use-package writegood-mode :ensure t)

(use-package flycheck :ensure t
  :hook (after-init . global-flycheck-mode)
  :config
  ;; TODO: Ensure proselint is installed.
  ;; From http://unconj.ca/blog/linting-prose-in-emacs.html
  ;; Disabling. Lots of locks in org mode.
  ;; (flycheck-define-checker proselint
  ;;   "A linter for prose."
  ;;   :command ("proselint" source-inplace)
  ;;   :error-patterns
  ;;   ((warning line-start (file-name) ":" line ":" column ": "
  ;;             (id (one-or-more (not (any " "))))
  ;;             (message) line-end))
  ;;   :modes (gfm-mode
  ;;           markdown-mode
  ;;           org-mode
  ;;           text-mode))
  ;;(add-to-list 'flycheck-checkers 'proselint)
  ;; Override default flycheck triggers
  (validate-setq flycheck-check-syntax-automatically
                 '(save idle-change mode-enabled)
                 flycheck-idle-change-delay 0.8))

(use-package flycheck-inline
  :ensure t
  :config
  (flycheck-inline-mode +1))

;; Trying out flycheck-inline first.
;; (use-package flycheck-posframe :ensure t
;;   :after flycheck
;;   :config
;;   (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

(use-package osx-dictionary
  :if (memq window-system '(mac ns))
  :ensure t)

(use-package pos-tip :ensure t
  :config
  (when (fboundp 'tooltip-mode) (tooltip-mode +1)))

;; No Objective-C 'other file' support out of the box. Fix that.
(csetq cc-other-file-alist
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
(csetq save-interprogram-paste-before-kill t)

(use-package region-bindings-mode :ensure
  :config
  (region-bindings-mode-enable))

(use-package multiple-cursors :ensure t
  :after region-bindings-mode
  :init
  (global-unset-key (kbd "M-<down-mouse-1>"))
  :bind (("C-c a" . mc/mark-all-like-this)
         ("C-c n" . mc/mark-more-like-this-extended)
         ("M-1" . mc/mark-next-like-this)
         ("M-!" . mc/unmark-next-like-this)
         ("M-2" . mc/mark-previous-like-this)
         ("M-@" . mc/unmark-previous-like-this)
         ("M-<mouse-1>" . mc/add-cursor-on-click))
  :bind (:map region-bindings-mode-map
              ("a" . mc/mark-all-like-this)
              ("p" . mc/mark-previous-like-this)
              ("n" . mc/mark-next-like-this)
              ("P" . mc/unmark-previous-like-this)
              ("N" . mc/unmark-next-like-this)
              ("m" . mc/mark-more-like-this-extended)
              ("h" . mc-hide-unmatched-lines-mode)
              ("\\" . mc/vertical-align-with-space)
              ("#" . mc/insert-numbers) ; use num prefix to set the starting number
              ("^" . mc/edit-beginnings-of-lines)
              ("$" . mc/edit-ends-of-lines)))

(use-package origami :ensure t
  :after key-chord
  :config
  (key-chord-define-global "QQ" #'origami-toggle-all-nodes)
  (key-chord-define-global "qq" #'origami-recursively-toggle-node)
  (global-origami-mode))

(use-package phi-search :ensure t)
(use-package phi-rectangle :ensure t)
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

;; Major mode for reading EPUB documents.
(use-package nov :ensure t)

(use-package hydra :ensure t
  :config (validate-setq hydra-is-helpful t))

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

(use-package display-line-numbers
  :init
  (defhydra hydra-goto-line (:pre (progn
                                    ;; Disabling. Slow on large files.
                                    ;; (global-git-gutter-mode -1)
                                    (display-line-numbers-mode +1))
                                  :post (progn
                                          ;; Disabling. Slow on large files.
                                          ;; (global-git-gutter-mode +1)
                                          (display-line-numbers-mode -1))
                                  :color blue)
    "goto"
    ("g" goto-line "line")
    ("c" goto-char "char")
    ("q" nil "quit"))
  :bind (("M-g" . hydra-goto-line/body)))

(defhydra hydra-open-c-mode (:color blue)
  "open"
  ("o" ff-find-other-file "other")
  ("e" ar/platform-open-in-external-app "externally")
  ("u" ar/platform-open-file-at-point "url at point")
  ("b" ar/file-open-closest-build-file "build file")
  ("q" nil "cancel"))

(defhydra hydra-open (:color blue)
  "
Open: _p_oint _e_xternally
      _u_rls
"
  ("e" ar/platform-open-in-external-app nil)
  ("p" ar/platform-open-file-at-point nil)
  ("u" link-hint-open-link nil)
  ("q" nil "cancel"))

(defhydra hydra-open-prog-mode (:color blue)
  "open"
  ("o" ff-find-other-file "other")
  ("e" ar/platform-open-in-external-app "externally")
  ("u" ar/platform-open-file-at-point "url at point")
  ("b" ar/file-open-closest-build-file "build file")
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
  ("d" ar/helm-ag "search directory")
  ("r" ar/projectile-helm-ag "search repository")
  ("f" ar/find-dired-current-dir "find file")
  ("a" ar/find-all-dired-current-dir "find all files")
  ("i" ar/helm-ag-insert "insert match")
  ("q" nil "quit"))
(bind-key "C-c s" #'hydra-search/body)

(defhydra hydra-git-gutter (:pre (git-gutter-mode +1))
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

(defun ar/try-smerge ()
  "Activate smerge on conflicts."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode +1))))

(use-package stripe-buffer :ensure t
  :after dired-mode
  :hook (dired-mode . turn-on-stripe-buffer-mode))

(use-package cl
  :init
  ;; Ignore running processes when closing Emacs
  ;; From http://oremacs.com/2015/01/04/dired-nohup
  (defadvice save-buffers-kill-emacs
      (around no-query-kill-emacs activate)
    "Prevent \"Active processes exist\" query on exit."
    (cl-letf (((symbol-function #'process-list) (lambda ())))
      ad-do-it)))

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

(defun ar/org-insert-link-dwim ()
  "Convert selected region into a link with clipboard http link (if one is found). Default to `org-insert-link' otherwise."
  (interactive)
  (if (and (string-match-p "^http" (current-kill 0))
           (region-active-p))
      (let ((region-content (buffer-substring-no-properties (region-beginning)
                                                            (region-end))))
        (delete-region (region-beginning)
                       (region-end))
        (insert (format "[[%s][%s]]"
                        (current-kill 0)
                        region-content)))
    (call-interactively 'org-insert-link)))

(use-package org
  :init
  (defun ar/org-mode-hook-function ()
    "Called when entering org mode."
    (setq-local company-backends '((company-yasnippet
                                    company-keywords
                                    company-files
                                    company-emoji
                                    company-capf)))
    (toggle-truncate-lines 0)
    (validate-setq show-trailing-whitespace t)
    (set-fill-column 1000)
    (flyspell-mode +1)
    (org-display-inline-images))
  :ensure t
  :hook ((org-mode . ar/org-mode-hook-function)
         (org-mode . visual-line-mode))
  :config
  (csetq org-todo-keywords
         '((sequence
            "TODO"
            "STARTED"
            "DONE"
            "OBSOLETE"
            "CANCELLED")))

  (use-package org-faces
    :config
    (csetq org-todo-keyword-faces
           '(("TODO" . (:foreground "red" :weight bold))
             ("STARTED" . (:foreground "yellow" :weight bold))
             ("DONE" . (:foreground "green" :weight bold))
             ("OBSOLETE" . (:foreground "blue" :weight bold))
             ("CANCELLED" . (:foreground "gray" :weight bold)))))

  ;; Look into font-locking email addresses.
  ;; http://kitchingroup.cheme.cmu.edu/blog/category/email/
  (use-package button-lock :ensure t)

  (csetq org-refile-targets '((nil . (:regexp . "Week of"))
                              (nil . (:regexp . "RESOLVED"))))

  (validate-setq org-ellipsis "…")
  (validate-setq org-fontify-emphasized-text t)
  ;; Fontify code in code blocks.
  (validate-setq org-src-fontify-natively t)
  ;; When exporting anything, do not insert in kill ring.
  (validate-setq org-export-copy-to-kill-ring nil)
  ;; Display images inline when running in GUI.
  (validate-setq org-startup-with-inline-images (display-graphic-p))
  (validate-setq org-src-tab-acts-natively t)
  ;; Prevent inadvertently editing invisible areas in Org.
  (validate-setq org-catch-invisible-edits 'error)
  (validate-setq org-cycle-separator-lines 2)
  (validate-setq org-image-actual-width nil)
  (validate-setq org-hide-emphasis-markers t)
  ;; All Org leading stars become invisible.
  (validate-setq org-hide-leading-stars t)
  ;; Skip Org's odd indentation levels (1, 3, ...).
  (validate-setq org-odd-levels-only t)
  ;; Disable auto isearch within org-goto.
  (validate-setq org-goto-auto-isearch nil)
  ;; Enable RET to follow Org links.
  (validate-setq org-return-follows-link t)
  :bind
  (:map org-mode-map
        ("C-c C-l" . ar/org-insert-link-dwim)
        ("M-C-y" . ar/yank-line-below)))

;; Required by code block syntax highlighting.
(use-package htmlize :ensure t)

(ignore-errors
  (use-package org-beautify-theme :ensure t
    :config
    (when (display-graphic-p)
      ;; From https://github.com/howardabrams/dot-files/blob/HEAD/emacs-client.org
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
                                `(org-document-title ((t (,@headline ,@sans-font :height 1.5 :underline nil))))
                                '(org-block-begin-line ((t (:underline nil :foreground "#008ED1" :background nil))))
                                '(org-block ((t (:background "#202020"))))
                                '(org-block-end-line ((t (:overline nil :foreground "#008ED1" :background nil))))
                                )))))

(use-package org-bullets :ensure t
  :hook (org-mode . org-bullets-mode)
  :config
  (validate-setq org-bullets-bullet-list
                 '("◉" "◎" "⚫" "○" "►" "◇")))

;; From http://emacsredux.com/blog/2015/01/18/clear-comint-buffers/
(defun ar/comint-clear-buffer ()
  "Clear the content of shell/REPL."
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(use-package moody
  :ensure t
  :config
  ;; Fixes mode line separator issues.
  (validate-setq ns-use-srgb-colorspace nil)
  (validate-setq x-underline-at-descent-line t)
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

;; Wonderful weather forecast.
;; Disabling (needs login now).
;; (use-package sunshine :ensure t
;;   :config
;; (when (window-system)
;;   (validate-setq sunshine-show-icons t))
;; (validate-setq sunshine-units 'metric)
;; (validate-setq sunshine-location "London, GB"))

;; Weather forecast (no login/account needed).
(use-package wttrin :ensure t
  :commands wttrin
  :config
  (csetq wttrin-default-accept-language '("Accept-Language" . "en-GB"))
  (csetq wttrin-default-cities (list "London"))
  (defalias 'ar/weather 'wttrin))

(defun ar/kill-region-advice-fun (orig-fun &rest r)
  "Advice function around `kill-region' (ORIG-FUN and R)."
  (if (or (null (nth 2 r)) ;; Consider kill-line (C-k).
          mark-active)
      (apply orig-fun r)
    ;; Kill entire line.
    (let ((last-command (lambda ())) ;; Override last command to avoid appending to kill ring.
          (offset (- (point)
                     (line-beginning-position))))
      (apply orig-fun (list (line-beginning-position)
                            (line-end-position)
                            nil))
      (delete-char 1)
      (forward-char (min offset
                         (- (line-end-position)
                            (line-beginning-position)))))))

(advice-add 'kill-region
            :around
            'ar/kill-region-advice-fun)

;; From https://github.com/daschwa/emacs.d
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single
line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-end-position)))))

(use-package simple
  :config
  ;; Don't bother saving things to the kill-ring twice, remove duplicates.
  (csetq kill-do-not-save-duplicates t)
  ;; Wait a bit longer than the default (0.5 seconds) before assuming Emacs is idle.
  (csetq idle-update-delay 2)
  ;; Increase mark ring size.
  (csetq global-mark-ring-max 500))

(use-package isearch
  :preface
  (provide 'isearch)
  :config
  (use-package char-fold)

  (csetq search-default-mode #'char-fold-to-regexp)
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
      ad-do-it)))

;; Open gyp files in prog-mode.
(add-to-list 'auto-mode-alist '("\\.gyp\\'" . prog-mode))

;; Open rc files with conf-mode.
(use-package conf-mode
  :mode ("rc$" . conf-mode))

;; For plantuml see https://zhangweize.wordpress.com/2010/09/20/update-plantuml-mode
;; (use-package  puml-mode :ensure t)

(defun ar/org-confirm-babel-evaluate (lang body)
  "Do not confirm org babel evaluation for LANG and BODY."
  (and
   (not (string= lang "emacs-lisp"))
   (not (string= lang "plantuml"))
   (not (string= lang "python"))))

(use-package ob-plantuml
  :config
  (use-package org-src)
  ;; Use fundamental mode when editing plantuml blocks with C-c '
  (add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))
  (validate-setq org-confirm-babel-evaluate 'ar/org-confirm-babel-evaluate)
  (cond ((ar/osx-p)
         (validate-setq org-plantuml-jar-path "~/homebrew/Cellar/plantuml/1.2018.5/libexec/plantuml.jar")
         (setenv "GRAPHVIZ_DOT" (expand-file-name "~/homebrew/bin/dot")))
        (t
         (message "Warning: Could not find plantuml.8018.jar")
         (message "Warning: Could not find $GRAPHVIZ_DOT location"))))

;; Avoid native dialogs when running graphical.
(when (boundp 'use-dialog-box)
  (validate-setq use-dialog-box nil))

(use-package server
  :defer 10
  :config
  (unless (server-running-p)
    (server-start)))

(run-with-idle-timer 5 nil
                     'ar/load-all-files
                     "~/.emacs.d/local/*.el")

;; Use a hook so the message doesn't get clobbered by other messages.
;; From https://zzamboni.org/post/my-emacs-configuration-with-commentary
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(provide 'init)
;;; init.el ends here
