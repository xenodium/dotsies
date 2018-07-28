;; Put off GC until 10MB of allocation or 5s of idle time.
(setq gc-cons-threshold (* 10 1024 1024))
(setq gc-cons-percentage 0.2)
(run-with-idle-timer 5 t #'garbage-collect)
(setq garbage-collection-messages t)

;; Match theme color early on, so loading experience is smoother.
(set-background-color "#1b181b")

;; Transparent titlebar on
(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)) ; nil for dark text
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

;; Hide UI (early on).

;; Don't want a mode line while loading init.
(setq mode-line-format nil)

;; No scrollbar by default.
(when (fboundp 'toggle-scroll-bar) (toggle-scroll-bar -1))
;; No nenubar by default.
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;; No toolbar by default.
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; No Alarms.
(setq ring-bell-function 'ignore)
;; Empty buffer loads faster than scratch as default.
(setq initial-buffer-choice (lambda ()
                              (get-buffer-create "welcome")))

;; Additional load paths.
(add-to-list 'load-path "~/.emacs.d/ar")
(add-to-list 'load-path "~/.emacs.d/local")
(add-to-list 'load-path "~/.emacs.d/external")

;; Ask shell for PATH, MANPATH, and exec-path and update Emacs environment.
;; We do this early on as we assert binaries are installed throughout
;; init.
(load-file (expand-file-name "~/.emacs.d/downloads/exec-path-from-shell/exec-path-from-shell.el"))
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; macOS basics.
(when (string-equal system-type "darwin")
  (menu-bar-mode 1)
  ;; Fixes mode line separator issues on macOS.
  (setq ns-use-srgb-colorspace nil)
  (setq mac-command-modifier 'meta)
  (setq exec-path (append exec-path '("~/homebrew/bin"
                                      "~/homebrew/Cellar/llvm/HEAD/bin"
                                      "/usr/local/bin"))))

;;;; Set up package tls START

(require 'cl)
(require 'package)
(require 'tls)

(assert (executable-find "gnutls-cli") nil
        "Need brew install gnutls or apt-get install gnutls-bin")
(assert (eq 0 (call-process-shell-command "python -c \"import certifi\"")) nil
        "Need python -m pip install --user certifi")

(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string "python -m certifi")))))
  (setq tls-program
        (list
         (format "gnutls-cli --x509cafile %s -p %%p %%h" trustfile)))
  (setq gnutls-trustfiles (list trustfile)))

(setq tls-checktrust t)
(setq package-archives `(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;;;; Set up package tls END

;;;; Set up use-package START

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-enable-imenu-support t)
(setq use-package-verbose t)

(use-package async
  :ensure t
  :config
  (async-bytecomp-package-mode +1))

(use-package use-package-ensure-system-package
  :ensure t)

;;;; Set up use-package END

;;;; Init helpers START

(use-package validate
  :ensure t
  :config
  (defalias 'vsetq 'validate-setq))

;;;; Init helpers END

;;;; Appearance START

(when (display-graphic-p)
  ;; Enable if you'd like to start as fullscreen.
  ;; (set-frame-parameter nil 'fullscreen 'fullboth)
  ;; Don't want titles in frames.
  (validate-setq frame-title-format '("Ⓔ ⓜ ⓐ ⓒ ⓢ")))

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-atelier-heath t)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#695d69")))

;; Ensure window is maximized after window setup.
(use-package maxframe
  :ensure t
  :hook (window-setup . maximize-frame))

(use-package moody
  :ensure t
  :config
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

(use-package fullframe
  :ensure t
  :commands fullframe)

;;;; Appearance END

;;;; Init maintenance START

;; Find errors in init.el by bisecting the file.
(use-package bug-hunter
  :ensure t
  :commands bug-hunter-init-file)

;; Restart Emacs from Emacs.
(use-package restart-emacs
  :ensure t
  :commands restart-emacs)

(use-package esup
  :ensure t
  :commands esup)

;; Peak into macros by expanding them inline.
(use-package macrostep
  :ensure t
  :commands macrostep-expand)

(defun ar/edit-init ()
  (interactive)
  (find-file "~/stuff/active/code/dots/emacs/newinit.el"))
;;;; Init maintenance END

;;;; Editing START

;; Prevent Extraneous Tabs.
;; From http://www.gnu.org/software/emacs/manual/html_node/eintr/Indent-Tabs-Mode.html
(setq-default indent-tabs-mode nil)

(use-package expand-region
  :ensure t
  :bind ("C-c w" . er/expand-region))

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
  (validate-setq show-trailing-whitespace t)
  (set-face-attribute 'whitespace-line nil
                      :foreground "DarkOrange1"
                      :background "default"))

(use-package smartparens :ensure t
  ;; Add to minibuffer also.
  :hook ((minibuffer-setup . smartparens-mode))
  :config
  (require 'smartparens-config)
  (require 'smartparens-html)
  (require 'smartparens-python)
  (smartparens-global-strict-mode +1)

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

  ;; I prefer keeping C-w to DWIM kill, provided by
  ;; `ar/kill-region-advice-fun'. Removing remap.
  ;;   (define-key smartparens-strict-mode-map [remap kill-region] nil)

  (defun ar/smartparens-wrap-square-bracket (arg)
    "[] equivalent of `paredit-wrap-round'."
    (interactive "P")
    (save-excursion
      (unless (sp-point-in-symbol)
        (backward-char))
      (sp-wrap-with-pair "["))
    (insert " "))

  :bind (:map smartparens-strict-mode-map
              ([remap kill-region] . kill-region)
              ("C-c <right>" . sp-forward-slurp-sexp)
              ("C-c <left>" . sp-forward-barf-sexp)
              ("M-[" . sp-rewrap-sexp)
              :map smartparens-mode-map
              ("M-]" . ar/smartparens-wrap-square-bracket)))


(use-package region-bindings-mode
  :ensure t
  :defer 2
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

;; From https://github.com/daschwa/emacs.d
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single
line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-end-position)))))

(use-package hungry-delete
  :ensure t
  :config (global-hungry-delete-mode))

;;;; Editing END

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes)
  (message "magit")
  (fullframe magit-status magit-mode-quit-window))

;; Ivy equivalents to Emacs commands.
(use-package counsel
  :ensure t
  :defer 0.1
  :config
  ;; Smex handles M-x command sorting. Bringing recent commands to the top.
  (use-package smex
    :ensure t)
  (counsel-mode +1))

(use-package ivy
  :ensure t
  :defer 0.1
  :config
  (validate-setq ivy-height 40)
  (validate-setq ivy-count-format "")
  (validate-setq ivy-use-virtual-buffers t)
  (validate-setq enable-recursive-minibuffers t)
  (ivy-mode +1))

;;;; Navigation START
(use-package isearch
  :commands (isearch-forward isearch-backward)
  :defer
  :preface
  (provide 'isearch)
  :config
  (use-package char-fold)

  (validate-setq search-default-mode #'char-fold-to-regexp)

  ;; Prepopulate isearch with selectionn.
  ;; From http://www.reddit.com/r/emacs/comments/2amn1v/isearch_selected_text
  (defadvice isearch-mode (around isearch-mode-default-string
                                  (forward &optional regexp op-fun recursive-edit word-p) activate)
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

;; Smarter move to beginning/end of line.
(use-package mwim
  :ensure t
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

(use-package centered-cursor-mode
  :defer 0.1
  :ensure t
  :pin melpa
  :init
  ;; Workaround to use centered-cursor-mode in --nw.
  (defvar mouse-wheel-mode nil)
  :config
  (global-centered-cursor-mode +1))

;;;; Navigation END

;;;; Helm START

(use-package helm-ag :ensure t
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

  (defun ar/helm-ag (arg)
    "Helm-ag search remembering last location.  With ARG, forget the last location."
    (interactive "P")
    (defvar ar/helm-ag--default-locaction nil)
    (when (or arg (not ar/helm-ag--default-locaction))
      (validate-setq ar/helm-ag--default-locaction
                     (read-directory-name "search in: " default-directory nil t)))
    (helm-do-ag ar/helm-ag--default-locaction))

  (defun ar/find-dired-current-dir ()
    "Find files from current location."
    (interactive)
    (helm-find t))

  (defun ar/find-all-dired-current-dir ()
    "Invokes `find-dired' for current dir."
    (interactive)
    (let ((dir (if buffer-file-name
                   (file-name-directory buffer-file-name)
                 ".")))
      (find-dired dir "'(' -name .svn -o -name .git ')' -prune -o -type f")))


  (cond ((executable-find "rg")
         (validate-setq helm-ag-base-command "rg --vimgrep --no-heading --ignore-case"))
        ((executable-find "pt")
         (validate-setq helm-ag-base-command "pt -e --nocolor --nogroup"))
        ((executable-find "ag")
         (validate-setq helm-ag-base-command "ag --nocolor --nogroup"))
        (t
         (validate-setq helm-ag-base-command "ack --nocolor --nogroup"))))

;;;; Helm END

;;;; Hydra START
(use-package hydra
  :ensure t
  :config
  (validate-setq hydra-is-helpful t)
  (defhydra hydra-search (:color blue)
    "search"
    ("d" ar/helm-ag "search directory")
    ("f" ar/find-dired-current-dir "find file")
    ("a" ar/find-all-dired-current-dir "find all files")
    ("i" ar/helm-ag-insert "insert match")
    ("q" nil "quit"))
  :bind (("C-c s" . hydra-search/body)))
;;;; Hydra END

;;;; Buffers START
(use-package menu-bar
  ;; No need to confirm killing buffers.
  :bind ("C-x k" . kill-this-buffer))
;;;; Buffers END

(use-package ar-mu4e
  :if (locate-library "ar-mu4e")
  :bind (:map global-map
              ("M-m" . ar/mu4e--view-unread-messages)))

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

;; Local Variables:
;; compile-command: "make newinit"
;; End:
