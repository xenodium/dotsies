
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

;;;; Set up use-package - START

;; TODO: Can we optimize ar-package?
(require 'ar-package)
(ar/package-initialize)

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

;;;; Set up use-package - END

;;;; Init helpers - START

(use-package validate :ensure t)

;;;; Init helpers - END

;;;; Appearance - START

(when (display-graphic-p)
  ;; Enable if you'd like to start as fullscreen.
  ;; (set-frame-parameter nil 'fullscreen 'fullboth)
  ;; Don't want titles in frames.
  (validate-setq frame-title-format '("Ⓔ ⓜ ⓐ ⓒ ⓢ")))

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

;;;; Appearance - END

;;;; Init maintenance - START

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

;;;; Init maintenance - END

;;;; Editing - START

(use-package expand-region
  :ensure t
  :bind ("C-c w" . er/expand-region)
  :config
  ;; Workaround fixing expand-region:
  ;; https://github.com/magnars/expand-region.el/issues/220
  ;; (validate-setq shift-select-mode nil)
  )

;;;; Editing - END

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

;;;; Buffers START
(use-package menu-bar
  ;; No need to confirm killing buffers.
  :bind ("C-x k" . kill-this-buffer))
;;;; Buffers END

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
