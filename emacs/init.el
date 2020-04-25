;;; init.el --- This is my init.    -*- lexical-binding: t; -*-

;;; Commentary:
;; There is where my Emacs config starts.


;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; init.el gc values (faster loading) ;;;;

(setq gc-cons-threshold (* 384 1024 1024)
      gc-cons-percentage 0.6)

;; Default of 800 was too low.
;; Avoid Lisp nesting exceeding in swift-mode.
(setq max-lisp-eval-depth 3000)
(setq max-specpdl-size 3000)

;;; Temporarily avoid loading any modes during init (undone at end).
(defvar ar/init--file-name-handler-alist file-name-handler-alist)

;;; Set to t to debug (load synchronously).
(defvar ar/init-debug-init nil)

(setq file-name-handler-alist nil)

;; Match theme color early on (smoother transition).
;; Theme loaded in features/ui.el.
(set-background-color "#1b181b")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Hide UI (early on) ;;;;

;; Don't want a mode line while loading init.
(setq mode-line-format nil)

;; No scrollbar by default.
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; No nenubar by default.
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; No toolbar by default.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; No tooltip by default.
(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))

;; No Alarms by default.
(setq ring-bell-function 'ignore)

;; Get rid of splash screens.
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)

;; Set momentary title.
(when (display-graphic-p)
  (setq frame-title-format "loading..."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Set up package tls ;;;;

;; Do not load outdated byte code files.
(setq load-prefer-newer t)

(require 'package)

;; Don't auto-initialize.
(setq package-enable-at-startup nil)

;; Don't add that `custom-set-variables' block to init.
(setq package--init-file-ensured t)

;; Save custom vars to separate file from init.el.
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(require 'tls)

;; From https://irreal.org/blog/?p=8243
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; From https://github.com/hlissner/doom-emacs/blob/5dacbb7cb1c6ac246a9ccd15e6c4290def67757c/core/core-packages.el#L102
(setq gnutls-verify-error (not (getenv "INSECURE")) ; you shouldn't use this
      tls-checktrust gnutls-verify-error
      tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
                        ;; compatibility fallbacks
                        "gnutls-cli -p %p %h"
                        "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof"))

(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")))

(setq package-archive-priorities
      '(("melpa" .  4)
        ("melpa-stable" . 3)
        ("org" . 2)
        ("gnu" . 1)))

(when (< emacs-major-version 27)
  (unless package--initialized
    (package-initialize)))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; use-package-enable-imenu-support must be
;; set before requiring use-package.
(setq use-package-enable-imenu-support t)
(require 'use-package)
;; (setq use-package-verbose t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Now kick off non-essential loading ;;;;

(defun ar/load-non-core-init ()
  "Load non-core initialisation."
   ;; Undo GC values post init.el.
   (setq gc-cons-threshold 16777216
         gc-cons-percentage 0.1)
   (run-with-idle-timer 5 t #'garbage-collect)
   (setq garbage-collection-messages t)
   (setq file-name-handler-alist ar/init--file-name-handler-alist)

   ;; Done loading core init.el. Announce it and let the heavy loading begin.
   (message "Emacs ready in %s with %d garbage collections."
            (format "%.2f seconds" (float-time
                                    (time-subtract after-init-time before-init-time)))
            gcs-done)

   ;; Additional load paths.
   (add-to-list 'load-path "~/.emacs.d/ar")
   (add-to-list 'load-path "~/.emacs.d/local")
   (add-to-list 'load-path "~/.emacs.d/external")
   (add-to-list 'load-path "~/.emacs.d/downloads")

   ;; Need these loaded ASAP (many subsequent libraries depend on them).
   (load "~/.emacs.d/features/fe-package-extensions.el")
   (load "~/.emacs.d/features/fe-libs.el")
   (load "~/.emacs.d/features/fe-mac.el")
   (load "~/.emacs.d/features/fe-linux.el")
   (load "~/.emacs.d/features/fe-ui.el")
   (load "~/.emacs.d/features/fe-scratch.el")

   ;; Load non-core features.
   (load "~/.emacs.d/features/fe-features.el"))

(if ar/init-debug-init
    (ar/load-non-core-init)
  (add-hook
   'emacs-startup-hook
   #'ar/load-non-core-init))

(provide 'init)
;;; init.el ends here
