;;; -*- lexical-binding: t; -*-
;; Remember scratch content across sessions.
(use-package persistent-scratch
  :ensure t
  :config
  (with-current-buffer "*scratch*"
    (emacs-lock-mode 'kill))
  ;; Show persistent scratch ASAP.
  (persistent-scratch-setup-default)
  ;; Reload emacs-lisp-mode in a few seconds.
  ;; (with all the goodies from lazy-loaded packages)
  (run-with-timer 3 nil #'emacs-lisp-mode))
