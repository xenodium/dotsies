;;; -*- lexical-binding: t; -*-

;; Remember scratch content across sessions.
(use-package persistent-scratch
  :ensure t
  :defer 0.1
  :config
  ;; Show persistent scratch ASAP.
  (persistent-scratch-setup-default)
  ;; Reload emacs-lisp-mode (with all the goodies from lazy-loaded packages)
  (run-with-timer 10 nil (lambda ()
                           (with-current-buffer "*scratch*"
			     (emacs-lisp-mode)))))

;; Lock scratch buffer (can't be killed).
(run-with-timer 5 nil (lambda ()
                        (with-current-buffer "*scratch*"
                          (emacs-lock-mode 'kill))))
