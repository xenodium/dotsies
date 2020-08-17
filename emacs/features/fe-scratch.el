;;; -*- lexical-binding: t; -*-

;; Remember scratch content across sessions.
(use-package persistent-scratch
  :ensure t
  :defer 0.1
  :config
  ;; Show persistent scratch ASAP.
  (persistent-scratch-setup-default)
  ;; Reload emacs-lisp-mode (with all the goodies from lazy-loaded packages)
  (run-with-timer 60 nil (lambda ()
                           (with-current-buffer "*scratch*"
			     (emacs-lisp-mode +1)))))

;; Lock scratch buffer (can't be killed).
(run-with-timer 5 nil (lambda ()
                        (with-current-buffer "*scratch*"
                          (emacs-lock-mode 'kill))))
