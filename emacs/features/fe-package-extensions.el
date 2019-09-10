(use-package use-package-ensure-system-package
  :ensure t)

(use-package use-package-chords
  :ensure t
  :config
  (run-with-idle-timer 2 nil
                       (lambda ()
                         (key-chord-mode 1))))

;; Ask shell for PATH, MANPATH, and exec-path and update Emacs environment.
;; We do this early on as we assert binaries are installed throughout
;; init.
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package auto-compile
  :ensure t
  :config
  (auto-compile-on-load-mode +1)
  (auto-compile-on-save-mode +1))
