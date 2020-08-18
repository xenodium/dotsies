;;; -*- lexical-binding: t; -*-

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :validate-custom
  (magit-diff-refine-hunk 'all)
  (magit-status-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
  (magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  :config
  (load "~/.emacs.d/features/config-magit"))

(use-package git-gutter
  :ensure t
  :hook ((prog-mode . ar/delayed-git-gutter-mode)
         (protobuf-mode . git-gutter-mode))
  :bind (("C-c <up>" . git-gutter:previous-hunk)
         ("C-c <down>" . git-gutter:next-hunk))
  :validate-custom
  (git-gutter:handled-backends '(git hg))
  :config
  (defun ar/delayed-git-gutter-mode ()
    "Git gutter can take time to load on some repos. Delay enabling."
    (if (file-remote-p default-directory)
        (message "Ignoring git-gutter (tramp): %s" buffer-file-name)
      (let ((buffer (current-buffer)))
        (run-with-idle-timer 3 nil
                             (lambda ()
                               (when (buffer-live-p buffer)
                                 (with-current-buffer buffer
                                   (git-gutter-mode +1)))))))))

(use-package gitconfig-mode
  :ensure t
  :mode "\\.?gitconfig.?.*\\'")

(use-package gitignore-mode
  :ensure t
  :mode "\\.?gitignore.?.*\\'")

(use-package git-timemachine
  :commands git-timemachine
  :ensure t)

(use-package vc-hooks
  :defer 10
  :custom
  (vc-handled-backends '(Git)))
