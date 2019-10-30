;;; -*- lexical-binding: t; -*-
(use-package files
  :custom-validated
  ;; Always display opened file using canonical location (not symlink).
  (find-file-visit-truename t)
  ;; From: http://anirudhsasikumar.net/blog/2005.01.21.html
  (backup-inhibited t)
  ;; Disable backup.
  ;; Ensure files end with newline.
  ;; (require-final-newline t)
  ;; Disable auto save.
  ;; From: http://anirudhsasikumar.net/blog/2005.01.21.html
  (auto-save-default nil)
  ;; I've inadvertedly exited Emacs far too many times.
  ;; Ask for confirmation.
  (confirm-kill-emacs 'yes-or-no-p)
  ;; Open that large file! YOLO. Ok, got `openwith' to handle it.
  (large-file-warning-threshold nil)
  :config
  (require 'ar-platform)

  (defun ar/open-clipboard-file-externally ()
    (interactive)
    (funcall (ar/platform-open-in-external-app-function) (current-kill 0)))

  (defun ar/open-clipboard-file ()
    (interactive)
    (find-file (current-kill 0))))

(use-package autorevert
  :custom-validated
  ;; Auto refresh dired.
  ;; https://mixandgo.com/learn/how-ive-convinced-emacs-to-dance-with-ruby
  (global-auto-revert-non-file-buffers t)

  ;; Be quiet about dired refresh.
  (auto-revert-verbose nil)
  :config
  (global-auto-revert-mode))

;; Avoid creating lock files (ie. .#some-file.el)
(setq create-lockfiles nil)

(use-package recentf
  :custom-validated
  (recentf-exclude '("/auto-install/"
                     ".recentf"
                     "/repos/"
                     "/elpa/"
                     "\\.mime-example"
                     "\\.ido.last"
                     "COMMIT_EDITMSG"
                     ".gz"
                     "~$"
                     "/ssh:"
                     "/sudo:"
                     "/scp:"))
  (recentf-max-saved-items 1000
                           recentf-max-menu-items 50)
  :config
  (use-package recentf-ext
    :ensure t)

  (recentf-mode +1))
