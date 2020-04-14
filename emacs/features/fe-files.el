;;; -*- lexical-binding: t; -*-
(use-package files
  :validate-custom
  ;; read-only buffers enable view-mode (C-x C-q enables editing).
  (view-read-only t)
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
    "Open clipboard file URL."
    (interactive)
    (find-file (current-kill 0))))

(use-package autorevert
  :validate-custom
  ;; Auto refresh dired.
  ;; https://mixandgo.com/learn/how-ive-convinced-emacs-to-dance-with-ruby
  (global-auto-revert-non-file-buffers t)

  ;; Be quiet about dired refresh.
  (auto-revert-verbose nil)
  :config
  (global-auto-revert-mode +1))

;; Avoid creating lock files (ie. .#some-file.el)
(setq create-lockfiles nil)

(use-package recentf
  :validate-custom
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

  (defun ar/recentf-delete-entry ()
    "Delete a recentf entry."
    (interactive)
    (let ((selection (completing-read "delete: " recentf-list)))
      (when (> (length selection) 0)
        (setq recentf-list
              (seq-remove (lambda (candidate)
                            (string-equal candidate selection)) recentf-list))
        (message "%S removed from the list" selection))))

  (recentf-mode +1))
