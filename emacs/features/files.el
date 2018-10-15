(require 'ar-vsetq)
(require 'ar-csetq)

(use-package files
  :config
  ;; Disable backup.
  ;; From: http://anirudhsasikumar.net/blog/2005.01.21.html
  (ar/vsetq backup-inhibited t)
  ;; Ensure files end with newline.
  (ar/csetq require-final-newline t)
  ;; Disable auto save.
  ;; From: http://anirudhsasikumar.net/blog/2005.01.21.html
  (ar/csetq auto-save-default nil)

  ;; I've inadvertedly exited Emacs far too many times.
  ;; Ask for confirmation.
  (ar/csetq confirm-kill-emacs 'yes-or-no-p))

(use-package autorevert
  :config
  ;; Auto refresh dired.
  ;; https://mixandgo.com/learn/how-ive-convinced-emacs-to-dance-with-ruby
  (ar/csetq global-auto-revert-non-file-buffers t)

  ;; Be quiet about dired refresh.
  (ar/csetq auto-revert-verbose nil)

  (global-auto-revert-mode))

;; Avoid creating lock files (ie. .#some-file.el)
(setq create-lockfiles nil)

(use-package recentf
  :config
  (ar/vsetq recentf-exclude '("/auto-install/"
                                   ".recentf"
                                   "/repos/"
                                   "/elpa/"
                                   "\\.mime-example"
                                   "\\.ido.last"
                                   "COMMIT_EDITMSG"
                                   ".gz"
                                   "~$"
                                   "/tmp/"
                                   "/ssh:"
                                   "/sudo:"
                                   "/scp:"))
  (ar/vsetq recentf-max-saved-items 1000
            recentf-max-menu-items 50)
  (recentf-mode))
