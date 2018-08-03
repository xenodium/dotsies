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
  (ar/csetq auto-save-default nil))

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
