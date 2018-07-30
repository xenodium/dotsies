(use-package files
  :config
  ;; Disable backup.
  ;; From: http://anirudhsasikumar.net/blog/2005.01.21.html
  (vsetq backup-inhibited t)
  ;; Ensure files end with newline.
  (csetq require-final-newline t)
  ;; Disable auto save.
  ;; From: http://anirudhsasikumar.net/blog/2005.01.21.html
  (csetq auto-save-default nil))
