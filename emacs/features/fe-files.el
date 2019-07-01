(require 'ar-vsetq)
(require 'ar-csetq)

(use-package files
  :config
  (require 'ar-platform)
  ;; Disable backup.
  ;; From: http://anirudhsasikumar.net/blog/2005.01.21.html
  (ar/vsetq backup-inhibited t)
  ;; Ensure files end with newline.
  ;; (ar/csetq require-final-newline t)
  ;; Disable auto save.
  ;; From: http://anirudhsasikumar.net/blog/2005.01.21.html
  (ar/csetq auto-save-default nil)

  ;; I've inadvertedly exited Emacs far too many times.
  ;; Ask for confirmation.
  (ar/csetq confirm-kill-emacs 'yes-or-no-p)

  ;; Open that large file! YOLO. Ok, got `openwith' to handle it.
  (ar/csetq large-file-warning-threshold nil)

  (defun ar/open-clipboard-file-externally ()
    (interactive)
    (funcall (ar/platform-open-in-external-app-function) (current-kill 0)))

  (defun ar/open-clipboard-file ()
    (interactive)
    (find-file (current-kill 0))))

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
                              "/ssh:"
                              "/sudo:"
                              "/scp:"))
  (ar/vsetq recentf-max-saved-items 1000
            recentf-max-menu-items 50)

  (use-package recentf-ext
    :ensure t)

  (recentf-mode +1))
