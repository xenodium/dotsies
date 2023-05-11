;;; -*- lexical-binding: t; -*-

(use-package files
  :defer
  :validate-custom
  ;; Automatically Kill Running Processes on Exit.
  ;; https://emacsredux.com/blog/2020/07/18/automatically-kill-running-processes-on-exit
  (confirm-kill-processes nil)
  ;; read-only buffers enable view-mode (C-x C-q enables editing).
  (view-read-only t)
  ;; Always display opened file using canonical location (not symlink).
  (find-file-visit-truename t)
  ;; From: http://anirudhsasikumar.net/blog/2005.01.21.html
  (backup-inhibited t)
  ;; Disable backup.
  ;; Ensure files end with newline.
  (require-final-newline t)
  ;; Disable auto save.
  ;; From: http://anirudhsasikumar.net/blog/2005.01.21.html
  (auto-save-default nil)
  ;; I've inadvertedly exited Emacs far too many times.
  ;; Ask for confirmation.
  (confirm-kill-emacs 'yes-or-no-p)
  ;; Open that large file! YOLO. Ok, got `openwith' to handle it.
  (large-file-warning-threshold nil)
  :config
  (defun ar/files-create-non-existent-directory ()
    "Create a non-existent directory."
    (when-let* ((file-name buffer-file-name)
                (parent-directory (file-name-directory file-name)))
      (when (and (not (file-exists-p parent-directory))
                 (y-or-n-p (format "Create `%s' dir? " parent-directory)))
        (make-directory parent-directory t))))

  (add-to-list 'find-file-not-found-functions
               #'ar/files-create-non-existent-directory))

(use-package autorevert
  :defer 10
  :validate-custom
  ;; Be quiet about reverts.
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  :config
  ;; global-auto-revert-mode can slow things down. try to enable it per active window.
  (add-to-list 'window-state-change-functions
               (defun ar/window-state-state-change (state)
                 (let* ((old-selected-window (old-selected-window))
                        (old-buffer (when old-selected-window
                                      (window-buffer old-selected-window)))
                        (selected-window (selected-window))
                        (new-buffer (when selected-window
                                      (window-buffer selected-window))))
                   (when old-buffer
                     (with-current-buffer old-buffer
                       (when buffer-file-name
                         ;; (message "auto-revert-mode -1 <%s>" buffer-file-name)
                         (auto-revert-mode -1))))
                   (when new-buffer
                     (with-current-buffer new-buffer
                       (when buffer-file-name
                         ;; (message "auto-revert-mode +1 <%s>" buffer-file-name)
                         (auto-revert-mode +1))))))))

;; Avoid creating lock files (ie. .#some-file.el)
(setq create-lockfiles nil)

(use-package recentf
  :defer 10
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

(use-package ar-file
  :commands (ar/file-open-closest-build-file))
