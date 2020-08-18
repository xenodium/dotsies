;;; -*- lexical-binding: t; -*-

(add-to-list 'magit-no-confirm 'stage-all-changes)

(use-package git-identity
  :ensure t
  :bind (:map magit-status-mode-map
              ("I" . git-identity-info))
  :custom
  (git-identity-verify t)
  :config
  (git-identity-magit-mode +1))

;; https://github.com/magit/magit/issues/4054
(defun ar/magit-dired-untracked ()
  "Create a dired buffer from the listed untracked files."
  (interactive)
  (require 's)
  (assert (eq major-mode 'magit-status-mode) nil "Not in magit-status-mode")
  (let (beg end files)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^Untracked files ([0-9]+)")
      (forward-char)
      (setq beg (point))
      ;; find the end of the list of Untracked files.
      (re-search-forward "^$")
      (beginning-of-line)
      (backward-char 1)
      (setq end (point))
      (setq files (s-lines (buffer-substring-no-properties beg end))))
    (dired (cons "dired-untracked" files))))

(defun ar/magit-soft-reset-head~1 ()
  "Soft reset current git repo to HEAD~1."
  (interactive)
  (magit-reset-soft "HEAD~1"))

(use-package git-commit
  :ensure t
  :bind (:map git-commit-mode-map
              ("M-r" . ar/M-r-commit-message-history))
  :init
  (defun ar/M-r-commit-message-history ()
    "Search and insert commit message from history."
    (interactive)
    (require 'dash)
    (insert (completing-read "History: "
                             (-remove
                              (lambda (item)
                                (s-contains-p "Summary: " item))
                              (delete-dups
                               ;; Remove unnecessary newlines (at beg and end).
                               (mapcar (lambda (text)
                                         (string-trim text))
                                       (ring-elements log-edit-comment-ring))))))))

(use-package log-edit
  :validate-custom
  ;; Remember more commit messages.
  (log-edit-comment-ring (make-ring 1000)))
