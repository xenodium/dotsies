;;; -*- lexical-binding: t; -*-
(require 'ar-vsetq)
(require 'ar-csetq)
(require 's)
(require 'dash)

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :validate-custom
  (magit-diff-refine-hunk 'all)
  (magit-status-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
  :config

  (add-to-list 'magit-no-confirm 'stage-all-changes)

  (with-eval-after-load 'fullframe
    (fullframe magit-status magit-mode-quit-window))

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
    (magit-reset-soft "HEAD~1")))

(use-package with-editor
  :ensure t
  :hook ((eshell-mode . with-editor-export-editor)
         (term-exec . with-editor-export-editor)
         (shell-mode . with-editor-export-editor))
  :config
  ;; Requires ~/.hgrc
  ;; [merge-tools]
  ;; emacsclient.args = --eval '(ediff-merge-with-ancestor "$local" "$other" "$base" nil "$output")'
  (setenv "HGMERGE" "emacsclient"))

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
    (let ((buffer (current-buffer)))
      (run-with-idle-timer 3 nil
                           (lambda ()
                             (with-current-buffer buffer
                               (git-gutter-mode +1)))))))

(use-package ar-git
  :defer 2)

(use-package log-edit
  :config
  ;; Let's remember more commit messages.
  (ar/vsetq log-edit-comment-ring (make-ring 1000)))

(use-package git-commit
  :ensure t
  :bind (:map git-commit-mode-map
              ("M-r" . ar/M-r-commit-message-history))
  :init
  (defun ar/M-r-commit-message-history ()
    "Search and insert commit message from history."
    (interactive)
    (insert (completing-read "History: "
                             (-remove
                              (lambda (item)
                                (s-contains-p "Summary: " item))
                              (delete-dups
                               ;; Remove unnecessary newlines (at beg and end).
                               (mapcar (lambda (text)
                                         (string-trim text))
                                       (ring-elements log-edit-comment-ring))))))))

(use-package gitconfig-mode
  :ensure t
  :mode "\\.?gitconfig.?.*\\'")

(use-package gitignore-mode
  :ensure t
  :mode "\\.?gitignore.?.*\\'")

(use-package git-timemachine
  :commands git-timemachine
  :ensure t)
