;;; -*- lexical-binding: t; -*-

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :validate-custom
  (magit-diff-refine-hunk 'all)
  (magit-status-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
  (magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes)

  ;; Stop asking for GitHub ssh passwords
  (use-package ssh-agency
    :ensure t)

  ;; Open git file at remote/repo location.
  (use-package browse-at-remote
    :ensure t
    :commands
    (ar/browse-at-remote-open-at-github
     browse-at-remote)
    :config
    (transient-append-suffix 'magit-file-dispatch "m"
      '("o" "Browse file" browse-at-remote))
    (transient-replace-suffix 'magit-dispatch "o"
      '("o" "Browse file" browse-at-remote))
    (defalias 'ar/browse-at-remote-open-at-github #'browse-at-remote))

  (use-package git-rebase
    :commands git-rebase-mode
    :bind (:map git-rebase-mode-map
                ("a" . ar/magit-change-commit-author)))

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

  ;; https://gist.github.com/danielmartin/34bc36dafd8f900de483394087230f48
  (defun ar/magit-change-commit-author (arg)
    "Change the commit author during an interactive rebase in Magit.
With a prefix argument, insert a new change commit author command
even when there is already another rebase command on the current
line.  With empty input, remove the change commit author action
on the current line, if any."
    (interactive "P")
    (let ((author
           (magit-transient-read-person "Select a new author for this commit"
                                        nil
                                        nil)))
      (git-rebase-set-noncommit-action
       "exec"
       (lambda (_) (if author
                       (format "git commit --amend --author='%s'" author)
                     ""))
       arg)))

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
    (log-edit-comment-ring (make-ring 1000))))

(use-package igist
  :ensure t
  :commands
  (igist-create-new-gist
   igist-list-gists)
  :custom
  (igist-current-user-name "xenodium")
  (igist-list-format (igist-pick-from-alist
                      '(description
                        public
                        updated_at
                        comments
                        files)
                      (copy-tree igist-default-formats))))

(use-package diff-hl
  :ensure t
  :hook ((prog-mode . diff-hl-mode)
         (protobuf-mode . diff-hl-mode))
  :bind (:map prog-mode-map
         ("C-c <up>" . diff-hl-previous-hunk)
         ("C-c <down>" . diff-hl-next-hunk)
         ;; :map protobuf-mode-map
         ;; ("C-c <up>" . diff-hl-previous-hunk)
         ;; ("C-c <down>" . diff-hl-next-hunk)
         )
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh))

(use-package git-modes
  :ensure t
  :mode (("\\.?gitconfig.?.*\\'" . gitconfig-mode)
         ("\\.?gitignore.?.*\\'" . gitignore-mode)))

(use-package git-timemachine
  :commands git-timemachine
  :ensure t)

(use-package vc-hooks
  :custom
  (vc-handled-backends '(Git))
  :config
  (defun ar/apply-git-patch-from-clipboard-url ()
    "Download and apply patch from clipboard URL to current git repo."
    (interactive)
    (let* ((git-root (vc-git-root default-directory))
           (url (current-kill 0))
           (patch-name (file-name-nondirectory
                        (url-filename
                         (url-generic-parse-url url))))
           (default-directory git-root))
      (unless git-root
        (error "Not in a git repo"))
      (unless (string-prefix-p "http" url)
        (error "Clipboard is not an http URL"))
      (with-temp-buffer
        (url-insert-file-contents url)
        (write-file patch-name))
      (when (eq 0 (shell-command (format "git apply %s" (shell-quote-argument patch-name))))
        (message "Applied %s" patch-name)))))

(use-package diff-mode
  :defer
  :validate-custom
  (diff-font-lock-prettify t))
