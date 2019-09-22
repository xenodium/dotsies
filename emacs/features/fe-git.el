(require 'ar-vsetq)
(require 'ar-csetq)
(require 's)
(require 'dash)

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config

  (ar/csetq magit-status-margin
            '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))

  (add-to-list 'magit-no-confirm 'stage-all-changes)

  (with-eval-after-load 'fullframe
    (fullframe magit-status magit-mode-quit-window))

  (defun ar/magit-soft-reset-head~1 ()
    "Soft reset current git repo to HEAD~1."
    (interactive)
    (magit-reset-soft "HEAD~1")))

(use-package with-editor
  :ensure t
  :hook ((eshell-mode . with-editor-export-editor)
         (term-exec . with-editor-export-editor)
         (shell-mode . with-editor-export-editor)))

(use-package git-gutter
  :hook ((prog-mode . ar/delayed-git-gutter-mode)
         (protobuf-mode . git-gutter-mode))
  :ensure t
  :bind (("C-c <up>" . git-gutter:previous-hunk)
         ("C-c <down>" . git-gutter:next-hunk))
  :config
  (ar/vsetq
   git-gutter:handled-backends '(git hg))

  (defun ar/delayed-git-gutter-mode ()
    "Git gutter can take time to load on some repos. Delay enabling."
    (run-with-idle-timer 5 nil #'git-gutter-mode +1)))

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
