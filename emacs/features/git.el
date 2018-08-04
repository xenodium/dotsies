(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes)

  (fullframe magit-status magit-mode-quit-window)

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
  :hook (prog-mode . git-gutter-mode)
  :ensure t
  :bind (("C-c <up>" . git-gutter:previous-hunk)
         ("C-c <down>" . git-gutter:next-hunk)))

(use-package ar-git
  :defer 2)
