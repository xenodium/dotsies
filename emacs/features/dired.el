(use-package projectile
    :ensure t
    :defer 2
    :config
    (vsetq projectile-enable-caching t)
    (projectile-mode))

  (use-package dired
    :hook (dired-mode . dired-hide-details-mode)
    :bind (:map global-map
                ("C-l" . dired-jump)
                :map dired-mode-map
                ("j" . dired-next-line)
                ("k" . dired-previous-line)
                ;; Go to parent directory.
                ("^" . ar/file-find-alternate-parent-dir)
                ("RET" . dired-find-file)
                ("P" . peep-dired)
                ("i" . dired-hide-details-mode)
                ("C-l". dired-jump)
                ("M" . ar/dired-mark-all))
    :commands (dired-mode
               ar/find-all-dired-current-dir
               ar/dired-mark-all
               ar/file-find-alternate-parent-dir)
    :init
    (defun ar/file-find-alternate-parent-dir ()
      "Open parent dir."
      (interactive)
      (find-alternate-file ".."))

    (defun ar/dired-mark-all ()
      (interactive)
      (dired-mark-files-regexp ""))

    (defun ar/find-all-dired-current-dir ()
      "Invokes `find-dired' for current dir."
      (interactive)
      (let ((dir (if buffer-file-name
                     (file-name-directory buffer-file-name)
                   ".")))
        (find-dired dir "'(' -name .svn -o -name .git ')' -prune -o -type f")))
    :config
    ;; For dired-jump.
    (use-package dired-x)

    (use-package peep-dired
      :ensure t
      :bind (:map dired-mode-map
                  ("P" . peep-dired)))

    (use-package dired-subtree :ensure t
      :bind (:map dired-mode-map
                  ("<tab>" . dired-subtree-toggle)
                  ("<backtab>" . dired-subtree-cycle)))

    ;; Adding human readable units and sorted by date.
    (validate-setq dired-listing-switches "-Alht")

    ;; Try to guess the target directory for operations.
    (validate-setq dired-dwim-target t)

    ;; Enable since disabled by default.
    (put 'dired-find-alternate-file 'disabled nil)

    ;; Automatically refresh dired buffers when contents changes.
    (validate-setq dired-auto-revert-buffer t))
