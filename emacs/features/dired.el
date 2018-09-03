(require 'ar-vsetq)
(require 'ar-csetq)

(use-package projectile
  :ensure t
  :defer 2
  :config
  (ar/vsetq projectile-enable-caching t)
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
  (ar/vsetq dired-listing-switches "-Alht")

  ;; Try to guess the target directory for operations.
  (ar/vsetq dired-dwim-target t)

  ;; Enable since disabled by default.
  (put 'dired-find-alternate-file 'disabled nil)

  ;; Automatically refresh dired buffers when contents changes.
  (ar/vsetq dired-auto-revert-buffer t)

  ;; Hide some files
  (setq dired-omit-files "^\\..*$\\|^\\.\\.$")
  (setq dired-omit-mode t))

(use-package openwith
  :ensure t
  :config
  (ar/csetq openwith-associations
            (cond
             ((string-equal system-type "darwin")
              '(("\\.\\(dmg\\|doc\\|docs\\|xls\\|xlsx\\)$"
                 "open" (file))
                ("\\.\\(mp4\\|mp3\\|webm\\|avi\\|flv\\|mov\\)$"
                 "open" ("-a" "VLC" file))))
             ((string-equal system-type "gnu/linux")
              '(("\\.\\(mp4\\|mp3\\|webm\\|avi\\|flv\\|mov\\)$"
                 "xdg-open" (file))))))
  (openwith-mode +1))
