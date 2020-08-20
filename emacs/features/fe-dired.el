;;; -*- lexical-binding: t; -*-

(use-package projectile
  :ensure t
  :defer 2
  :ensure-system-package fd
  :validate-custom
  (projectile-dynamic-mode-line nil)
  (projectile-enable-caching t)
  (projectile-completion-system 'ivy)
  ;; Use `hybrid' since `alien' ignores .projectile file, which is
  ;; handy for very large repositories.
  (projectile-indexing-method 'hybrid)
  ;; fd is super fast. Use it if available.
  (projectile-project-root-files-functions
   '(projectile-root-local
     projectile-root-bottom-up))
  :config
  (when (executable-find "fd")
    (let ((fd-command "fd . --print0"))
      (require 'validate)
      (validate-setq projectile-hg-command fd-command)
      (validate-setq projectile-git-command fd-command)
      (validate-setq projectile-fossil-command fd-command)
      (validate-setq projectile-bzr-command fd-command)
      (validate-setq projectile-darcs-command fd-command)
      (validate-setq projectile-svn-command fd-command)
      (validate-setq projectile-generic-command fd-command)))

  (defun adviced:projectile-project-root (orig-fun &rest r)
    "Same as `projectile-project-root' but return nil if remote location (ie. tramp)."
    (defvar adviced:projectile-project-root--cache '())
    (let* ((dir (or (nth 0 r) default-directory))
           (cached-root (map-elt adviced:projectile-project-root--cache
                                 dir)))
      (if (file-remote-p dir)
          nil ;; Always ignore remote (pretend they are not projects)
        (if cached-root
            cached-root
          (setq cached-root (apply orig-fun r))
          (map-put adviced:projectile-project-root--cache dir cached-root)
          cached-root))))

  (advice-add #'projectile-project-root
              :around
              #'adviced:projectile-project-root)

  (projectile-mode))

(use-package dired
  :defer
  :hook (dired-mode . dired-hide-details-mode)
  :bind (:map global-map
              ("C-l" . dired-jump)
              :map dired-mode-map
              ("j" . dired-next-line)
              ("k" . dired-previous-line)
              ;; Go to parent directory.
              ("^" . ar/file-find-parent-dir)
              ("RET" . dired-find-file)
              ("P" . peep-dired)
              ("i" . dired-hide-details-mode)
              ("C-l". dired-jump)
              ("s" . hydra-dired-sort/body)
              ("A" . ar/dired-mark-all)
              ("M" . ar/dired-mark-all))
  :commands (dired-mode
             dired
             ar/find-all-dired-current-dir
             ar/dired-mark-all
             ar/file-find-alternate-parent-dir)
  :validate-custom
  (dired-recursive-copies 'always)
  :config
  (load "~/.emacs.d/features/config-dired"))

(use-package tramp
  :defer 10
  :validate-custom
  ;; Favor .ssh/config instead.
  (tramp-use-ssh-controlmaster-options nil)
  :config
  ;; Use remote PATH on tramp (handy for eshell).
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;; make sure vc stuff is not making tramp slower
  (setq vc-ignore-dir-regexp
	(format "%s\\|%s"
		vc-ignore-dir-regexp
		tramp-file-name-regexp)))
