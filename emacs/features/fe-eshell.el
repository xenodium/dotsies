;;; -*- lexical-binding: t; -*-
(require 'ar-vsetq)
(require 'ar-csetq)
(require 'seq)

(use-package shell-pop
  :ensure t
  :bind (([f5] . ar/shell-pop))
  :config
  (use-package eshell
    :commands eshell
    :hook ((eshell-mode . goto-address-mode)
           (eshell-mode . ar/eshell-mode-hook-function)
           (eshell-mode . turn-on-hide-mode-line-mode)
           (term-exec . view-mode))
    :init
    (defun ar/eshell-mode-hook-function ()
      (setq-local imenu-generic-expression
                  '(("Prompt" " $ \\(.*\\)" 1)))

      ;; Turn off semantic-mode in eshell buffers.
      (semantic-mode -1)

      (eshell-smart-initialize)
      (eshell-tramp-initialize) ;; su/sudo support.

      (setq-local global-hl-line-mode nil)

      (add-to-list 'eshell-visual-commands "ssh")
      (add-to-list 'eshell-visual-commands "nano")
      (add-to-list 'eshell-visual-commands "tail")
      (add-to-list 'eshell-visual-commands "top")
      (add-to-list 'eshell-visual-commands "htop")
      (add-to-list 'eshell-visual-commands "prettyping")
      (add-to-list 'eshell-visual-commands "ncdu")
      (add-to-list 'eshell-visual-subcommands '("hg" "log" "diff"))

      (setq-local company-backends '((company-yasnippet company-cd company-projectile-cd)))

      (yas-minor-mode +1)
      (company-mode +1)

      ;; comint-magic-space needs to be whitelisted to ensure we receive company-begin events in eshell.
      (when (boundp 'company-begin-commands)
        (setq-local company-begin-commands
                    (append company-begin-commands (list 'comint-magic-space))))

      (bind-keys :map eshell-mode-map
                 ([remap eshell-pcomplete] . completion-at-point)
                 ("C-l" . ar/eshell-cd-to-parent))

      (if (>= emacs-major-version 27)
          ;; eshell-hist-mode-map introduced in Emacs 27.
          (bind-keys :map eshell-hist-mode-map
                     ("M-r" . ar/eshell-counsel-history))
        (bind-keys :map eshell-mode-map
                   ("M-r" . ar/eshell-counsel-history))))
    :config
    (require 'counsel)
    (require 'company-projectile-cd)
    (require 'company-cd)

    (require 'em-hist)

    (defun adviced:eshell-add-input-to-history (orig-fun &rest r)
      "Cd to relative paths aren't that useful in history. Change to absolute paths."
      (let* ((input (nth 0 r))
             (args (progn
                     (set-text-properties 0 (length input) nil input)
                     (split-string input))))
        (if (and (equal "cd" (nth 0 args))
                 (not (seq-find (lambda (item)
                                  (string-prefix-p "/ssh:" item))
                                args)))
            (apply orig-fun (list (format "cd %s"
                                          (expand-file-name (concat default-directory
                                                                    (nth 1 args))))))
          (apply orig-fun r))))

    (advice-add #'eshell-add-input-to-history
                :around
                #'adviced:eshell-add-input-to-history)

    (defun adviced:eshell-exec-visual (orig-fun &rest r)
      ;; Don't let visual commands keep creating multiple buffers.
      ;; Kill it first if it already exists.
      (cl-letf (((symbol-function #'generate-new-buffer)
                 (lambda (name)
                   (when (get-buffer name)
                     (kill-buffer name))
                   (get-buffer-create (generate-new-buffer-name name)))))
        (apply orig-fun r)))

    (advice-add #'eshell-exec-visual
                :around
                #'adviced:eshell-exec-visual)

    (require 'em-glob)

    ;; Use native 'sudo', system sudo asks for password every time.
    (require 'em-tramp)

    (when (< emacs-major-version 27)
      (use-package em-banner))

    (use-package em-banner
      :config
      (ar/csetq eshell-banner-message "
  Welcome to the Emacs

                         _/                  _/  _/
      _/_/      _/_/_/  _/_/_/      _/_/    _/  _/
   _/_/_/_/  _/_/      _/    _/  _/_/_/_/  _/  _/
  _/            _/_/  _/    _/  _/        _/  _/
   _/_/_/  _/_/_/    _/    _/    _/_/_/  _/  _/

"))

    (use-package pcmpl-homebrew
      :ensure t)

    ;; Fringe exit status indicators.
    (use-package eshell-fringe-status
      :ensure t
      :hook ((eshell-mode . eshell-fringe-status-mode)))

    (use-package eshell-up
      :ensure t
      :config
      (defalias 'eshell/up #'eshell-up))

    ;; It's nicer to type (range 0 3) in eshell.
    (defalias 'eshell/range #'number-sequence)
    (defalias 'range #'number-sequence)

    (use-package pcmpl-git
      :ensure t)

    (use-package pcmpl-args
      :ensure t)

    (use-package pcomplete-extension
      :ensure t)

    (use-package shrink-path
      :ensure t)

    (use-package esh-help
      :ensure t
      :config
      ;; Eldoc support.
      (setup-esh-help-eldoc))

    (use-package esh-mode
      :config
      ;; Why is vsetq not finding it?
      (setq eshell-scroll-to-bottom-on-input 'all)

      ;; Override existing clear function. I like this one better.
      ;; Also there's a bug in Emacs 26:
      ;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2018-05/msg00141.html
      (defun eshell/clear (&optional scrollback)
        "Alias to clear (destructive) eshell content."
        (interactive)
        (let ((inhibit-read-only t))
          (erase-buffer)))

      ;; https://github.com/dakra/dmacs/blob/master/init.org#eshell
      (defun eshell/lcd (&optional directory)
        "Like regular 'cd' but don't jump out of a tramp directory.
When on a remote directory with tramp don't jump 'out' of the server.
So if we're connected with sudo to 'remotehost'
'$ lcd /etc' would go to '/sudo:remotehost:/etc' instead of just
'/etc' on localhost."
        (if (file-remote-p default-directory)
            (with-parsed-tramp-file-name default-directory nil
              (eshell/cd
               (tramp-make-tramp-file-name
                method user nil host nil (or directory "") hop)))
          (eshell/cd directory)))

      (defun eshell/sudo-ec (path)
        (let ((qualified-path (if (string-match "^/" path)
                                  path
                                (concat (expand-file-name (eshell/pwd)) "/" path))))
          (find-file (concat "/sudo::" qualified-path)))))

    (use-package em-dirs)
    (use-package em-smart)

    ;; Avoid "WARNING: terminal is not fully functional."
    ;; http://mbork.pl/2018-06-10_Git_diff_in_Eshell
    (setenv "PAGER" "cat")

    (ar/vsetq eshell-where-to-jump 'begin)
    (ar/vsetq eshell-review-quick-commands nil)
    (ar/vsetq eshell-smart-space-goes-to-end t)

    (ar/vsetq eshell-history-size (* 10 1024))
    (ar/vsetq eshell-hist-ignoredups t)
    (ar/vsetq eshell-error-if-no-glob t)
    (ar/vsetq eshell-glob-case-insensitive t)
    (ar/vsetq eshell-list-files-after-cd nil)
    ;; Let buffer linger, but can easily quit since view-mode
    ;; is enabled in term-exec-hook above.
    (ar/csetq eshell-destroy-buffer-when-process-dies nil)

    (defun ar/eshell-counsel-history ()
      (interactive)
      (let ((history eshell-history-ring)
            (selection nil))
        (with-temp-buffer
          (setq eshell-history-ring history)
          (counsel-esh-history)
          (setq selection (buffer-string)))
        (goto-char eshell-last-output-end)
        (delete-region eshell-last-output-end (line-end-position))
        (insert selection)
        (eshell-send-input nil t)))

    (defun ar/eshell-cd-to-parent (projectile-root-p)
      "Change directory to parent. With prefix PROJECTILE-ROOT, change to projectile root dir."
      (interactive "P")
      (goto-char (point-max))
      (insert (if projectile-root-p
                  (projectile-project-root)
                "cd .."))
      (eshell-send-input nil t))

    (defun eshell/a ()
      "Change PWD to active dir."
      (eshell/cd "~/stuff/active"))

    (defun eshell/c ()
      "Change PWD to active dir."
      (eshell/cd "~/stuff/active/code/"))

    (defun eshell/extract (file)
      "One universal command to extract FILE (for bz2, gz, rar, etc.)"
      (eshell-command-result (format "%s %s" (cond ((string-match-p ".*\.tar.bz2" file)
                                                    "tar xzf")
                                                   ((string-match-p ".*\.tar.gz" file)
                                                    "tar xzf")
                                                   ((string-match-p ".*\.bz2" file)
                                                    "bunzip2")
                                                   ((string-match-p ".*\.rar" file)
                                                    "unrar x")
                                                   ((string-match-p ".*\.gz" file)
                                                    "gunzip")
                                                   ((string-match-p ".*\.tar" file)
                                                    "tar xf")
                                                   ((string-match-p ".*\.tbz2" file)
                                                    "tar xjf")
                                                   ((string-match-p ".*\.tgz" file)
                                                    "tar xzf")
                                                   ((string-match-p ".*\.zip" file)
                                                    "unzip")
                                                   ((string-match-p ".*\.jar" file)
                                                    "unzip")
                                                   ((string-match-p ".*\.Z" file)
                                                    "uncompress")
                                                   (t
                                                    (error "Don't know how to extract %s" file)))
                                     file)))


    (use-package ar-eshell-config))

  ;; (ar/csetq shell-pop-term-shell "/bin/bash")
  ;; (ar/csetq shell-pop-shell-type '("ansi-term"
  ;;                              "terminal"
  ;;                              (lambda
  ;;                                nil (ansi-term shell-pop-term-shell))))

  ;; Must use custom set for these.
  (ar/csetq shell-pop-window-position "full")
  (ar/csetq shell-pop-shell-type '("eshell" "*eshell*" (lambda ()
                                                         (eshell))))
  (ar/csetq shell-pop-term-shell "eshell")

  (defun ar/shell-pop (prefix)
    "Shell pop with PREFIX to cd to working dir. Else use existing location."
    (interactive "P")
    ;; Overriding shell-pop-autocd-to-working-dir with `prefix' value.
    (let ((shell-pop-autocd-to-working-dir prefix))
      (if (string= (buffer-name) shell-pop-last-shell-buffer-name)
          (shell-pop-out)
        (shell-pop-up shell-pop-last-shell-buffer-index)))))

(use-package shell
  ;; Mostly for `async-shell-command'.
  :hook (shell-mode . goto-address-mode))

(use-package term
  :bind (:map
         term-mode-map
         ("C-c C-j" . ar/term-toggle-mode)
         ("C-c C-k" . ar/term-toggle-mode)
         :map
         term-raw-map
         ("C-c C-j" . ar/term-toggle-mode)
         ("C-c C-k" . ar/term-toggle-mode))
  :config
  ;; https://joelmccracken.github.io/entries/switching-between-term-mode-and-line-mode-in-emacs-term
  (defun ar/term-toggle-mode ()
    "Toggle term between line mode and char mode."
    (interactive)
    (if (term-in-line-mode)
        (term-char-mode)
      (term-line-mode))))
