;;; -*- lexical-binding: t; -*-

(use-package shell-pop
  :ensure t
  :bind (([f5] . ar/shell-pop))
  :validate-custom
  ;; (shell-pop-term-shell "/bin/bash")
  ;; (shell-pop-shell-type '("ansi-term"
  ;;                              "terminal"
  ;;                              (lambda
  ;;                                nil (ansi-term shell-pop-term-shell))))
  (shell-pop-window-position "full")
  (shell-pop-shell-type '("eshell" "*eshell*" (lambda ()
                                                (eshell))))
  (shell-pop-term-shell "eshell")
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

      (eshell/alias "unzip" "atool --extract --explain $1")
      (eshell/alias "clear" "clear t") ;; Clear content too.

      (yas-minor-mode +1)

      ;; comint-magic-space needs to be whitelisted to ensure we receive company-begin events in eshell.
      (when (boundp 'company-begin-commands)
        (setq-local company-begin-commands
                    (append company-begin-commands (list 'comint-magic-space))))

      (bind-keys :map eshell-mode-map
                 ([remap eshell-pcomplete] . completion-at-point)
                 ("C-l" . ar/eshell-cd-to-parent))

      (if (< emacs-major-version 28)
          (bind-keys :map eshell-mode-map
                     ("M-r" . ar/eshell-counsel-history))
        (bind-keys :map eshell-hist-mode-map
                   ("M-r" . ar/eshell-counsel-history))))
    :config
    (require 'counsel)
    ;; Use native 'sudo', system sudo asks for password every time.
    (require 'em-tramp)

    (use-package esh-autosuggest
      :hook (eshell-mode . esh-autosuggest-mode)
      :ensure t)

    (use-package em-hist
      :validate-custom
      (eshell-history-size (* 10 1024))
      (eshell-hist-ignoredups t))

    (defun adviced:eshell-add-input-to-history (orig-fun &rest r)
      "Cd to relative paths aren't that useful in history. Change to absolute paths."
      (require 'seq)
      (let* ((input (nth 0 r))
             (args (progn
                     (set-text-properties 0 (length input) nil input)
                     (split-string input))))
        (if (and (equal "cd" (nth 0 args))
                 (not (seq-find (lambda (item)
                                  ;; Don't rewrite "cd /ssh:" in history.
                                  (string-prefix-p "/ssh:" item))
                                args))
                 (not (seq-find (lambda (item)
                                  ;; Don't rewrite "cd -" in history.
                                  (string-equal "-" item))
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
                 (lambda (name &optional inhibit-buffer-hooks)
                   (when (get-buffer name)
                     (kill-buffer name))
                   (get-buffer-create (generate-new-buffer-name name)))))
        (apply orig-fun r)))

    (advice-add #'eshell-exec-visual
                :around
                #'adviced:eshell-exec-visual)

    (use-package em-glob
      :validate-custom
      (eshell-glob-case-insensitive t)
      (eshell-error-if-no-glob t))

    (when (< emacs-major-version 27)
      (use-package em-banner))

    (use-package em-banner
      :validate-custom
      (eshell-banner-message "
  Welcome to the Emacs

                         _/                  _/  _/
      _/_/      _/_/_/  _/_/_/      _/_/    _/  _/
   _/_/_/_/  _/_/      _/    _/  _/_/_/_/  _/  _/
  _/            _/_/  _/    _/  _/        _/  _/
   _/_/_/  _/_/_/    _/    _/    _/_/_/  _/  _/

"))

    (use-package pcmpl-homebrew
      :ensure t)

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
      :validate-custom
      (eshell-scroll-to-bottom-on-input 'all)
      :config
      ;; https://github.com/atomontage/xterm-color
      (use-package xterm-color
        :ensure t
        :hook (eshell-before-prompt . (lambda ()
                                        (setq xterm-color-preserve-properties t)))
        :config
        (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
        (setenv "TERM" "xterm-256color"))

      (defun eshell/sudo-ec (path)
        (let ((qualified-path (if (string-match "^/" path)
                                  path
                                (concat (expand-file-name (eshell/pwd)) "/" path))))
          (find-file (concat "/sudo::" qualified-path)))))

    (use-package em-dirs
      :validate-custom
      (eshell-list-files-after-cd nil)
      :config
      ;; https://github.com/dakra/dmacs/blob/master/init.org#eshell
      (defun eshell/rcd (&optional directory)
        "Like regular 'cd' but don't jump out of a tramp directory.
When on a remote directory with tramp don't jump 'out' of the server.
So if we're connected with sudo to 'remotehost'
'$ rcd /etc' would go to '/sudo:remotehost:/etc' instead of just
'/etc' on localhost."
        (unless (file-remote-p default-directory)
          (error "not in a remote location"))
        (with-parsed-tramp-file-name default-directory nil
          (eshell/cd
           (tramp-make-tramp-file-name
            method user nil host nil (or directory "") hop)))))

    (use-package em-term
      :validate-custom
      ;; Let buffer linger, but can easily quit since view-mode
      ;; is enabled in term-exec-hook above.
      (eshell-destroy-buffer-when-process-dies nil))

    (use-package em-smart
      :hook
      ((eshell-mode . eshell-smart-initialize)))

    ;; Avoid "WARNING: terminal is not fully functional."
    ;; http://mbork.pl/2018-06-10_Git_diff_in_Eshell
    (setenv "PAGER" "cat")

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

    (defun eshell/emacs (&rest args)
      "Open a file (ARGS) in Emacs.  Some habits die hard."
      (if (null args)
          ;; If I just ran "emacs", I probably expect to be launching
          ;; Emacs, which is rather silly since I'm already in Emacs.
          ;; So just pretend to do what I ask.
          (bury-buffer)
        ;; We have to expand the file names or else naming a directory in an
        ;; argument causes later arguments to be looked for in that directory,
        ;; not the starting directory
        (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))

    (defalias 'eshell/e 'eshell/emacs)
    (defalias 'eshell/ec 'eshell/emacs))

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
