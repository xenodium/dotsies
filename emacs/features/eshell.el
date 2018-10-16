(require 'ar-vsetq)

(use-package shell-pop
  :ensure t
  :bind (([f5] . ar/shell-pop))
  :config
  (use-package eshell
    :hook ((eshell-mode . goto-address-mode)
           (eshell-mode . ar/eshell-mode-hook-function))
    :init
    (defun ar/eshell-mode-hook-function ()
      (setq-local imenu-generic-expression
                  '(("Prompt" " $ \\(.*\\)" 1)))

      ;; Turn off semantic-mode in eshell buffers.
      (semantic-mode -1)

      (eshell-smart-initialize)

      (setq-local global-hl-line-mode nil)

      (add-to-list 'eshell-visual-commands "ssh")
      (add-to-list 'eshell-visual-commands "tail")
      (add-to-list 'eshell-visual-commands "top")
      (add-to-list 'eshell-visual-commands "htop")
      (add-to-list 'eshell-visual-commands "prettyping")
      (add-to-list 'eshell-visual-commands "ncdu")

      (if (boundp 'ar/alt-mode)
          (setq-local company-backends '((company-async-files)))
        (setq-local company-backends '((company-projectile-cd
                                        company-escaped-files))))

      (company-mode +1)

      ;; comint-magic-space needs to be whitelisted to ensure we receive company-begin events in eshell.
      (when (boundp 'company-begin-commands)
        (setq-local company-begin-commands
                    (append company-begin-commands (list 'comint-magic-space))))

      (bind-key "M-r" #'helm-eshell-history eshell-mode-map)
      (bind-key "C-l" #'ar/eshell-cd-to-parent eshell-mode-map))
    :config
    (require 'company)
    (require 'helm-eshell)


    (require 'company-escaped-files)
    (require 'company-projectile-cd)

    (require 'em-hist)
    (require 'em-glob)

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
          (erase-buffer))))

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

    (defun ar/eshell-cd-to-parent ()
      (interactive)
      (goto-char (point-max))
      (insert "cd ..")
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

  ;; (csetq shell-pop-term-shell "/bin/bash")
  ;; (csetq shell-pop-shell-type '("ansi-term"
  ;;                              "terminal"
  ;;                              (lambda
  ;;                                nil (ansi-term shell-pop-term-shell))))

  ;; Must use custom set for these.
  (csetq shell-pop-window-position "full")
  (csetq shell-pop-shell-type '("eshell" "*eshell*" (lambda ()
                                                      (eshell))))
  (csetq shell-pop-term-shell "eshell")

  (defun ar/shell-pop (shell-pop-autocd-to-working-dir)
    "Shell pop with arg to cd to working dir. Else use existing location."
    (interactive "P")
    ;; shell-pop-autocd-to-working-dir is defined in shell-pop.el.
    ;; Using lexical binding to override.
    (if (string= (buffer-name) shell-pop-last-shell-buffer-name)
        (shell-pop-out)
      (shell-pop-up shell-pop-last-shell-buffer-index))))

(use-package shell
  ;; Mostly for `async-shell-command'.
  :hook (shell-mode . goto-address-mode))
