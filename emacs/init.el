;;; init.el --- Emacs initialization entry point.
;;; Commentary:
;; Just another init.el file.
;;; Code:

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; From https://github.com/purcell/emacs.d/blob/master/lisp/init-utils.el
(if (fboundp 'with-eval-after-load)
    (defalias 'ar/after-load #'with-eval-after-load)
  (defmacro ar/after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

;; Increase memory threshold for garbage collection.
(setq gc-cons-threshold 20000000)

;; Enhanced list-packages replacement.
(use-package paradox :ensure t)

(use-package async :ensure t)
(require 'async-bytecomp)

(use-package molokai-theme :ensure t)

(set-cursor-color "#FA009A")

;; Hide UI.
(menu-bar-mode -1)
(when (fboundp 'toggle-scroll-bar)
  (toggle-scroll-bar -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
;; Avoid native dialogs when running graphical.
(when (boundp 'use-dialog-box)
  (setq use-dialog-box nil))

(use-package fullframe :ensure t)

(ar/after-load 'magit
  (fullframe magit-status magit-mode-quit-window))

(ar/after-load 'paradox
  (fullframe paradox-list-packages paradox-quit-and-close))

(ar/after-load 'ibuffer
  (fullframe ibuffer ibuffer-quit))

(ar/after-load 'dired
  (fullframe dired quit-window))

;; Based on http://www.lunaryorn.com/2014/07/26/make-your-emacs-mode-line-more-useful.html
(defvar ac/vc-mode-line
  '(" " (:propertize
         ;; Strip the backend name from the VC status information
         (:eval (let ((backend (symbol-name (vc-backend (buffer-file-name)))))
                  (substring vc-mode (+ (length backend) 2))))
         face font-lock-variable-name-face))
  "Mode line format for VC Mode.")
(put 'ac/vc-mode-line 'risky-local-variable t)

;; Customizing mode line.
;; Based on http://emacs-fu.blogspot.co.uk/2011/08/customizing-mode-line.html
(setq-default mode-line-format
              (list
               ;;"★ "
               "✪ "
               ;; the buffer name; the file name as a tool tip
               '(:eval (propertize "%b"
                                   'face 'font-lock-keyword-face
                                   'help-echo (buffer-file-name)))

               '(vc-mode ac/vc-mode-line)

               " | "
               ;; line and column, '%02' to set to 2 chars at least
               ;; prevents flickering
               (propertize "%02l" 'face 'font-lock-type-face)
               ","
               (propertize "%02c" 'face 'font-lock-type-face)
               " | "

               ;; relative position, size of file
               (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
               "/"
               (propertize "%I" 'face 'font-lock-constant-face) ;; size
               " | "

               ;; the current major mode for the buffer.
               '(:eval (propertize "%m"
                                   'face
                                   'font-lock-string-face
                                   'help-echo buffer-file-coding-system))
               " | "


               ;; insert vs overwrite mode, input-method in a tooltip
               '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                                   'face 'font-lock-preprocessor-face
                                   'help-echo (concat "Buffer is in "
                                                      (if overwrite-mode "overwrite" "insert") " mode")))

               ;; was this buffer modified since the last save?
               '(:eval (when (buffer-modified-p)
                         (concat ","  (propertize "Mod"
                                                  'face 'font-lock-warning-face
                                                  'help-echo "Buffer has been modified"))))

               ;; is this buffer read-only?
               '(:eval (when buffer-read-only
                         (concat ","  (propertize "RO"
                                                  'face 'font-lock-type-face
                                                  'help-echo "Buffer is read-only"))))
               " | "

               ;; add the time, with the date and the emacs uptime in the tooltip
               '(:eval (propertize (format-time-string "%H:%M")
                                   'help-echo
                                   (concat (format-time-string "%c; ")
                                           (emacs-uptime "Uptime:%hh"))))
               ))

;; Set font face height. Value is 1/10pt.
(set-face-attribute 'default nil :height 180)

;; Ensure window is maximized.
(use-package maxframe :ensure t)
(add-hook 'window-setup-hook 'maximize-frame t)

(use-package elfeed :ensure t)
(setq elfeed-feeds
      '(("http://planet.emacsen.org/atom.xml" blog emacs)
        ("http://planet.gnome.org/rss20.xml" blog gnome)
        ("http://sachachua.com/blog/feed" blog sachachua)
        ("http://blog.roteiv.com/atom.xml" blog vietor)
        ("http://reddit.com/r/emacs/.rss" blog reddit)))
;; Start off with elfeed.

(use-package bind-key :ensure t)

;; Enable upcase and downcase region (disabled by default).
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(use-package anchored-transpose :ensure t)

;; From http://pages.sachachua.com/.emacs.d/Sacha.html#sec-1-7-3
;; Transpose stuff with M-t
(bind-key "M-t" nil) ;; which used to be transpose-words
(bind-key "M-t r" #'anchored-transpose)
(bind-key "M-t l" #'transpose-lines)
(bind-key "M-t w" #'transpose-words)
(bind-key "M-t t" #'transpose-words)
(bind-key "M-t M-t" #'transpose-words)
(bind-key "M-t s" #'transpose-sexps)
(bind-key "C-+" #'text-scale-increase)
(bind-key "C--" #'text-scale-decrease)

(use-package hackernews :ensure t)

;; Stack Exchange viewer.
(use-package sx :ensure t)

;; Twitter.
(use-package twittering-mode :ensure t)

(use-package rainbow-delimiters :ensure t)

(use-package hungry-delete :ensure t)
(global-hungry-delete-mode)
(global-font-lock-mode)

(global-auto-revert-mode)
;; Let auto-revert-mode update vc/git info.
;; Need it for mode-line-format to stay up to date.
(setq auto-revert-check-vc-info t)

(use-package expand-region :ensure t)
(global-set-key (kbd "C-c w") #'er/expand-region)

(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode)

(use-package yasnippet :ensure t)
(setq yas-snippet-dirs
      '("~/.emacs.d/yasnippets/personal"))
(yas-reload-all)

(use-package helm
  :init
  (progn
    (use-package helm-ag :ensure t)
    (use-package helm-buffers)
    (use-package helm-files)
    (use-package helm-grep)
    (use-package helm-swoop :ensure t)
    (use-package helm-config)) :ensure t)

(defun ar/projectile-helm-ag ()
  "Search current repo/project using ag."
  (interactive)
  (helm-do-ag (projectile-project-root)))

;; http://stackoverflow.com/questions/6133799/delete-a-word-without-adding-it-to-the-kill-ring-in-emacs
(defun ar/backward-delete-subword (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point)
                 (progn
                   (subword-backward arg)
                   (point))))
(global-set-key (kbd "M-DEL") #'ar/backward-delete-subword)
(global-set-key (kbd "<C-backspace>") #'ar/backward-delete-subword)

(use-package helm-dash :ensure t :demand)
(bind-key "C-h y" #'helm-dash-at-point)
(setq helm-dash-browser-func #'eww)

(global-set-key (kbd "M-C-s") #'helm-multi-swoop-all)
(global-set-key (kbd "C-c i") #'helm-imenu)
(define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") #'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  #'helm-select-action) ; list actions using C-z
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "M-y") #'helm-show-kill-ring)
(global-set-key (kbd "C-x b") #'helm-buffers-list)
(global-set-key (kbd "C-h a") #'helm-apropos)
;; Duplicate line.
(global-set-key "\C-x\C-d" "\C-a\C- \C-e\M-w\C-j\C-y")
;; On Mac, this is effectively fn-M-backspace.
(global-set-key (kbd "M-(") #'kill-word)
(global-set-key (kbd "C-q") #'previous-buffer)
(global-set-key (kbd "C-z") #'next-buffer)

(define-key helm-grep-mode-map (kbd "<return>")  #'helm-grep-mode-jump-other-window)
(define-key helm-grep-mode-map (kbd "n")  #'helm-grep-mode-jump-other-window-forward)
(define-key helm-grep-mode-map (kbd "p")  #'helm-grep-mode-jump-other-window-backward)

(setq
 helm-google-suggest-use-curl-p t
 helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
 helm-quick-update t ; do not display invisible candidates
 helm-idle-delay 0.01 ; be idle for this many seconds, before updating in delayed sources.
 helm-input-idle-delay 0.01 ; be idle for this many seconds, before updating candidate buffer
 helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.

 helm-split-window-default-side 'below ;; open helm buffer below.
 helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window
 helm-buffers-favorite-modes (append helm-buffers-favorite-modes
                                     '(picture-mode artist-mode))
 helm-buffer-max-length nil
 helm-candidate-number-limit 100 ; limit the number of displayed canidates
 helm-M-x-requires-pattern 0     ; show all candidates when set to 0
 helm-boring-file-regexp-list
 '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$") ; do not show these files in helm buffer

 helm-ff-file-name-history-use-recentf t
 helm-move-to-line-cycle-in-source t ; move to end or beginning of source
 ido-use-virtual-buffers t
 helm-buffers-fuzzy-matching t)

;; Save current position to mark ring when jumping to a different place
(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

(helm-mode 1)

(defun ar/helm-do-grep-recursive (&optional non-recursive)
  "Like `helm-do-grep', but greps recursively by default.
Optional argument NON-RECURSIVE to shallow-search."
  (interactive "P")
  (let* ((current-prefix-arg (not non-recursive))
         (helm-current-prefix-arg non-recursive))
    (call-interactively #'helm-do-grep)))

;; ggtags code indexing.
;; https://github.com/leoliu/ggtags
;; https://github.com/leoliu/ggtags/wiki/Install-Global-with-support-for-exuberant-ctags
;; Linux
;; Install exuberant ctags from trunk.
;; Install GNU Global using ./configure --with-exuberant-ctags=PATH_TO_CTAGS_EXECUTABLE
;; Mac OS
;; brew install --HEAD ctags
;; brew install global --with-exuberant-ctags
;; http://writequit.org/org/settings.html#sec-1-26
(use-package ggtags :ensure t)
(use-package helm-gtags :ensure t)
(helm-gtags-mode 1)
(global-set-key (kbd "M-.") #'helm-gtags-dwim)

(use-package projectile :ensure t)
(projectile-global-mode)
(setq projectile-enable-caching t)

;; Best way (so far) to search for files in repo.
(use-package helm-projectile :ensure t
  :bind (("C-x f" . helm-projectile)))

;; Prevent split-window-sensibly to split horizontally.
(setq split-width-threshold nil)

(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
;; ediff-revision cleanup.
;; From http://www.emacswiki.org/emacs/DavidBoon#toc8
(defvar ar/ediff-bwin-config nil
  "Window configuration before ediff.")

(defvar ar/ediff-bwin-reg ?b
  "Register to be set up to hold ar/ediff-bwin-config configuration.")

(defun ar/ediff-bsh ()
  "Function to be called before any buffers or window setup for ediff."
  (remove-hook 'ediff-quit-hook #'ediff-cleanup-mess)
  (window-configuration-to-register ar/ediff-bwin-reg))

(defun ar/ediff-aswh ()
  "Setup hook used to remove the `ediff-cleanup-mess' function.  It causes errors."
  (remove-hook 'ediff-quit-hook #'ediff-cleanup-mess))

(defun ar/ediff-qh ()
  "Function to be called when ediff quits."
  (remove-hook 'ediff-quit-hook #'ediff-cleanup-mess)
  (ediff-cleanup-mess)
  (jump-to-register ar/ediff-bwin-reg))

(add-hook 'ediff-before-setup-hook #'ar/ediff-bsh)
(add-hook 'ediff-after-setup-windows-hook #'ar/ediff-aswh);
(add-hook 'ediff-quit-hook #'ar/ediff-qh)

;; Highlight lines longer than 100 columns.
(require 'whitespace)
(setq whitespace-line-column 100
      whitespace-style '(face lines tabs))
(setq-default whitespace-mode 1)

(defun ar/compile-autoclose (buffer string)
  "Hide successful builds window with BUFFER and STRING."
  (cond ((string-match "finished" string)
         (message "Build finished")
         (run-with-timer 2 nil
                         #'delete-window
                         (get-buffer-window buffer t)))
        (t
         (next-error)
         (message "Compilation exited abnormally: %s" string))))

;; Automatically hide successful builds window.
(setq compilation-finish-functions #'ar/compile-autoclose)

;; Automatically scroll build output.
(setq compilation-scroll-output t)

;; Prevent Extraneous Tabs.
;; From http://www.gnu.org/software/emacs/manual/html_node/eintr/Indent-Tabs-Mode.html
(setq-default indent-tabs-mode nil)

;; Automatically closes brackets.
(electric-pair-mode)
;; Additional electric pairs.
(setq electric-pair-pairs '((?\{ . ?\})
                            (?\< . ?\>)))
(electric-indent-mode)

;; Highlight matching parenthesis.
(show-paren-mode)
;; Highlight entire bracket expression.
(setq show-paren-style 'mixed)

;; Partially use path in buffer name.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Get rid of splash screens.
;; From http://www.emacswiki.org/emacs/EmacsNiftyTricks
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)

;; Enabling subword mode (ie. navigate cameCase)
;; From http://www.emacswiki.org/emacs/CamelCase
(global-subword-mode t)

(use-package git-timemachine :ensure t)

(defun ar/setup-tty ()
  "Setup tty frame."
  (unless (window-system)
    ;; Display line numbers.
    (use-package linum :ensure t)
    (set-face-attribute 'linum nil :background "#1B1D1E")
    (setq linum-format "%4d ")
    (use-package git-gutter :ensure t)
    (global-git-gutter-mode +1)
    (git-gutter:linum-setup)
    (global-set-key (kbd "C-c <up>") #'git-gutter:previous-hunk)
    (global-set-key (kbd "C-c <down>") #'git-gutter:next-hunk)))
(ar/setup-tty)

(set-face-attribute 'fringe nil :background "#1B1D1E")

(defun ar/setup-git-fringe ()
  "Setup git fringe (works in display mode only)."
  (use-package linum :ensure t)
  (global-linum-mode t)
  (use-package git-gutter+ :ensure t)
  (use-package fringe-helper :ensure t)
  (use-package git-gutter-fringe+ :ensure t)
  (global-git-gutter+-mode t)
  (set-face-foreground 'git-gutter-fr+-modified "yellow")
  (set-face-foreground 'git-gutter-fr+-added "green")
  (set-face-foreground 'git-gutter-fr+-deleted "red")
  (global-set-key (kbd "C-c <up>") #'git-gutter+-previous-hunk)
  (global-set-key (kbd "C-c <down>") #'git-gutter+-next-hunk))

;; TODO: Revisit this.
(defun ar/setup-graphic-display ()
  "Setup graphic display."
  (when (window-system)
    (use-package linum :ensure t)
    (global-linum-mode t)
    (ar/setup-git-fringe)
    (add-hook 'before-make-frame-hook
              (lambda ()
                (use-package hlinum :ensure t)
                (hlinum-activate)
                (ar/setup-git-fringe)))))

(ar/setup-graphic-display)

;; Handy pop-up messages with git info.
(use-package git-messenger :ensure t)

;; Display column numbers.
(setq-default column-number-mode t)

;; Highlights current line.
(require 'hl-line)
(global-hl-line-mode +1)
;; Set color as current line's background face.
(set-face-background 'hl-line "black")
;; Keep syntax highlighting in the current line.
(set-face-foreground 'highlight nil)


(require 'whitespace)
(setq whitespace-line-column 100)
(setq whitespace-style '(face lines-tail))

;;  From http://doc.rix.si/org/fsem.html
(defun ar/gnulinuxp ()
  "Return t if the system is a GNU/Linux machine, otherwise nil."
  (string-equal system-type "gnu/linux"))

(defun ar/osxp ()
  "Return t if the system is a Mac OS X machine, otherwise nil."
  (string-equal system-type "darwin"))

(defun ar/cygwinp ()
  "Return t if Emacs is running inside of Cygwin on Windows, otherwise nil."
  (string-equal system-type "cygwin"))

(defun ar/windowsp ()
  "Return t if the system is a native Emacs for Windows, otherwise nil."
  (string-equal system-type "windows"))

(defun ar/new-browser-tab-shell-command ()
  "Return new browser tab shell command."
  (cond
   ((ar/osxp)
    "open http://google.com")
   ((ar/gnulinuxp)
    "google-chrome http://google.com")
   (nil)))

(defun ar/new-browser-tab ()
  "Open a new browser tab in the default browser."
  (interactive)
  (let ((command (ar/new-browser-tab-shell-command)))
    (message command)
    (if command
        (shell-command command)
      (message "Unrecognized platform."))))
(global-set-key (kbd "C-x t") #'ar/new-browser-tab)

(defun ar/init-for-osx ()
  "Perform initializations for Mac OS X."
  (when (ar/osxp)
    ;; Sets the command (Apple) key as Meta.
    (setq mac-command-modifier 'meta)
    ;; Sets the option (Apple) key also as Meta.
    (setq mac-option-modifier 'meta)
    (setq exec-path (append exec-path '("~/homebrew/bin")))))
(ar/init-for-osx)

(defun ar/init-for-linux ()
  "Perform initializations for Linux."
  (when (ar/gnulinuxp)
    (setq exec-path (append exec-path '("~/local/bin")))))
(ar/init-for-linux)

;; Disable backup.
;; From: http://anirudhsasikumar.net/blog/2005.01.21.html
(setq backup-inhibited t)

;; Disable auto save.
(setq auto-save-default nil)

;; Disable auto save.
;; From: http://anirudhsasikumar.net/blog/2005.01.21.html
(setq auto-save-default nil)

;; Case-sensitive fold search search (ie. M-/ to autocomplete).
(setq dabbrev-case-fold-search nil)

;; Move buffer file.
;; From: https://sites.google.com/site/steveyegge2/my-dot-emacs-file
(defun ar/move-buffer-file (dir)
  "Move both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir (if (string-match dir "\\(?:/\\|\\\\)$")
                  (substring dir 0 -1)
                dir))
         (newname (concat dir "/" name)))

    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn (copy-file filename newname 1)
             (delete-file filename)
             (set-visited-file-name newname)
             (set-buffer-modified-p nil) t))))

;;  http://scottmcpeak.com/elisp/scott.emacs.el
;;  ------------------- yes-or-no-p ---------------------
;;  There are a number of situations where Emacs wants to ask me a question,
;;  but the answer is always the same (or, it's easy to get the effect of
;;  the other answer afterwards).  The main example is the question:
;;
;;    File foo.txt has changed on disk.  Reread from disk?
;;
;;  This question is annoying because it gets asked while I'm moving around
;;  in a debugger stack trace, and often don't care about the file I happen
;;  to be at (because I want to move past that frame anyway).  Moreover,
;;  my F12 binding lets me re-read files with a single keystroke, so if I
;;  actually *do* want to re-read it's easy to do.

;;  First, I need the original definition of yes-or-no-p so I can call it
;;  after I've replaced its definition.  In case .emacs gets re-read
;;  after startup, avoid losing the original definition.
(if (fboundp 'orig-yes-or-no-p)
    nil        ; it's already bound, don't re-bind
  (fset 'orig-yes-or-no-p (symbol-function 'yes-or-no-p)))

;;  Now, define my version in terms of `orig-yes-or-no-p'.
(defun yes-or-no-p (prompt)
  "Ask user a yes-or-no question.  Return t if answer is yes, nil if no.
This is a wrapper around `orig-yes-or-no'.
Argument PROMPT to check for additional prompt."
  (if (string-match
       ;;  This message is created in lisp/files.el, and there are four
       ;;  variations.  I'm intentionally matching two of them.
       "File .* changed on disk.  Reread from disk"
       prompt)

      ;;  it's that question; the answer is no, but I *do* want to know
      ;;  that it has changed
      (progn (message "Note: File has changed on disk.") nil)

    ;;  it's a different question; for now, just ask me; I'll probably
    ;;  add more patterns to the above as I think of other questions that
    ;;  I don't want asked
    (orig-yes-or-no-p prompt)))

(use-package git-link :ensure t)

(use-package magit :ensure t)
;; Use vc-ediff as default.
(eval-after-load "vc-hooks"
  '(define-key vc-prefix-map "=" #'vc-ediff))
(global-set-key (kbd "C-x g") #'magit-status)
(setq magit-status-buffer-switch-function #'switch-to-buffer)

(defun ar/sort-lines-ignore-case ()
  "Sort region (case-insensitive)."
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively #'sort-lines)))
;; Sort lines (ie. package imports or headers).
(global-set-key (kbd "M-s l") #'ar/sort-lines-ignore-case)

(setq css-indent-offset 2)

(defun ar/open-in-external-app-lambda ()
  "Return a function to open FPATH externally."
  (cond
   ((ar/windowsp)
    (lambda (fPath)
      (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t))))
   ((ar/osxp)
    (lambda (fPath)
      (shell-command (format "open \"%s\"" fPath))))
   ((ar/gnulinuxp)
    (lambda (fPath)
      (let ((process-connection-type nil))
        (start-process "" nil "xdg-open" fPath))))))

;; Based on http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html
(defun ar/open-in-external-app ()
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.

Version 2015-01-26
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'"
  (interactive)
  (let* ((ξfile-list
          (if (string-equal major-mode "dired-mode")
              (dired-get-marked-files)
            (list (buffer-file-name))))
         (ξdo-it-p (if (<= (length ξfile-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when ξdo-it-p
      (mapc (ar/open-in-external-app-lambda) ξfile-list))))
(global-set-key (kbd "C-M-o") #'ar/open-in-external-app)

(setq ring-bell-function 'ignore)

(use-package markdown-mode+ :ensure t)
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(setq display-time-world-list '(("Europe/Paris" "Paris")
                                ("Europe/London" "London")
                                ("America/Los_Angeles" "Los Angeles")))

;; From http://wenshanren.org/?p=298#more-298
(defun ar/edit-current-file-as-root ()
  "Edit the file that is associated with the current buffer as root."
  (interactive)
  (if (buffer-file-name)
      (let ((file (concat "/sudo:root@localhost:" (buffer-file-name))))
        (find-file file))
    (message "Current buffer does not have an associated file.")))

(global-set-key "\M-/" 'hippie-expand)
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-visible
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line))
;; Thank you Sacha Chua.
;; From http://pages.sachachua.com/.emacs.d/Sacha.html#sec-1-4-8
(fset 'yes-or-no-p 'y-or-n-p)

;; From http://www.wisdomandwonder.com/wordpress/wp-content/uploads/2014/03/C3F.html
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode +1)
(setq savehist-save-minibuffer-history +1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

;; From http://pages.sachachua.com/.emacs.d/Sacha.html#sec-1-5-12
(defun ar/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'ar/smarter-move-beginning-of-line)

;; From http://www.reddit.com/r/emacs/comments/25v0eo/you_emacs_tips_and_tricks/chldury
(defun ar/vsplit-last-buffer ()
  "Vertically splitting the screen and open the previous buffer instead of identical buffers."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun ar/hsplit-last-buffer ()
  "Horizontally splitting the screen and open the previous buffer instead of identical buffers."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

(global-set-key (kbd "C-x 2") #'ar/vsplit-last-buffer)
(global-set-key (kbd "C-x 3") #'ar/hsplit-last-buffer)

;; Thank you Bozhidar.
;; From https://github.com/bbatsov/prelude/blob/a52cdc83eeec567b13a8a5719a174dfe294ee739/core/prelude-core.el#L340
(defun ar/swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (if (/= (count-windows) 2)
      (message "You need exactly 2 windows to do this.")
    (let* ((w1 (car (window-list)))
           (w2 (cadr (window-list)))
           (b1 (window-buffer w1))
           (b2 (window-buffer w2))
           (s1 (window-start w1))
           (s2 (window-start w2)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 s2)
      (set-window-start w2 s1)))
  (other-window 1))
(bind-key "C-\\" #'ar/swap-windows)

;; From https://github.com/bbatsov/prelude/blob/a52cdc83eeec567b13a8a5719a174dfe294ee739/core/prelude-core.el#L111
(defun ar/smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

;; Do not auto indent current line when pressing <RET>.
(add-hook 'sgml-mode-hook
          (lambda() (local-set-key (kbd "<RET>") #'electric-indent-just-newline)))

(defun ar/smart-open-line (arg)
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.
With a prefix ARG open line above the current line."
  (interactive "P")
  (if arg
      (ar/smart-open-line-above)
    (progn
      (move-end-of-line nil)
      (newline-and-indent))))

(global-set-key (kbd "C-o") #'ar/smart-open-line)

(use-package ace-jump-mode :ensure t)

(use-package ace-jump-zap :ensure t
  :bind
  (("M-z" . ace-jump-zap-up-to-char-dwim)
   ("C-M-z" . ace-jump-zap-to-char-dwim)))

(use-package ace-window :ensure t
  :bind (("C-x o" . ace-window)))


;; Interactively resize current window.
(use-package windsize :ensure t)
(windsize-default-keybindings)

(use-package auto-dim-other-buffers :ensure t)
(add-hook 'after-init-hook (lambda ()
                             (when (fboundp 'auto-dim-other-buffers-mode)
                               (auto-dim-other-buffers-mode t))))

(use-package key-chord :ensure t)
(key-chord-define-global "jj" #'ace-jump-char-mode)
(key-chord-define-global "jk" #'ace-jump-char-mode)
(key-chord-define-global "jl" #'ace-jump-line-mode)
(key-chord-define-global "xx" #'execute-extended-command)
(key-chord-define-global "kk" #'kill-whole-line)

;; From http://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer
(defun ar/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(key-chord-define-global "JJ" #'ar/switch-to-previous-buffer)
(key-chord-define-global "BB" #'other-window)
(key-chord-mode +1)

;; Needs clang-format installed.
;; See http://blog.hardcodes.de/articles/63/building-clang-format-and-friends-on-osx-mountain-lion
;; See http://clang.llvm.org/docs/ClangFormat.html
(use-package clang-format :ensure t)

(use-package company :ensure t)
(use-package company-quickhelp :ensure t)
(use-package company-c-headers :ensure t)
(setq company-backends (delete 'company-semantic company-backends))
(setq company-minimum-prefix-length 2)
(setq company-idle-delay 0.5)
(setq company-show-numbers t)
(global-company-mode)
(add-to-list 'company-backends 'company-c-headers)
(global-set-key (kbd "<backtab>") #'company-complete)

;; (add-to-list 'load-path
;;              (concat (getenv "HOME") "/.emacs.d/downloads/rtags/src"))
;; (require 'rtags)
;; (require 'company-rtags)
;; (setq rtags-path
;;       (concat (getenv "HOME") "/.emacs.d/downloads/rtags/bin"))
;; (setq company-backends (delete 'company-clang company-backends))
;; (setq company-rtags-begin-after-member-access t)
;; (setq rtags-completions-enabled t)
;; (add-to-list 'company-backends 'company-rtags)
;; (rtags-diagnostics)

(use-package helm-c-yasnippet :ensure t)

(use-package helm-make :ensure t)

;; Make Emacs more discoverable (Handy for dired-mode). Trigger with '?'.
;; http://www.masteringemacs.org/article/discoverel-discover-emacs-context-menus
(use-package discover :ensure t)
(global-discover-mode 1)

(use-package drag-stuff :ensure t)
(global-set-key (kbd "M-<up>") #'drag-stuff-up)
(global-set-key (kbd "M-<down>") #'drag-stuff-down)

;; Avoid creating lock files (ie. .#some-file.el)
(setq create-lockfiles nil)

;; displays hex strings representing colors
(use-package rainbow-mode :ensure t)
(rainbow-mode 1)

;; If eclim is your cup of tea.
;; (require 'eclim)
;; (global-eclim-mode)
;; (custom-set-variables
;;  '(eclim-eclipse-dirs '("~/tools/eclipse"))
;;  '(eclim-executable "~/tools/eclipse/eclim"))
;; (require 'eclimd)
;; (require 'company-emacs-eclim)
;; (company-emacs-eclim-setup)
;; (global-company-mode t)

;; Relies on manual installation (ie. make emaXcode).
;; Enable auto-complete to use emaXcode while generating snippets.
;;(use-package auto-complete
; :ensure t)
;;(load "~/.emacs.d/downloads/emaXcode/emaXcode.el")
;;(require 'emaXcode)

;; From http://sakito.jp/emacs/emacsobjectivec.html#xcode
(defun ar/xc:build ()
  "Tell Xcode to build."
  (interactive)
  (do-applescript
   (format
    (concat
     "tell application \"Xcode\" to activate \r"
     "tell application \"System Events\" \r"
     "     tell process \"Xcode\" \r"
     "          key code 11 using {command down} \r"
     "    end tell \r"
     "end tell \r"
     ))))

(defun ar/xc:run ()
  "Tell Xcode to run."
  (interactive)
  (do-applescript
   (format
    (concat
     "tell application \"Xcode\" to activate \r"
     "tell application \"System Events\" \r"
     "     tell process \"Xcode\" \r"
     "          key code 15 using {command down} \r"
     "    end tell \r"
     "end tell \r"
     ))))

;;  Note: For ycmd.
;;  * No need for global_ycm_extra_conf.py
;;    (Use .ycm_extra_conf.py)
;;  * Needs .ycm_extra_conf.py (pointing to compile_commands.json location)
;;    (See emacs/ycmd/.ycm_extra_conf.py)
;;  * Needs compile_commands.json
;;    (See http://blog.patspam.com/2014/vim-objc-code-completion)
;;  * Add objc-mode to company-ycmd--extended-features-modes in company-ycmd.el
;; (use-package ycmd
;; :ensure t)
;;
;; (use-package company-ycmd
;; :ensure t)
;;
;; (setq ycmd-server-command (list "python"
;;                                 (expand-file-name "~/.emacs.d/downloads/ycmd/ycmd")))
;; (setq ycmd--log-enabled t)

(use-package objc-font-lock :ensure t)
(objc-font-lock-global-mode)
(setq objc-font-lock-background-face nil)

(use-package dummy-h-mode :ensure t)
(add-to-list 'auto-mode-alist '("\\.h\\'" . dummy-h-mode))

(defun ar/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              ;; Disables "required at runtime" warning for cl package.
              (with-no-warnings
                (remove-if-not 'buffer-file-name (buffer-list))))))

;; From: http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer
(defun ar/rename-current-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

;;  From http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
(defun ar/delete-current-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(use-package go-mode :ensure t)
;; Requires gocode daemon. Install with:
;; go get -u github.com/nsf/gocode
;; go get -u code.google.com/p/rog-go/exp/cmd/godef
;; go get -u code.google.com/p/go.tools/cmd/goimports
;; Useful info at:
;; http://tleyden.github.io/blog/2014/05/22/configure-emacs-as-a-go-editor-from-scratch
;; http://tleyden.github.io/blog/2014/05/27/configure-emacs-as-a-go-editor-from-scratch-part-2
;; http://dominik.honnef.co/posts/2013/03/writing_go_in_emacs
(use-package company-go :ensure t)
(add-hook 'go-mode-hook (lambda ()
                          (helm-dash-activate-docset "Go")
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)
                          (setq tab-width 2 indent-tabs-mode 1)
                          (add-hook 'before-save-hook #'gofmt-before-save)))

(unless (server-running-p)
  (server-start))

;; Customize vertical window divider:
;; Set symbol for the border.
(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?|))

(defun ar/split-camel-region ()
  "Splits camelCaseWord to camel case word."
  (interactive)
  (let ((case-fold-search nil))
    (while (re-search-forward "[A-Z]" (region-end) t)
      (replace-match (format " %s"
                             (downcase (match-string 0)))
                     t nil))))
(global-set-key (kbd "C-c l") #'ar/split-camel-region)

;; M-. elisp navigation.
(use-package elisp-slime-nav :ensure t)

(defun ar/add-functions-to-mode-hooks (hook-functions hooks)
  "Add HOOK-FUNCTIONS to mode HOOKS."
  (dolist (hook hooks)
    (dolist (hook-function hook-functions)
      (add-hook hook hook-function))))

(defun ar/emacs-lisp-mode-hook-function ()
  "Called when entering `emacs-lisp-mode'."
  (helm-dash-activate-docset "Emacs Lisp")
  (turn-on-elisp-slime-nav-mode))
(ar/add-functions-to-mode-hooks '(ar/emacs-lisp-mode-hook-function)
                                '(emacs-lisp-mode-hook
                                  ielm-mode-hook))

(defun ar/save-point ()
  "Save point to register 9999."
  (interactive)
  (point-to-register 9999)
  )

(defun ar/jump-to-saved-point ()
  "Jumps cursor to register 9999's value."
  (interactive)
  (jump-to-register 9999))
(global-set-key (kbd "C-c `") #'ar/jump-to-saved-point)

(defun ar/after-prog-mode-text-change (beg end len)
  "Execute for all text modifications in `prog-mode'.
Argument BEG beginning.
Argument END end.
Argument LEN Length."
  ;; Saving point to register enables jumping back to last change at any time.
  (ar/save-point))

(defun ar/select-current-block ()
  "Select the current block of text between blank lines.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2015-02-07."
  (interactive)
  (let (p1 p2)
    (if (re-search-backward "\n[ \t]*\n" nil "move")
        (progn (re-search-forward "\n[ \t]*\n")
               (setq p1 (point)))
      (setq p1 (point)))
    (if (re-search-forward "\n[ \t]*\n" nil "move")
        (progn (re-search-backward "\n[ \t]*\n")
               (setq p2 (point)))
      (setq p2 (point)))
    (set-mark p1)))

(defun ar/sort-current-block ()
  "Select and sort current block."
  (interactive)
  (ar/select-current-block)
  (ar/sort-lines-ignore-case))

(global-set-key (kbd "M-s b") #'ar/sort-current-block)

(defun ar/sort-objc-headers ()
  "Alphabetically sort Objective-C headers."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^#\\(include\\|import\\).*\n\n" nil t)
      (goto-char (match-beginning 0))
      (ar/sort-current-block))))

(defun ar/clang-format-buffer ()
  "Clang format current buffer."
  (interactive)
  (clang-format (point-min)
                (point-max)))

(defun ar/objc-mode-hook-function ()
  "Called when entering `objc-mode'."
  (add-hook 'before-save-hook
            #'ar/clang-format-buffer
            nil
            'make-it-local)
  (add-hook 'before-save-hook
            #'ar/sort-objc-headers
            nil
            'make-it-local)
  (helm-dash-activate-docset "iOS")
  (set (make-local-variable 'company-backends)
       ;; List with multiple back-ends for mutual inclusion.
       '((;;company-ycmd
          company-yasnippet
          company-gtags
          company-dabbrev-code
          company-files)))
  ;;(ycmd-mode)

  ;; List targets with xcodebuild -list
  ;; List SDKS with xcodebuild -sdk -version, for example:
  ;; iPhoneSimulator7.1.sdk - Simulator - iOS 7.1 (iphonesimulator7.1)
  ;; SDKVersion: 7.1
  ;; Path: /Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator7.1.sdk
  ;; PlatformPath: /Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform
  ;; ProductBuildVersion: 11D167
  ;; ProductCopyright: 1983-2014 Apple Inc.
  ;; ProductName: iPhone OS
  ;; ProductVersion: 7.1
  (set (make-local-variable 'compile-command)
       "xcodebuild -sdk iphonesimulator7.1 -target MyTarget")
  (local-set-key (kbd "<f7>") #'ar/xc:build)
  (local-set-key (kbd "<f8>") #'ar/xc:run)
  (key-chord-define (current-local-map) ";;" "\C-e;"))
(add-hook 'objc-mode-hook #'ar/objc-mode-hook-function)

(defun ar/java-mode-hook-function ()
  "Called when entering `java-mode'."
  ;; 2-char indent for java.
  (defvar c-basic-offset)
  (setq c-basic-offset 2)
  ;; 100-column limit for java.
  (set-fill-column 100))

(add-hook 'java-mode-hook #'ar/java-mode-hook-function)

(defun ar/prog-mode-hook-function ()
  "Called when entering all programming modes."
  (add-hook 'after-change-functions
            #'ar/after-prog-mode-text-change
            t t)
  (let ((m prog-mode-map))
    (define-key m [f6] #'recompile))
  ;; Show trailing whitespace.
  (set (make-local-variable 'show-trailing-whitespace) t)
  ;; Spellcheck comments and documentation
  ;; From http://mwolson.org/projects/emacs-config/init.el.html
  (flyspell-prog-mode)
  (whitespace-mode)
  (rainbow-delimiters-mode)
  (linum-mode)
  (centered-cursor-mode)
  ;; Language-aware editing commands. Useful for imenu-menu.
  (semantic-mode 1)
  (yas-minor-mode))

(defun ar/markdown-mode-hook-function ()
  "Called when entering `markdown-mode'."
  (set (make-local-variable 'markdown-indent-on-enter) nil)
  (local-set-key (kbd "RET") #'electric-newline-and-maybe-indent))

(ar/add-functions-to-mode-hooks '(ar/prog-mode-hook-function)
                                '(prog-mode-hook))

(ar/add-functions-to-mode-hooks '(ar/prog-mode-hook-function
                                  ar/markdown-mode-hook-function)
                                '(markdown-mode-hook))

;; Workaround to use centered-cursor-mode in --nw.
(defvar mouse-wheel-mode nil)
(use-package centered-cursor-mode :ensure t)

(defun ar/create-non-existent-directory ()
  "Create a non-existent directory."
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it? " parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions
             #'ar/create-non-existent-directory)

;; Select help window by default.
(setq help-window-select t)

;; No need to confirm killing buffers.
(global-set-key [(control x) (k)] #'kill-this-buffer)

;; Customize shell-pop.
(setq shell-pop-term-shell "/bin/bash")
(setq shell-pop-shell-type '("ansi-term"
                             "*ansi-term*"
                             (lambda
                               nil (ansi-term shell-pop-term-shell))))
(setq shell-pop-window-position "bottom")
;; Do not auto cd to working directory.
(setq shell-pop-autocd-to-working-dir nil)

(global-set-key [f5] #'shell-pop)
(use-package shell-pop :ensure t)

(defun ar/comment-dwim ()
  "Comment current line or region."
  (interactive)
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (when (region-active-p)
      (setq start (save-excursion
                    (goto-char (region-beginning))
                    (beginning-of-line)
                    (point))
            end (save-excursion
                  (goto-char (region-end))
                  (end-of-line)
                  (point))))
    (comment-or-uncomment-region start end)))
(global-set-key (kbd "M-;") #'ar/comment-dwim)

(defun ar/new-file-with-snippet (name extension mode snippet-name &optional interactive-snippet-p)
  "Create file with NAME, EXTENSION, MODE, SNIPPET-NAME, and optional INTERACTIVE-SNIPPET-P."
  (find-file (format "%s%s" name extension))
  (funcall mode)
  (insert snippet-name)
  (yas-expand-from-trigger-key)
  (unless interactive-snippet-p
    (yas-exit-all-snippets)))

(defun ar/new-objc-file ()
  "Create and `yas-expand' Objective-C interface header/implementation files."
  (interactive)
  (let ((interface-name (read-from-minibuffer "Interface name: ")))
    (ar/new-file-with-snippet interface-name
                              ".h"
                              'objc-mode
                              "inter")
    (ar/new-file-with-snippet interface-name
                              ".m"
                              'objc-mode
                              "impl")))

(defun ar/new-blog-post-file ()
  "Create and `yas-expand' Objective-C interface header/implementation files."
  (interactive)
  (let* ((post-title (read-from-minibuffer "Post title: "))
         (post-date (format-time-string "%Y-%m-%d"))
         (post-file-name (format "%s-%s" post-date post-title)))
    (ar/new-file-with-snippet post-file-name
                              ".markdown"
                              'markdown-mode
                              "post"
                              ;; interactive-snippet-p
                              t)))

;; Hide dired details by default.
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
;; Use RET instead of "a" in dired.
(define-key dired-mode-map (kbd "RET") #'dired-find-alternate-file)

(defun ar/dired-cd-to-parent ()
  "Use ^ in dired to cd to parent."
  (interactive)
  (find-alternate-file ".."))
(define-key dired-mode-map (kbd "^") #'ar/dired-cd-to-parent)

(defun ar/find-all-dired-current-dir ()
  "Invokes `find-dired' for current dir."
  (interactive)
  (let ((dir (if buffer-file-name
                 (file-name-directory buffer-file-name)
               ".")))
    (find-dired dir "'(' -name .svn -o -name .git ')' -prune -o -type f")))

;; Quickly undo pop-ups or other window configurations.
(use-package winner :ensure t
  :init (winner-mode 1))
(setq winner-boring-buffers
      (append winner-boring-buffers '("*helm M-x*"
                                      "helm mini*"
                                      "*helm projectile*")))

(use-package helm-descbinds :ensure
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds)))

;;  Guarantee that Emacs never loads outdated byte code files.
(setq load-prefer-newer t)
(use-package auto-compile :ensure t :demand)
(auto-compile-on-load-mode 1)
(auto-compile-on-save-mode 1)

(defun ar/char-upcasep (letter)
  "Check if LETTER is uppercase."
  (eq letter (upcase letter)))

;;  http://oremacs.com/2014/12/25/ode-to-toggle
(defun ar/capitalize-word-toggle ()
  "Capitalize word toggle."
  (interactive)
  (let ((start
         (car
          (bounds-of-thing-at-point 'symbol))))
    (if start
        (save-excursion
          (goto-char start)
          (funcall
           (if (ar/char-upcasep (char-after))
               'downcase-region
             'upcase-region)
           start (1+ start)))
      (capitalize-word -1))))
(global-set-key (kbd "C-c c") #'ar/capitalize-word-toggle)

(defun ar/upcase-word-toggle ()
  "Toggle word case at point."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'symbol))
        beg end
        (regionp
         (if (eq this-command last-command)
             (get this-command 'regionp)
           (put this-command 'regionp nil))))
    (cond
     ((or (region-active-p) regionp)
      (setq beg (region-beginning)
            end (region-end))
      (put this-command 'regionp t))
     (bounds
      (setq beg (car bounds)
            end (cdr bounds)))
     (t
      (setq beg (point)
            end (1+ beg))))
    (save-excursion
      (goto-char (1- beg))
      (and (re-search-forward "[A-Za-z]" end t)
           (funcall (if (ar/char-upcasep (char-before))
                        'downcase-region
                      'upcase-region)
                    beg end)))))
(global-set-key (kbd "C-c u") #'ar/upcase-word-toggle)
(global-set-key (kbd "C-c r") #'set-rectangular-region-anchor)

;; Collaborate with clipboard.
(setq x-select-enable-clipboard t)
;; More expected region behaviour.
(transient-mark-mode t)

;;  Make a shell script executable automatically on save.
;;  From https://github.com/bbatsov/prelude
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;;  Automatically indent yanked code
;;  From http://www.emacswiki.org/emacs/AutoIndentation#toc3
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode '(emacs-lisp-mode
                                     lisp-mode
                                     markdown-mode
                                     clojure-mode
                                     scheme-mode
                                     haskell-mode
                                     ruby-mode
                                     rspec-mode
                                     python-mode
                                     c-mode
                                     c++-mode
                                     objc-mode
                                     latex-mode
                                     plain-tex-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

(global-set-key (kbd "C-x C-r") #'eval-region)

;;  From http://oremacs.com/2015/01/05/youtube-dl
(defun ar/youtube-dowload ()
  "Download youtube video from url in clipboard."
  (interactive)
  (let* ((url (current-kill 0))
         (default-directory "~/Downloads")
         (proc (get-buffer-process (ansi-term "/bin/bash"))))
    (term-send-string
     proc
     (concat "cd ~/Downloads && youtube-dl " url "\n"))))

(defun ar/view-clipboard-buffer ()
  "View clipboard buffer."
  (interactive)
  (let* ((clipboard-content (current-kill 0))
         (clipboard-buffer (get-buffer-create "*Clipboard*")))
    (switch-to-buffer clipboard-buffer)
    (erase-buffer)
    (insert clipboard-content)
    (prog-mode)))
(global-set-key (kbd "C-c y") #'ar/view-clipboard-buffer)

;;  Save Emacs state from one session to another.
;;  Disabling. Trial without it.
;; (desktop-save-mode 1)
;;  Number of buffers to restore immediately.
;; (setq desktop-restore-eager 10)

(defun ar/desktop-save ()
  "Write the desktop save file to ~/.emacs.d."
  (desktop-save (expand-file-name "~/.emacs.d/")))

;; Is this what's locking things up?
;; (run-with-idle-timer 300 t 'ar/desktop-save)

(defun ar/load-all-files (pattern)
  "Load all files found by PATTERN, ie. (ar/load-all-files '~/*.el')."
  (dolist (file (file-expand-wildcards pattern))
    (load file)))

(ar/load-all-files "~/.emacs.d/local/*.el")

(use-package google-translate :ensure t)

;; From http://ergoemacs.org/emacs/emacs_copy_file_path.html
(defun ar/copy-file-path (&optional φdir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
If `universal-argument' is called, copy only the dir path.
Version 2015-01-14
URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'
Optional argument ΦDIR-PATH-ONLY-P if copying buffer directory."
  (interactive "P")
  (let ((fPath
         (if (equal major-mode 'dired-mode)
             default-directory
           (buffer-file-name))))
    (kill-new
     (if (equal φdir-path-only-p nil)
         fPath
       (file-name-directory fPath)))
    (message "File path copied: %s" fPath)))

(defun ar/open-file-at-point ()
  "Open the file path at point.
If there is text selection, uses the text selection for path.
If the path starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
Path may have a trailing “:‹n›” that indicates line number.
If so, jump to that line number.
If path does not have a file extention, automatically try with “.el” for elisp
files.
This command is similar to `find-file-at-point' but without prompting for
confirmation.

URL `http://ergoemacs.org/emacs/emacs_open_file_path_fast.html'"
  (interactive)
  (let ((ξpath (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (let (p0 p1 p2)
                   (setq p0 (point))
                   ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
                   (skip-chars-backward "^  \"\t\n`'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\`")
                   (setq p1 (point))
                   (goto-char p0)
                   (skip-chars-forward "^  \"\t\n`'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\'")
                   (setq p2 (point))
                   (goto-char p0)
                   (buffer-substring-no-properties p1 p2)))))
    (if (string-match-p "\\`https?://" ξpath)
        (browse-url ξpath)
      (progn ; not starting “http://”
        (if (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\'" ξpath)
            (progn
              (let (
                    (ξfpath (match-string 1 ξpath))
                    (ξline-num (string-to-number (match-string 2 ξpath))))
                (if (file-exists-p ξfpath)
                    (progn
                      (find-file ξfpath)
                      (goto-char 1)
                      (forward-line (1- ξline-num)))
                  (progn
                    (when (y-or-n-p (format "File doesn't exist: %s.  Create? " ξfpath))
                      (find-file ξfpath))))))
          (progn
            (if (file-exists-p ξpath)
                (find-file ξpath)
              (if (file-exists-p (concat ξpath ".el"))
                  (find-file (concat ξpath ".el"))
                (when (y-or-n-p (format "File doesn't exist: %s.  Create? " ξpath))
                  (find-file ξpath ))))))))))

(use-package flycheck :ensure t)

(use-package flycheck-pos-tip :ensure t)

;; No Objective-C 'other file' support out of the box. Fix that.
(setq cc-other-file-alist
      `(("\\.cpp$" (".hpp" ".h"))
        ("\\.h$" (".c" ".cpp" ".m" ".mm"))
        ("\\.hpp$" (".cpp" ".c"))
        ("\\.m$" (".h"))
        ("\\.mm$" (".h"))))

(defun ar/find-dired-current-dir ()
  "Find files from current location."
  (interactive)
  (helm-find t))

(use-package multiple-cursors :ensure t)
(multiple-cursors-mode)
(global-set-key (kbd "C-c n") #'mc/mark-next-like-this)
(global-set-key (kbd "C-c a") #'mc/mark-all-like-this)

(defun ar/org-add-cl (cl-number)
  "Add a CL url."
  (interactive "sEnter CL number: ")
  (let ((rendered-cl (format "[[http://cl/%s][cl/%s]]" cl-number cl-number)))
    (insert rendered-cl)))

(defun ar/org-add-bug (bug-number)
  "Add a bug url."
  (interactive "sEnter bug number: ")
  (let ((rendered-cl (format "[[http://b/%s][b/%s]]" bug-number bug-number)))
    (insert rendered-cl)))

(use-package hydra :ensure t)
(setq hydra-is-helpful t)

(defhydra hydra-org-add-object (:color blue)
  "add"
  ("c" ar/org-add-cl "cl")
  ("b" ar/org-add-bug "bug")
  ("q" nil "quit"))

(defhydra hydra-open-c-mode (:color blue)
  "open"
  ("o" ff-find-other-file "other")
  ("e" ar/open-in-external-app "externally")
  ("u" ar/open-file-at-point "url at point")
  ("q" nil "cancel"))

(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key (kbd "C-c o") #'hydra-open-c-mode/body)))

(global-set-key
 (kbd "C-c o")
 (defhydra hydra-open (:color blue)
   "open"
   ("e" ar/open-in-external-app "externally")
   ("u" ar/open-file-at-point "url at point")
   ("q" nil "cancel")))

(global-set-key
 (kbd "C-c s")
 (defhydra hydra-search (:color blue)
   "search"
   ("d" helm-do-ag "search directory")
   ("r" ar/projectile-helm-ag "search repository")
   ("f" ar/find-dired-current-dir "find file")
   ("a" ar/find-all-dired-current-dir "find all files")
   ("q" nil "quit")))

(global-set-key
 (kbd "C-c h")
 (defhydra hydra-hunks (:color amaranth)
   "git hunks"
   ("n" git-gutter+-next-hunk "next")
   ("p" git-gutter+-previous-hunk "previous")
   ("k" git-gutter+-revert-hunk "kill")
   ("d" git-gutter+-popup-hunk "diff")
   ("l" git-messenger:popup-show-verbose "log")
   ("q" nil "quit")))

(defhydra hydra-magit-commit (:color blue)
  "magit commit"
  ("u" (lambda ()
         (interactive)
         (insert "Update.")
         (git-commit-commit)) "update")
  ("r" (lambda ()
         (interactive)
         (insert "Addressing review comments.")
         (git-commit-commit)) "review comments")
  ("q" nil "quit"))

(require 'smerge-mode)
(defhydra hydra-smerge (:color amaranth)
  "git smerge"
  ("n" smerge-next "next")
  ("p" smerge-prev "previous")
  ("m" smerge-keep-mine "keep mine")
  ("o" smerge-keep-other "keep other")
  ("b" smerge-keep-base "keep base")
  ("a" smerge-keep-all "keep all")
  ("q" nil "quit"))

(defun ar/smerge-mode-hook-function ()
  "Called when entering smerge mode."
  (hydra-smerge/body))
(add-hook 'smerge-mode-hook #'ar/smerge-mode-hook-function)

(defun ar/try-smerge ()
  "Activate smerge on conflicts."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))
(add-hook 'find-file-hook #'ar/try-smerge t)

;; Hotspots WIP.
;; (setq ar/helm-source-hotspots '((name . "Hotspots")
;;                                   (candidates . (("yadda" . "/Users/tuco/stuff/active/xenodium.github.dotfiles/emacs/init.el")
;;                                                  ("second" . "/Users/tuco/stuff/active/xenodium.github.dotfiles/emacs/init.el")))
;;                                   (action . (("Open" . (lambda (x) (find-file x)))))))

;; (defun ar/helm-hotspots ()
;;   "Show my hotspots."
;;   (interactive)
;;   (helm :sources '(ar/helm-source-hotspots
;;                    helm-source-buffers-list
;;                    helm-source-ido-virtual-buffers
;;                    helm-source-recentf)))

(require 'profiler)
(defun ar/profiler-start-cpu ()
  "Start cpu profiler."
  (interactive)
  (profiler-start 'cpu))

(global-set-key
 (kbd "C-c 1")
 (defhydra hydra-profile (:color blue)
   "profiling"
   ("b" ar/profiler-start-cpu "begin")
   ("r" profiler-report "report")
   ("e" profiler-stop "end")
   ("q" nil "quit")))

;; (global-set-key
;;  (kbd "C-c y")
;;  (defhydra hydra-root (:color blue)
;;    "cheatsheet"
;;    ("C-c s" hydra-search/body "search")
;;    ("q" nil "quit")))

;; Override default flycheck triggers
(setq flycheck-check-syntax-automatically
      '(save idle-change mode-enabled)
      flycheck-idle-change-delay 0.8)

(setq flycheck-display-errors-function
      #'flycheck-pos-tip-error-messages)

(add-hook 'after-init-hook #'global-flycheck-mode)

;; Prevent inadvertently editing invisible areas in Org.
(setq org-catch-invisible-edits t)

;; All Org leading stars become invisible.
(setq org-hide-leading-stars t)

;; Skip Org's odd indentation levels (1, 3, ...).
(setq org-odd-levels-only t)

;; Disable auto isearch within org-goto.
(setq org-goto-auto-isearch nil)

;; Enable RET to follow Org links.
(setq org-return-follows-link t)

;; From http://emacsredux.com/blog/2015/01/18/clear-comint-buffers/
(defun ar/comint-clear-buffer ()
  "Clear the content of shell/REPL."
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

;; Wonderful weather forecast.
(use-package sunshine :ensure t)
(when (window-system)
  (setq sunshine-show-icons t))
(setq sunshine-units 'metric)
(setq sunshine-location "London, GB")

;; From http://www.reddit.com/r/emacs/comments/2amn1v/isearch_selected_text
(defadvice isearch-mode (around isearch-mode-default-string (forward &optional regexp op-fun recursive-edit word-p) activate)
  "Enable isearch to start with current selection."
  (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
      (progn
        (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
        (deactivate-mark)
        ad-do-it
        (if (not forward)
            (isearch-repeat-backward)
          (goto-char (mark))
          (isearch-repeat-forward)))
    ad-do-it))

;; Open gyp files in prog-mode.
(add-to-list 'auto-mode-alist '("\\.gyp\\'" . prog-mode))

;; TODO: Moving to bottom. Investigate what triggers tramp (and password prompt).
;; C-u magit-status presents list of repositories.
(eval-after-load "projectile"
  '(progn (setq magit-repo-dirs (mapcar (lambda (dir)
                                          (substring dir 0 -1))
                                        ;; Disables "required at runtime" warning for cl package.
                                        (with-no-warnings
                                          (remove-if-not (lambda (project)
                                                           (file-directory-p (concat project "/.git/")))
                                                         (projectile-relevant-known-projects))))
                magit-repo-dirs-depth 1)))

(provide 'init)
;;; init.el ends here
