;; Add melpa package repository.
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(require 'use-package)

(use-package async
  :ensure async)
(require 'async-bytecomp)

;; Display line numbers.
(use-package linum
  :ensure linum)
(global-linum-mode)

;; Right-justify linum
;; From https://github.com/echosa/emacs.d#line-numbers
(setq linum-format (lambda (line)
                     (propertize
                      (format (concat "%"
                                      (number-to-string
                                       (length
                                        (number-to-string
                                         (line-number-at-pos
                                          (point-max)))))
                                      "d ")
                              line)
                      'face
                      'linum)))

(use-package molokai-theme
  :ensure molokai-theme)
(set-face-attribute 'linum nil :background "#1B1D1E")
(set-face-attribute 'fringe nil :background "#1B1D1E")
(set-cursor-color "#0087ff")

;; Hide UI.
(menu-bar-mode -1)
(when (fboundp 'toggle-scroll-bar)
  (toggle-scroll-bar -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))


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
(use-package maxframe
  :ensure maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)

(use-package elfeed
  :ensure elfeed)
(setq elfeed-feeds
      '(("http://planet.emacsen.org/atom.xml" blog emacs)
        ("http://planet.gnome.org/rss20.xml" blog gnome)))
;; Start off with elfeed.

(use-package bind-key
  :ensure bind-key)

;; Enable upcase and downcase region (disabled by default).
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(use-package anchored-transpose
  :ensure anchored-transpose)

;; From http://pages.sachachua.com/.emacs.d/Sacha.html#sec-1-7-3
;; Transpose stuff with M-t
(bind-key "M-t" nil) ;; which used to be transpose-words
(bind-key "M-t r" 'anchored-transpose)
(bind-key "M-t l" 'transpose-lines)
(bind-key "M-t w" 'transpose-words)
(bind-key "M-t t" 'transpose-words)
(bind-key "M-t M-t" 'transpose-words)
(bind-key "M-t s" 'transpose-sexps)

;; Alternative to grepping repo.
(bind-key "C-c s r" 'helm-ag-r-from-git-repo)
;; Alternative to grepping from current location.
(bind-key "C-c s d" 'helm-ag-r)

(use-package hackernews
  :ensure hackernews)

;; Stack Exchange viewer.
(use-package sx
  :ensure sx)

(use-package rainbow-delimiters
  :ensure rainbow-delimiters)

(use-package hungry-delete
  :ensure hungry-delete)
(global-hungry-delete-mode)
(global-font-lock-mode)

(global-auto-revert-mode)
;; Let auto-revert-mode update vc/git info.
;; Need it for mode-line-format to stay up to date.
(setq auto-revert-check-vc-info t)

(use-package expand-region
  :ensure expand-region)
(global-set-key (kbd "C-c w") 'er/expand-region)

(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode)

;; Language-aware editing commands. Useful for imenu-menu.
(semantic-mode 1)

(use-package yasnippet
  :ensure yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/yasnippets/personal"))
(yas-reload-all)

(use-package helm
  :ensure helm)

;; From http://tuhdo.github.io/helm-intro.html
;; must set before helm-config,  otherwise helm use default
;; prefix "C-x c", which is inconvenient because you can
;; accidentially pressed "C-x C-c"
(setq helm-command-prefix-key "C-c h")

(require 'helm-config)
(require 'helm-files)
(require 'helm-grep)
(require 'helm-eshell)
(require 'helm-buffers)

(use-package helm-ag-r
  :ensure helm-ag-r)
(require 'helm-ag-r)

(use-package helm-swoop
  :ensure helm-swoop)
(require 'helm-swoop)

(use-package helm-dash
  :ensure helm-dash)
(require 'helm-dash)
(setq helm-dash-browser-func 'eww)

(global-set-key (kbd "M-C-s") 'helm-multi-swoop-all)
(global-set-key (kbd "C-c i") 'helm-imenu)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-c f") 'helm-recentf)
(global-set-key (kbd "C-h ,") 'helm-apropos)
;; Duplicate line.
(global-set-key "\C-x\C-d" "\C-a\C- \C-e\M-w\C-j\C-y")
;; On Mac, this is effectively fn-M-backspace.
(global-set-key (kbd "M-(") 'kill-word)
(global-set-key (kbd "C-q") 'previous-buffer)
(global-set-key (kbd "C-z") 'next-buffer)

(define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
(define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
(define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)

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
 helm-buffer-max-length 30
 helm-candidate-number-limit 200 ; limit the number of displayed canidates
 helm-M-x-requires-pattern 0     ; show all candidates when set to 0
 helm-boring-file-regexp-list
 '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$") ; do not show these files in helm buffer
 helm-ff-file-name-history-use-recentf t
 helm-move-to-line-cycle-in-source t ; move to end or beginning of source
                                        ; when reaching top or bottom of source.
 ido-use-virtual-buffers t      ; Needed in helm-buffers-list
 helm-buffers-fuzzy-matching t          ; fuzzy matching buffer names when non--nil
                                        ; useful in helm-mini that lists buffers
 )

;; Save current position to mark ring when jumping to a different place
(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

(helm-mode 1)

(defun ar/helm-do-grep-recursive (&optional non-recursive)
  "Like `helm-do-grep', but greps recursively by default."
  (interactive "P")
  (let* ((current-prefix-arg (not non-recursive))
         (helm-current-prefix-arg non-recursive))
    (call-interactively 'helm-do-grep)))

;; ggtags code indexing.
;; https://github.com/leoliu/ggtags
;; https://github.com/leoliu/ggtags/wiki/Install-Global-with-support-for-exuberant-ctags
;; Linux
;; Install exuberant ctags from trunk.
;; Install GNU Global using ./configure --with-exuberant-ctags=PATH_TO_CTAGS_EXECUTABLE
;; Mac OS
;; brew install --HEAD ctags
;; brew install global --with-exuberant-ctags
(use-package ggtags
  :ensure ggtags)
(use-package helm-gtags
  :ensure helm-gtags)
(helm-gtags-mode 1)
(global-set-key (kbd "M-.") 'helm-gtags-dwim)

(use-package projectile
  :ensure projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)

;; Best way (so far) to search for files in repo.
(use-package helm-projectile
  :ensure helm-projectile)
(require 'helm-projectile)
(global-set-key (kbd "C-x f") 'helm-projectile)

(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
;; ediff-revision cleanup.
;; From http://www.emacswiki.org/emacs/DavidBoon#toc8
(defvar ar/ediff-bwin-config nil "Window configuration before ediff.")
(defcustom ar/ediff-bwin-reg ?b
  "*Register to be set up to hold `ar/ediff-bwin-config'
    configuration.")

(defun ar/ediff-bsh ()
  "Function to be called before any buffers or window setup for
    ediff."
  (remove-hook 'ediff-quit-hook 'ediff-cleanup-mess)
  (window-configuration-to-register ar/ediff-bwin-reg))

(defun ar/ediff-aswh ()
  "setup hook used to remove the `ediff-cleanup-mess' function.  It causes errors."
  (remove-hook 'ediff-quit-hook 'ediff-cleanup-mess))

(defun ar/ediff-qh ()
  "Function to be called when ediff quits."
  (remove-hook 'ediff-quit-hook 'ediff-cleanup-mess)
  (ediff-cleanup-mess)
  (jump-to-register ar/ediff-bwin-reg))

(add-hook 'ediff-before-setup-hook 'ar/ediff-bsh)
(add-hook 'ediff-after-setup-windows-hook 'ar/ediff-aswh);
(add-hook 'ediff-quit-hook 'ar/ediff-qh)

;; Highlight lines longer than 100 columns.
(require 'whitespace)
(setq whitespace-line-column 100
      whitespace-style '(face lines tabs))
(setq-default whitespace-mode 1)

;; Automatically scroll build output.
(setq compilation-scroll-output t)
;; Automatically hide successful builds window.
(setq compilation-finish-functions 'ar/compile-autoclose)
(defun ar/compile-autoclose (buffer string)
  (cond ((string-match "finished" string)
         (message "Build finished")
         (run-with-timer 2 nil
                         'delete-window
                         (get-buffer-window buffer t)))
        (t
         (next-error)
         (message "Compilation exited abnormally: %s" string))))

;; Prevent Extraneous Tabs.
;; From http://www.gnu.org/software/emacs/manual/html_node/eintr/Indent-Tabs-Mode.html
(setq-default indent-tabs-mode nil)

;; Automatically closes brackets.
(electric-pair-mode)
(electric-indent-mode)

;; Highlight matching parenthesis.
(show-paren-mode)

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

(use-package git-timemachine
  :ensure git-timemachine)

;; Highlight git hunks.
(use-package git-gutter
  :ensure git-gutter)
(global-git-gutter-mode +1)
(git-gutter:linum-setup)
(global-set-key (kbd "C-c <up>") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-c <down>") 'git-gutter:next-hunk)

;; Handy pop-up messages with git info.
(use-package git-messenger
  :ensure git-messenger)

;; Display column numbers.
(setq-default column-number-mode t)

;; Highlights current line.
(require 'hl-line)
(global-hl-line-mode 1)
;; Set color as current line's background face.
(set-face-background 'hl-line "black")
;; Keep syntax highlighting in the current line.
(set-face-foreground 'highlight nil)


(require 'whitespace)
(setq whitespace-line-column 100)
(setq whitespace-style '(face lines-tail))

;; New browser tab.
(cond
 ((string-equal system-type "darwin") ; Mac OS X
    (defun ar/new-browser-tab ()
      "Open a new browser tab in the default browser."
      (interactive)
      (shell-command "open http://google.com"))
    ;; Ensures PATH is loaded from shell.
    (use-package exec-path-from-shell
      :ensure exec-path-from-shell)
    (exec-path-from-shell-initialize))
 ((string-equal system-type "gnu/linux") ; Linux
    (defun ar/new-browser-tab ()
      "Open a new browser tab in the default browser."
      (interactive)
      (shell-command "google-chrome http://google.com")
      ))
 )
(global-set-key (kbd "C-x t") 'ar/new-browser-tab)

;; Disable backup.
;; From: http://anirudhsasikumar.net/blog/2005.01.21.html
(setq backup-inhibited t)
; Disable auto save.
(setq auto-save-default nil)

;; Disable auto save.
;; From: http://anirudhsasikumar.net/blog/2005.01.21.html
(setq auto-save-default nil)

;; Case-sensitive fold search search (ie. M-/ to autocomplete).
(setq dabbrev-case-fold-search nil)

;; Rename file and buffer.
;; From: https://sites.google.com/site/steveyegge2/my-dot-emacs-file
(defun ar/rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME." (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn  (rename-file name new-name 1)
                (rename-buffer new-name)
                (set-visited-file-name new-name)
                (set-buffer-modified-p nil))))))

;; Move buffer file.
;; From: https://sites.google.com/site/steveyegge2/my-dot-emacs-file
(defun ar/move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
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

; http://scottmcpeak.com/elisp/scott.emacs.el
; ------------------- yes-or-no-p ---------------------
; There are a number of situations where Emacs wants to ask me a question,
; but the answer is always the same (or, it's easy to get the effect of
; the other answer afterwards).  The main example is the question:
;
;   File foo.txt has changed on disk.  Reread from disk?
;
; This question is annoying because it gets asked while I'm moving around
; in a debugger stack trace, and often don't care about the file I happen
; to be at (because I want to move past that frame anyway).  Moreover,
; my F12 binding lets me re-read files with a single keystroke, so if I
; actually *do* want to re-read it's easy to do.

; First, I need the original definition of yes-or-no-p so I can call it
; after I've replaced its definition.  In case .emacs gets re-read
; after startup, avoid losing the original definition.
(if (fboundp 'orig-yes-or-no-p)
    nil        ; it's already bound, don't re-bind
  (fset 'orig-yes-or-no-p (symbol-function 'yes-or-no-p))
)

; Now, define my version in terms of `orig-yes-or-no-p'.
(defun yes-or-no-p (prompt)
  "Ask user a yes-or-no question.  Return t if answer is yes, nil if no.
This is a wrapper around `orig-yes-or-no'."
  (if (string-match
       ; This message is created in lisp/files.el, and there are four
       ; variations.  I'm intentionally matching two of them.
       "File .* changed on disk.  Reread from disk"
       prompt)

      ; it's that question; the answer is no, but I *do* want to know
      ; that it has changed
      (progn (message "Note: File has changed on disk.") nil)

    ; it's a different question; for now, just ask me; I'll probably
    ; add more patterns to the above as I think of other questions that
    ; I don't want asked
    (orig-yes-or-no-p prompt)
    )
)

(use-package git-link
  :ensure git-link)

(use-package magit
  :ensure magit)
;; Use vc-ediff as default.
(eval-after-load "vc-hooks"
  '(define-key vc-prefix-map "=" 'vc-ediff))
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-status-buffer-switch-function 'switch-to-buffer)

;; Sort lines (ie. package imports or headers).
(global-set-key (kbd "M-s l") 'sort-lines)

(setq css-indent-offset 2)

;; Thank you Xah Lee.
;; from http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html
(defun xah-open-in-external-app (&optional file)
  "Open the current file or dired marked files in external app. The app is chosen from your OS's preference."
  (interactive)
  (let ( doIt
         (myFileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           ((not file) (list (buffer-file-name)))
           (file (list file)))))
    (setq doIt (if (<= (length myFileList) 5)
                   t
                 (y-or-n-p "Open more than 5 files? ") ) )
    (when doIt
      (cond
       ((string-equal system-type "windows-nt")
        (mapc (lambda (fPath) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t)) ) myFileList))
       ((string-equal system-type "darwin")
        (mapc (lambda (fPath) (shell-command (format "open \"%s\"" fPath)) )  myFileList) )
       ((string-equal system-type "gnu/linux")
        (mapc (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath)) ) myFileList) ) ) ) ) )
(global-set-key (kbd "C-M-o") 'xah-open-in-external-app)

(setq ring-bell-function 'ignore)

(use-package ido-vertical-mode
  :ensure ido-vertical-mode)
(ido-vertical-mode)

(use-package markdown-mode+
  :ensure markdown-mode+)
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(setq display-time-world-list '(("Europe/Paris" "Paris")
                                ("Europe/London" "London")
                                ("America/Los_Angeles" "Los Angeles")))

;; From http://wenshanren.org/?p=298#more-298
(defun wenshan-edit-current-file-as-root ()
  "Edit the file that is associated with the current buffer as root"
  (interactive)
  (if (buffer-file-name)
      (progn
        (setq file (concat "/sudo:root@localhost:" (buffer-file-name)))
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
(defun sacha/smarter-move-beginning-of-line (arg)
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
                'sacha/smarter-move-beginning-of-line)

;; From https://github.com/itsjeyd/emacs-config/blob/emacs24/init.el
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; From http://www.reddit.com/r/emacs/comments/25v0eo/you_emacs_tips_and_tricks/chldury
(defun vsplit-last-buffer ()
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer)
  )
(defun hsplit-last-buffer ()
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer)
  )

(global-set-key (kbd "C-x 2") 'vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'hsplit-last-buffer)

;; Thank you Bozhidar.
;; From https://github.com/bbatsov/prelude/blob/a52cdc83eeec567b13a8a5719a174dfe294ee739/core/prelude-core.el#L340
(defun prelude-swap-windows ()
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
(bind-key "C-\\" 'prelude-swap-windows)

;; From https://github.com/bbatsov/prelude/blob/a52cdc83eeec567b13a8a5719a174dfe294ee739/core/prelude-core.el#L111
(defun prelude-smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

;; Do not auto indent current line when pressing <RET>.
(add-hook 'sgml-mode-hook
          (lambda() (local-set-key (kbd "<RET>") 'electric-indent-just-newline)))

(use-package multiple-cursors
  :ensure multiple-cursors)
(multiple-cursors-mode)
(global-set-key (kbd "C-c n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c a") 'mc/mark-all-like-this)

(defun prelude-smart-open-line (arg)
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.
With a prefix ARG open line above the current line."
  (interactive "P")
  (if arg
      (prelude-smart-open-line-above)
    (progn
      (move-end-of-line nil)
      (newline-and-indent))))

(global-set-key (kbd "C-o") 'prelude-smart-open-line)

(use-package ace-jump-mode
  :ensure ace-jump-mode)
(require 'ace-jump-mode)

(use-package golden-ratio
  :ensure golden-ratio)
(golden-ratio-mode)

(use-package auto-dim-other-buffers
  :ensure auto-dim-other-buffers)
(add-hook 'after-init-hook (lambda ()
                             (when (fboundp 'auto-dim-other-buffers-mode)
                               (auto-dim-other-buffers-mode t))))

(use-package key-chord
  :ensure key-chord)
(require 'key-chord)
(key-chord-define-global "jj" 'ace-jump-char-mode)
(key-chord-define-global "jl" 'ace-jump-line-mode)
(key-chord-define-global "xx" 'execute-extended-command)
(key-chord-define-global "kk" 'kill-whole-line)
;; From http://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer
(defun ar/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(key-chord-define-global "JJ" 'ar/switch-to-previous-buffer)
(key-chord-mode +1)

;; Needs clang-format installed.
;; See http://blog.hardcodes.de/articles/63/building-clang-format-and-friends-on-osx-mountain-lion
;; See http://clang.llvm.org/docs/ClangFormat.html
(use-package clang-format
  :ensure clang-format)

(use-package company
  :ensure company)
(use-package company-c-headers
  :ensure company-c-headers)
(require 'company)
(setq company-backends (delete 'company-semantic company-backends))
(setq company-minimum-prefix-length 2)
(setq company-idle-delay 0.5)
(setq company-show-numbers t)
(global-company-mode)
(add-to-list 'company-backends 'company-c-headers)
(global-set-key (kbd "<backtab>") 'company-complete)

;; Potential company theming.
;; (let ((bg (face-attribute 'default :background)))
;;   (custom-set-faces
;;    `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
;;    `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
;;    `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
;;    `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
;;    `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

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

(use-package helm-c-yasnippet
  :ensure helm-c-yasnippet)
(require 'helm-c-yasnippet)

(use-package helm-make
  :ensure helm-make)

(use-package discover
  :ensure discover)

(use-package drag-stuff
  :ensure drag-stuff)
(global-set-key (kbd "M-<up>") 'drag-stuff-up)
(global-set-key (kbd "M-<down>") 'drag-stuff-down)

;; Avoid creating lock files (ie. .#some-file.el)
(setq create-lockfiles nil)

;; displays hex strings representing colors
(use-package rainbow-mode
  :ensure rainbow-mode)

;; Activate smerge on conflicts.
(defun sm-try-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))
(add-hook 'find-file-hook 'sm-try-smerge t)

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
;;  :ensure auto-complete)
;;(load "~/.emacs.d/downloads/emaXcode/emaXcode.el")
;;(require 'emaXcode)

;; From http://sakito.jp/emacs/emacsobjectivec.html#xcode
(defun ar/xc:build ()
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
;; ycmd currently under development. Disabling for now.
;; (use-package ycmd
;;  :ensure ycmd)
;; (require 'ycmd)
;; (setq company-backends (delete 'company-clang company-backends))
;; (setq company-backends (add-to-list 'company-backends 'company-ycmd))
;; (setq ycmd-server-command (list "python" (expand-file-name "~/.emacs.d/downloads/ycmd/ycmd")))
;; (setq ycmd-extra-conf-whitelist '("~/stuff/active/*"))
;; (setq ycmd--log-enabled t)

;; (use-package company-ycmd
;;  :ensure company-ycmd)
;; (require 'company-ycmd)
;; (add-hook 'objc-mode-hook (lambda ()
;;                             (set (make-local-variable 'company-backends) '(company-ycmd))
;;                             (company-mode)))
;; (company-ycmd-setup)
;; (setq company-backends '(company-ycmd))
;; (company-ycmd-enable-comprehensive-automatic-completion)
(use-package objc-font-lock
  :ensure objc-font-lock)
(objc-font-lock-global-mode)
(setq objc-font-lock-background-face nil)

(add-hook 'objc-mode-hook (lambda ()
                            (set (make-local-variable 'company-backends)
                                 ;; List with multiple back-ends for mutual inclusion.
                                 '((company-yasnippet
                                    company-gtags
                                    company-dabbrev-code
                                    company-files)))
                            (company-mode)
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
                            (setq compile-command "xcodebuild -sdk iphonesimulator7.1 -target MyTarget")
                            (define-key objc-mode-map [f6] 'recompile)
                            (define-key objc-mode-map [f7] 'ar/xc:build)
                            (define-key objc-mode-map [f8] 'ar/xc:run)))

;; No Objective-C 'other file' support out of the box. Fix that.
(setq cc-other-file-alist
      `(("\\.cpp$" (".hpp" ".h"))
        ("\\.h$" (".c" ".cpp" ".m" ".mm"))
        ("\\.hpp$" (".cpp" ".c"))
        ("\\.m$" (".h"))
        ("\\.mm$" (".h"))
        ))
(add-hook 'c-mode-common-hook (lambda() (local-set-key (kbd "C-c o") 'ff-find-other-file)))
(use-package dummy-h-mode
  :ensure dummy-h-mode)
(add-to-list 'auto-mode-alist '("\\.h\\'" . dummy-h-mode))

(defun ar/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not 'buffer-file-name (buffer-list)))))

(defun ar/delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(use-package go-mode
  :ensure go-mode)
;; Requires gocode daemon. Install with:
;; go get -u github.com/nsf/gocode
;; go get -u code.google.com/p/rog-go/exp/cmd/godef
;; go get -u code.google.com/p/go.tools/cmd/goimports
;; Useful info at:
;; http://tleyden.github.io/blog/2014/05/22/configure-emacs-as-a-go-editor-from-scratch
;; http://tleyden.github.io/blog/2014/05/27/configure-emacs-as-a-go-editor-from-scratch-part-2
;; http://dominik.honnef.co/posts/2013/03/writing_go_in_emacs
(use-package company-go
  :ensure company-go)
(require 'company-go)
(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)
                          (setq tab-width 2 indent-tabs-mode 1)
                          (add-hook 'before-save-hook 'gofmt-before-save)))

(server-start)

;; Customize vertical window divider:
;; Reverse colors for the border to have nicer line.
(set-face-inverse-video-p 'vertical-border nil)
(set-face-background 'vertical-border (face-background 'default))
;; Set symbol for the border.
(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?|))

(defun ar/split-camel-region ()
  "Splits camelCaseWord to camel case word"
  (interactive)
  (progn (replace-regexp "\\([A-Z]\\)" " \\1" nil (region-beginning)(region-end))
         (downcase-region (region-beginning)(region-end))))
(global-set-key (kbd "C-c l") 'ar/split-camel-region)

;; M-. elisp navigation.
(use-package elisp-slime-nav
  :ensure elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))

(defun ar/save-point ()
  "Saves point to register 9999."
  (interactive)
  (point-to-register 9999)
  )

(defun ar/jump-to-saved-point ()
  "Jumps cursor to register 9999's value."
  (interactive)
  (jump-to-register 9999))
(global-set-key (kbd "C-c `") 'ar/jump-to-saved-point)

(defun ar/after-prog-mode-text-change (beg end len)
  "Executes for all text changes in prog-mode."
  ;; Saving point to register enables jumping back to last change at any time.
  (ar/save-point))

(defun ar/java-mode-hook ()
  "Called when entering java-mode"
  ;; 100-column limit for java.
  (set-fill-column 100)
  ;; 2-char indent for java.
  (setq c-basic-offset 2))
(add-hook 'java-mode-hook 'ar/java-mode-hook)

(defun ar/prog-mode-hook ()
  "Called when entering all programming modes."
  (add-hook 'after-change-functions
            'ar/after-prog-mode-text-change
            t t)
  (let ((m prog-mode-map))
    (define-key m [f6] 'recompile))
  ;; Show trailing whitespace.
  (set (make-local-variable 'show-trailing-whitespace) t)
  ;; Spellcheck comments and documentation
  ;; From http://mwolson.org/projects/emacs-config/init.el.html
  (flyspell-prog-mode)
  (whitespace-mode)
  (rainbow-delimiters-mode)
  (yas-minor-mode))

(add-hook 'prog-mode-hook 'ar/prog-mode-hook)

(use-package centered-cursor-mode
  :ensure centered-cursor-mode)
(global-centered-cursor-mode +1)

(defun ar/create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions
             #'ar/create-non-existent-directory)

;; C-u magit-status presents list of repositories.
(eval-after-load "projectile"
  '(progn (setq magit-repo-dirs (mapcar (lambda (dir)
                                          (substring dir 0 -1))
                                        (remove-if-not (lambda (project)
                                                         (file-directory-p (concat project "/.git/")))
                                                       (projectile-relevant-known-projects)))

                magit-repo-dirs-depth 1)))

;; Select help window by default.
(setq help-window-select t)

;; No need to confirm killing buffers.
(global-set-key [(control x) (k)] 'kill-this-buffer)

;; Customize shell-pop.
(setq shell-pop-term-shell "/bin/bash")
(setq shell-pop-shell-type '("ansi-term"
                             "*ansi-term*"
                             (lambda
                               nil (ansi-term shell-pop-term-shell))))
(setq shell-pop-window-position "bottom")
(global-set-key [f5] 'shell-pop)
(use-package shell-pop
  :ensure shell-pop)

;; Comment current line or region.
(defun ar/comment-dwim ()
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
(global-set-key (kbd "M-;") 'ar/comment-dwim)

(defun ar/new-file-with-template (name extension mode template)
  "Create file with NAME, EXTENSION, MODE, and TEMPLATE"
  (find-file (format "%s%s" name extension))
  (funcall mode)
  (insert template)
  (yas-expand-from-trigger-key)
  (yas-exit-all-snippets))

(defun ar/new-objc-file ()
  "Create and yas-expand Objective-C interface header/implementation files."
  (interactive)
  (let ((interface-name (read-from-minibuffer "Interface name: ")))
    (ar/new-file-with-template interface-name
                               ".h"
                               'objc-mode
                               "inter")
    (ar/new-file-with-template interface-name
                               ".m"
                               'objc-mode
                               "impl")))

;; Hide dired details by default.
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
