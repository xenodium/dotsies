;;; ivy.el -*- lexical-binding: t; -*-
(require 'ar-vsetq)

;; Ivy equivalents to Emacs commands.
(use-package counsel
  :ensure t
  :defer 0.1
  :bind (:map
         global-map
         ("C-c i" . counsel-semantic-or-imenu)
         ("M-i" . counsel-grep-or-swiper)
         :map counsel-ag-map
         ("C-c C-e" . ar/ivy-occur)
         :map wgrep-mode-map
         ("C-c C-c" . ar/wgrep-finish-edit)
         ("C-c C-k" . ar/wgrep-abort-changes))
  :init
  ;; `ar/ivy-occur',`ar/counsel-ag', `ar/wgrep-abort-changes' and `ar/wgrep-finish-edit' replicate a more
  ;; streamlined result-editing workflow I was used to in helm-ag.
  (defun ar/ivy-occur ()
    "Stop completion and put the current candidates into a new buffer.

The new buffer remembers current action(s).

While in the *ivy-occur* buffer, selecting a candidate with RET or
a mouse click will call the appropriate action for that candidate.

There is no limit on the number of *ivy-occur* buffers."
    (interactive)
    (if (not (window-minibuffer-p))
        (user-error "No completion session is active")
      ;; ar addition start.
      (defvar ar/ivy-occur--win-config)
      (setq ar/ivy-occur--win-config
            (current-window-configuration))
      ;; ar addition end.
      (let* ((caller (ivy-state-caller ivy-last))
             (occur-fn (plist-get ivy--occurs-list caller))
             (buffer
              (generate-new-buffer
               (format "*ivy-occur%s \"%s\"*"
                       (if caller
                           (concat " " (prin1-to-string caller))
                         "")
                       ivy-text))))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (if occur-fn
                (funcall occur-fn)
              (ivy-occur-mode)
              (insert (format "%d candidates:\n" (length ivy--old-cands)))
              (read-only-mode)
              (ivy--occur-insert-lines
               ivy--old-cands)))
          (setf (ivy-state-text ivy-last) ivy-text)
          (setq ivy-occur-last ivy-last)
          (setq-local ivy--directory ivy--directory)
          ;; ar addition.
          (ivy-wgrep-change-to-wgrep-mode))
        (ivy-exit-with-action
         (lambda (_)
           (pop-to-buffer buffer)
           ;; ar addition.
           (delete-other-windows))))))

  (defun ar/counsel-ag (arg)
    (interactive "P")
    (defvar ar/counsel-ag--default-locaction nil)
    (when (or arg (not ar/counsel-ag--default-locaction))
      ;; Prefix consumed by ar/counsel-ag. Avoid counsel-ag from using.
      (setq current-prefix-arg nil)
      (ar/vsetq ar/counsel-ag--default-locaction
                (read-directory-name "search in: " default-directory nil t)))
    (cond ((executable-find "rg")
           (counsel-rg nil ar/counsel-ag--default-locaction))
          ((executable-find "pt")
           (counsel-pt nil ar/counsel-ag--default-locaction))
          ((executable-find "ag")
           (counsel-ag nil ar/counsel-ag--default-locaction))
          (t
           (counsel-ack nil ar/counsel-ag--default-locaction))))

  (defun ar/wgrep-finish-edit ()
    (interactive)
    (let ((wgrep-auto-save-buffer t))
      (wgrep-finish-edit))
    (quit-window)
    (set-window-configuration ar/ivy-occur--win-config)
    (select-window (nth 0 (window-list))))

  (defun ar/wgrep-abort-changes ()
    (interactive)
    (wgrep-abort-changes)
    (quit-window)
    (set-window-configuration ar/ivy-occur--win-config)
    (select-window (nth 0 (window-list))))

  :config
  ;; Smex handles M-x command sorting. Bringing recent commands to the top.
  (use-package smex
    :ensure t)

  ;; Wgrep is used by counsel-ag (to make writeable).
  (use-package wgrep
    :ensure t)

  ;; https://oremacs.com/2017/08/04/ripgrep/
  (when (executable-find "rg")
    (ar/csetq counsel-grep-base-command
              "rg -i -M 120 --no-heading --line-number --color never '%s' %s"))

  (counsel-mode +1))

(use-package counsel-projectile
  :ensure t
  :bind ("C-x f" . counsel-projectile-find-file))

(use-package ivy
  :ensure t
  :defer 0.1
  :bind (("C-x C-b" . ivy-switch-buffer)
         ("C-c C-r" . ivy-resume)
         :map ivy-minibuffer-map
         ("C-g" . ar/ivy-keyboard-quit-dwim))
  :config
  (ar/vsetq ivy-height 40)
  (ar/vsetq ivy-count-format "")
  (ar/vsetq ivy-use-virtual-buffers t)
  (ar/vsetq enable-recursive-minibuffers t)
  (defun ar/ivy-keyboard-quit-dwim ()
    "If region active, deactivate. If there's content, minibuffer. Otherwise quit."
    (interactive)
    (cond ((and delete-selection-mode (region-active-p))
           (setq deactivate-mark t))
          ((> (length ivy-text) 0)
           (delete-minibuffer-contents))
          (t
           (minibuffer-keyboard-quit))))
  (ivy-mode +1)

  ;; Unsure about this one.
  ;; (use-package ivy-posframe
  ;;   :ensure t
  ;;   :config
  ;;   (setq ivy-display-function #'ivy-posframe-display-at-frame-center)
  ;;   (ivy-posframe-enable))
  )

;; Displays yasnippet previous inline when cycling through results.
(use-package ivy-yasnippet
  :ensure t
  :commands ivy-yasnippet
  :config
  (require 'yasnippet)
  (yas-minor-mode))

(use-package ar-ivy-org
  :commands (ar/ivy-org-add-bookmark
             ar/ivy-org-add-backlog-link
             ar/ivy-org-my-todos))
