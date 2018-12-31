(require 'ar-vsetq)

(use-package helm
  ;; Save current position to mark ring when jumping to a different place
  :hook  (helm-goto-line-before . helm-save-current-pos-to-mark-ring)
  :ensure t
  :config
  (use-package helm-utils)
  (use-package helm-elisp
    :config
    ;; Helm now defaults to 'helm-display-buffer-in-own-frame. Override this behavior.
    (ar/vsetq helm-show-completion-display-function #'helm-default-display-buffer))

  (ar/vsetq helm-scroll-amount 4) ; scroll 4 lines other window using M-<next>/M-<prior>
  (ar/vsetq helm-input-idle-delay 0.01) ; be idle for this many seconds, before updating candidate buffer
  (ar/vsetq helm-split-window-default-side 'below) ;; open helm buffer below.
  (ar/vsetq helm-split-window-in-side-p t)
  (ar/vsetq helm-candidate-number-limit 200)

  (use-package helm-config)

  (use-package helm-imenu
    :defer t
    :config
    (use-package imenu
      :config
      ;; Automatically rescan for imenu changes.
      (set-default 'imenu-auto-rescan t))
    (use-package imenu-anywhere
      :ensure t))

  (defun ar/helm-keyboard-quit-dwim (&optional arg)
    "First time clear miniuffer. Quit thereafter."
    (interactive "P")
    (if (> (length (minibuffer-contents)) 0)
        (call-interactively 'helm-delete-minibuffer-contents)
      (helm-keyboard-quit)))

  :bind (:map helm-map
         ("C-i" . helm-execute-persistent-action) ; make TAB works in terminal
         ("C-z" . helm-select-action) ; list actions using C-z
         ("M-p" . helm-previous-source)
         ("M-n" . helm-next-source)
         ("C-g" . ar/helm-keyboard-quit-dwim)))

;; Differentiate C-i key binding from TAB.
(define-key input-decode-map (kbd "C-i") (kbd "H-i"))

(use-package helm-ag
  :ensure t
  :bind ("H-i" . ar/helm-ag-insert)
  :commands (ar/helm-ag
             ar/helm-ag-insert)
  :config
  (defun ar/helm-ag-insert (arg)
    ;; Helm-ag and insert match.
    (interactive "P")
    (defun ar/insert-candidate (candidate)
      (move-beginning-of-line 1)
      (unless (eolp)
        (kill-line))
      ;; Drop file:line:column. For example:
      ;; arc_hostlink.c:13:2:#include <linux/fs.h>
      ;; => #include <linux/fs.h>
      (insert (replace-regexp-in-string "^[^ ]*:" "" candidate))
      (indent-for-tab-command))
    (let ((helm-source-do-ag (helm-build-async-source "Silver Searcher inserter"
                                                      :init 'helm-ag--do-ag-set-command
                                                      :candidates-process 'helm-ag--do-ag-candidate-process
                                                      :action 'ar/insert-candidate
                                                      :nohighlight t
                                                      :requires-pattern 3
                                                      :candidate-number-limit 9999
                                                      :keymap helm-do-ag-map)))
      (call-interactively #'ar/helm-ag)))

  (defun ar/helm-ag (arg)
    "Helm-ag search remembering last location.  With ARG, forget the last location."
    (interactive "P")
    (defvar ar/helm-ag--default-locaction nil)
    (when (or arg (not ar/helm-ag--default-locaction))
      (ar/vsetq ar/helm-ag--default-locaction
             (read-directory-name "search in: " default-directory nil t)))
    (helm-do-ag ar/helm-ag--default-locaction))


  (cond ((executable-find "rg")
         (ar/vsetq helm-ag-base-command "rg --vimgrep --no-heading --ignore-case"))
        ((executable-find "pt")
         (ar/vsetq helm-ag-base-command "pt -e --nocolor --nogroup"))
        ((executable-find "ag")
         (ar/vsetq helm-ag-base-command "ag --nocolor --nogroup"))
        (t
         (ar/vsetq helm-ag-base-command "ack --nocolor --nogroup"))))
