;;; ivy.el -*- lexical-binding: t; -*-

(use-package counsel
  :ensure t
  :commands (ar/counsel-ag
             ar/ivy-occur)
  :bind (:map
         global-map
         ("C-c i" . counsel-semantic-or-imenu)
         ("M-y" . counsel-yank-pop)
         ("M-x" . counsel-M-x)
         :map counsel-ag-map
         ("C-c C-e" . ar/ivy-occur))
  :validate-custom
  ;; https://oremacs.com/2017/08/04/ripgrep/
  (counsel-grep-base-command (if (executable-find "rg")
                                 "rg -i -M 120 --no-heading --line-number --color never '%s' %s"
                               counsel-grep-base-command))
  :config
  (push '(counsel-M-x . "") ivy-initial-inputs-alist)
  (push '(counsel-rg . "--glob ** -- ") ivy-initial-inputs-alist)
  (push '(counsel-ag . "--file-search-regex . -- ") ivy-initial-inputs-alist)

  ;; `ar/ivy-occur',`ar/counsel-ag', `ar/wgrep-abort-changes' and `ar/wgrep-finish-edit' replicate a more
  ;; streamlined result-editing workflow I was used to in helm-ag.
  (defvar ar/ivy-occur--win-config nil "Window configuration before ivy occur.")
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
      (setq ar/ivy-occur--win-config
            (current-window-configuration))
      ;; ar addition end.
      (let* ((caller (ivy-state-caller ivy-last))
             (occur-fn (or (plist-get ivy--occurs-list caller)
                           #'ivy--occur-default))
             (buffer
              (generate-new-buffer
               (format "*ivy-occur%s \"%s\"*"
                       (if caller
                           (concat " " (prin1-to-string caller))
                         "")
                       ivy-text))))
        (with-current-buffer buffer
          (funcall occur-fn ivy--old-cands)
          (setf (ivy-state-text ivy-last) ivy-text)
          (setq ivy-occur-last ivy-last)
          ;; ar addition.
          (ivy-wgrep-change-to-wgrep-mode))
        (ivy-exit-with-action
         (lambda (_)
           (pop-to-buffer buffer)
           (setq next-error-last-buffer buffer)
           (setq-local next-error-function #'ivy-occur-next-error)
           ;; ar addition.
           (delete-other-windows))))))

  (defun ar/counsel-ag--strip-insert-item (item)
    "Strip ITEM of file info.
For example:

\"some-file.el:43:  gimme this text only\"
=> \"  gimme this text only\"
"
    (insert (replace-regexp-in-string "^[^:]+:[^:]+:" ""
                                      (if (stringp item)
                                          item
                                        (car item)))))

  (defun ar/counsel-ag (arg)
    (interactive "P")
    (require 'counsel)
    (ivy-set-actions
     'counsel-rg
     `(("i" ,'ar/counsel-ag--strip-insert-item
        "insert")))
    (ivy-set-actions
     'counsel-ag
     `(("i" ,'ar/counsel-ag--strip-insert-item
        "insert")))
    (ivy-set-actions
     'counsel-pt
     `(("i" ,'ar/counsel-ag--strip-insert-item
        "insert")))
    (ivy-set-actions
     'counsel-ack
     `(("i" ,'ar/counsel-ag--strip-insert-item
        "insert")))

    (defvar ar/counsel-ag--default-location nil)
    (when (or arg (not ar/counsel-ag--default-location))
      ;; Prefix consumed by ar/counsel-ag. Avoid counsel-ag from using.
      (setq current-prefix-arg nil)
      (setq ar/counsel-ag--default-location
            (read-directory-name "search in: " default-directory nil t)))

    (let ((kmap counsel-ag-map))
      (define-key kmap (kbd "C-x C-f") (lambda ()
                                         (interactive)
                                         (ivy-quit-and-run
                                           (ar/counsel-ag t))))

      (define-key kmap (kbd "<tab>") (lambda ()
                                       (interactive)
                                       ;; First invocation calls ivy-call.
                                       ;; Second invocation calls ivy-alt-done.
                                       (if (eq this-command last-command)
                                           (ivy-alt-done)
                                         (ivy-call))))
      (cond ((executable-find "rg")
             (counsel-rg nil ar/counsel-ag--default-location))
            ((executable-find "pt")
             (counsel-pt))
            ((executable-find "ag")
             (counsel-ag nil ar/counsel-ag--default-location))
            (t
             (counsel-ack)))))

  (defun ar/wgrep-finish-edit ()
    (interactive)
    (let ((wgrep-auto-save-buffer t))
      (wgrep-finish-edit))
    (quit-window)
    (when ar/ivy-occur--win-config
      (set-window-configuration ar/ivy-occur--win-config))
    (select-window (nth 0 (window-list))))

  (defun ar/wgrep-abort-changes ()
    (interactive)
    (wgrep-abort-changes)
    (quit-window)
    (when ar/ivy-occur--win-config
      (set-window-configuration ar/ivy-occur--win-config))
    (select-window (nth 0 (window-list))))

  (defun adviced:counsel-M-x-action (orig-fun &rest r)
    "Additional support for multiple cursors."
    (apply orig-fun r)
    (let ((cmd (intern (car r))))
      (when (and (boundp 'multiple-cursors-mode)
                 multiple-cursors-mode
                 cmd
                 (not (memq cmd mc--default-cmds-to-run-once))
                 (not (memq cmd mc/cmds-to-run-once))
                 (or mc/always-run-for-all
                     (memq cmd mc--default-cmds-to-run-for-all)
                     (memq cmd mc/cmds-to-run-for-all)
                     (mc/prompt-for-inclusion-in-whitelist cmd)))
        (mc/execute-command-for-all-fake-cursors cmd))))

  (advice-add #'counsel-M-x-action
              :around
              #'adviced:counsel-M-x-action)

  (defun ar/counsel-hacking-with-swift-search ()
    "Ivy interface for dynamically querying hackingwithswift.com."
    (interactive)
    (require 'request)
    (require 'json)
    (require 'url-http)
    (ivy-read "hacking with swift: "
              (lambda (input)
                (or
                 (ivy-more-chars)
                 (let ((request-curl-options (list "-H" (string-trim (url-http-user-agent-string)))))
                   (request
                    "https://www.hackingwithswift.com/example-code/search"
                    :type "GET"
                    :params (list
                             (cons "search" input))
                    :parser 'json-read
                    :success (cl-function
                              (lambda (&key data &allow-other-keys)
                                (ivy-update-candidates
                                 (mapcar (lambda (item)
                                           (let-alist item
                                             (propertize .title 'url .url)))
                                         data)))))
                   0)))
              :action (lambda (selection)
                        (browse-url (concat "https://www.hackingwithswift.com"
                                            (get-text-property 0 'url selection))))
              :dynamic-collection t
              :caller 'ar/counsel-hacking-with-swift-search))

  (defun ar/counsel-apple-search ()
    "Ivy interface for dynamically querying apple.com docs."
    (interactive)
    (require 'request)
    (require 'json)
    (require 'url-http)
    (ivy-read "apple docs: "
              (lambda (input)
                (let* ((url (url-encode-url (format "https://developer.apple.com/search/search_data.php?q=%s" input)))
                       (c1-width (round (* (- (window-width) 9) 0.3)))
                       (c2-width (round (* (- (window-width) 9) 0.5)))
                       (c3-width (- (window-width) 9 c1-width c2-width)))
                  (or
                   (ivy-more-chars)
                   (let ((request-curl-options (list "-H" (string-trim (url-http-user-agent-string)))))
                     (request url
                              :type "GET"
                              :parser 'json-read
                              :success (cl-function
                                        (lambda (&key data &allow-other-keys)
                                          (ivy-update-candidates
                                           (mapcar (lambda (item)
                                                     (let-alist item
                                                       (propertize
                                                        (format "%s   %s   %s"
                                                                (truncate-string-to-width (propertize (or .title "")
                                                                                                      'face '(:foreground "yellow")) c1-width nil ?\s "…")
                                                                (truncate-string-to-width (or .description "") c2-width nil ?\s "…")
                                                                (truncate-string-to-width (propertize (string-join (or .api_ref_data.languages "") "/")
                                                                                                      'face '(:foreground "cyan1")) c3-width nil ?\s "…"))
                                                        'url .url)))
                                                   (cdr (car data)))))))
                     0))))
              :action (lambda (selection)
                        (browse-url (concat "https://developer.apple.com"
                                            (get-text-property 0 'url selection))))
              :dynamic-collection t
              :caller 'ar/counsel-apple-search))

  ;; Smex handles M-x command sorting. Bringing recent commands to the top.
  (use-package smex
    :ensure t)

  ;; Wgrep is used by counsel-ag (to make writeable).
  (use-package wgrep
    :ensure t
    :bind
    (:map wgrep-mode-map
          ("C-c C-c" . ar/wgrep-finish-edit)
          ("C-c C-k" . ar/wgrep-abort-changes)))

  (counsel-mode +1))

(use-package swiper
  :ensure t
  :bind (("C-s" . ar/swiper-isearch-dwim)
         ("C-r" . ar/swiper-isearch-backward-dwim)
         :map swiper-isearch-map
         ("C-r" . ivy-previous-line))
  :config
  (defun ar/swiper-isearch-backward-dwim ()
    (interactive)
    (cond ((and (boundp 'multiple-cursors-mode)
                multiple-cursors-mode
                (fboundp  'phi-search-backward))
           (call-interactively 'phi-search-backward))
          (defining-kbd-macro
            (call-interactively 'isearch-backward))
          (t
           (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
               (let ((region (buffer-substring-no-properties (mark) (point))))
                 (deactivate-mark)
                 (swiper-isearch-backward region))
             (swiper-isearch-backward)))))

  (defun ar/swiper-isearch-dwim ()
    (interactive)
    (cond ((and (boundp 'multiple-cursors-mode)
                multiple-cursors-mode
                (fboundp  'phi-search))
           (call-interactively 'phi-search))
          (defining-kbd-macro
            (call-interactively 'isearch-forward))
          (t
           (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
               (let ((region (buffer-substring-no-properties (mark) (point))))
                 (deactivate-mark)
                 (swiper-isearch region))
             (swiper-isearch))))))

(use-package counsel-projectile
  :ensure t
  :bind ("C-x f" . counsel-projectile-find-file))

(use-package ivy
  :ensure t
  :bind ("C-x b" . ivy-switch-buffer)
  :commands (ar/ivy-bluetooth-connect
             ivy-switch-buffer)
  :validate-custom
  (ivy-initial-inputs-alist '((org-refile . "^")
                              (org-agenda-refile . "^")
                              (org-capture-refile . "^")
                              (Man-completion-table . "^")
                              (woman . "^")))
  (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (ivy-count-format "")
  (ivy-use-virtual-buffers t)
  (ivy-display-style 'fancy)
  (ivy-wrap nil)
  (enable-recursive-minibuffers t)
  :bind (("C-x C-b" . ivy-switch-buffer)
         ("C-c C-r" . ivy-resume)
         :map ivy-minibuffer-map
         ("DEL" . ivy-backward-delete-char)
         ("C-g" . ar/ivy-keyboard-quit-dwim)
         ("C--" . ivy-minibuffer-shrink)
         ("C-+" . ivy-minibuffer-grow))
  :config
  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (setq truncate-lines nil)))
  (add-hook 'window-state-change-functions
            (defun ar/update-ivy-height (&optional _)
              ;; Make ivy-height proportional to frame size.
              (setq ivy-height (round (* 0.01666 (frame-pixel-height))))))

  (defun ar/ivy-reset-height ()
    "Reset ivy height considering frame height."
    (interactive)
    (setq ivy-height (round (* 0.01666 (display-pixel-height)))))

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

  ;; Trying without. May be slowing things down.
  ;; (use-package ivy-rich
  ;;   :ensure t
  ;;   :validate-custom
  ;;   ;; Avoid lag for TRAMP.
  ;;   (ivy-rich-parse-remote-buffer nil)
  ;;   (ivy-rich-display-transformers-list
  ;;    '(counsel-M-x
  ;;      (:columns
  ;;       ((counsel-M-x-transformer (:width 60))  ; the original transfomer
  ;;        (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
  ;;      ivy-switch-buffer
  ;;      (:columns
  ;;       ((ivy-rich-candidate (:width 60))
  ;;        (ivy-rich-switch-buffer-size (:width 7))
  ;;        (ivy-rich-switch-buffer-project (:width 15 :face success)))
  ;;       :predicate
  ;;       (lambda (cand) (get-buffer cand)))))
  ;;   :config
  ;;   (ivy-rich-mode +1))

  ;; Unsure about this one.
  ;; (use-package ivy-posframe
  ;;   :ensure t
  ;;   :config
  ;;   ;; (push '(counsel-M-x . ivy-posframe-display-at-frame-center) ivy-display-functions-alist)
  ;;   ;; (push '(ivy-switch-buffer . ivy-posframe-display-at-frame-center) ivy-display-functions-alist)
  ;;   ;; (push '(t . ivy-posframe-display) ivy-display-functions-alist)
  ;;   (setq ivy-display-function #'ivy-posframe-display)
  ;;   (ivy-posframe-enable))
  )

(use-package ar-counsel-find
  :commands ar/counsel-find)

(use-package ar-ivy-org
  :commands (ar/ivy-org-add-backlog-link
	     ar/ivy-org-add-bookmark-dwim
             ar/ivy-org-my-todos))

(use-package counsel-dash
  :commands counsel-dash
  :ensure t
  :config
  (defun ar/dash-parse-apple-api-query (query)
    "Parse QUERY if recognized as dash-apple-api://. Nil otherwise.

dash-apple-api://load?request_key=hsM5TRxINf#<dash_entry_language=swift><dash_entry_menuDescription=MKMapRect><dash_entry_name=MKMapRect>

'((\"dash_entry_language\" . \"swift\")
  (\"dash_entry_menuDescription\" . \"MKMapRect\")
  (\"dash_entry_name\" . \"MKMapRect\"))"
    (when (s-prefix-p "dash-apple-api://" query)
      (mapcar (lambda (item)
                (let ((values (s-split "=" item)))
                  (cons (nth 0 values) (nth 1 values))))
              (thread-last (nth 1 (s-split "#" query))
                           (s-chop-suffix ">")
                           (s-chop-prefix "<")
                           (s-split "><")))))


  (defun adviced:dash-docs-result-url (orig-fun &rest r)
    "Transforms dash-apple-api:// queries to dash://."
    (let* ((filename (nth 1 r))
           (apple-api-query (ar/dash-parse-apple-api-query filename)))
      (if apple-api-query
          (format "dash://%s" (map-elt apple-api-query "dash_entry_name" nil 'string-equal))
        (apply orig-fun r))))

  (advice-add #'dash-docs-result-url
              :around
              #'adviced:dash-docs-result-url))


(defun ar/counsel-ag (arg)
  (interactive "P")
  (defvar ar/counsel-ag--default-locaction nil)
  (when (or arg (not ar/counsel-ag--default-locaction))
    ;; Prefix consumed by ar/counsel-ag. Avoid counsel-ag from using.
    (setq current-prefix-arg nil)
    (setq ar/counsel-ag--default-locaction
          (read-directory-name "search in: " default-directory nil t)))

  (require 'counsel) ;; counsel-ag-map
  (let ((kmap counsel-ag-map))
    (define-key kmap (kbd "C-x C-f") (lambda ()
                                       (interactive)
                                       (ivy-quit-and-run
                                         (ar/counsel-ag t))))
    (cond ((executable-find "rg")
           (counsel-rg nil ar/counsel-ag--default-locaction))
          ((executable-find "pt")
           (counsel-pt nil ar/counsel-ag--default-locaction))
          ((executable-find "ag")
           (counsel-ag nil ar/counsel-ag--default-locaction))
          (t
           (counsel-ack nil ar/counsel-ag--default-locaction)))))
