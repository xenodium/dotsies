(require 'ar-vsetq)
(require 'ar-csetq)

(use-package org
  :ensure org-plus-contrib ;; Ensure latest org installed from elpa
  :bind (:map org-mode-map
              ("M-RET" . ar/org-meta-return)
              ("C-x C-q" . view-mode)
              ("C-c C-l" . ar/org-insert-link-dwim)
              ("<" . ar/org-insert-char-dwim))
  :init
  (defun ar/org-mode-hook-function ()
    (toggle-truncate-lines 0)
    (org-display-inline-images)
    (ar/vsetq show-trailing-whitespace t)
    (set-fill-column 1000)
    (use-package ar-org))
  :hook ((org-mode . ar/org-mode-hook-function)
         (org-mode . visual-line-mode)
         (org-mode . yas-minor-mode)
         (org-mode . smartparens-mode))
  :config
  (defun ar/org-meta-return (&optional arg)
    (interactive "P")
    (end-of-line)
    (call-interactively 'org-meta-return))

  (ar/csetq org-todo-keywords
            '((sequence
               "TODO"
               "STARTED"
               "DONE"
               "OBSOLETE"
               "CANCELLED")))

  (ar/csetq org-goto-auto-isearch nil)

  (use-package org-bullets :ensure t
    :hook (org-mode . org-bullets-mode)
    :config
    (ar/vsetq org-bullets-bullet-list
              '("◉" "◎" "⚫" "○" "►" "◇")))

  (use-package org-faces
    :config
    (ar/vsetq org-todo-keyword-faces
              '(("TODO" . (:foreground "red" :weight bold))
                ("STARTED" . (:foreground "yellow" :weight bold))
                ("DONE" . (:foreground "green" :weight bold))
                ("OBSOLETE" . (:foreground "blue" :weight bold))
                ("CANCELLED" . (:foreground "gray" :weight bold)))))

  (defun ar/org-insert-char-dwim ()
    (interactive)
    ;; Display org-insert-structure-template if < inserted at BOL.
    (if (looking-back "^")
        (call-interactively #'org-insert-structure-template)
      (self-insert-command 1)))

  (defun ar/org-insert-link-dwim ()
    "Convert selected region into a link with clipboard http link (if one is found). Default to `org-insert-link' otherwise."
    (interactive)
    (if (and (string-match-p "^http" (current-kill 0))
             (region-active-p))
        (let ((region-content (buffer-substring-no-properties (region-beginning)
                                                              (region-end))))
          (delete-region (region-beginning)
                         (region-end))
          (insert (format "[[%s][%s]]"
                          (current-kill 0)
                          region-content)))
      (call-interactively 'org-insert-link)))

  ;; Look into font-locking email addresses.
  ;; http://kitchingroup.cheme.cmu.edu/blog/category/email/
  ;; (use-package button-lock :ensure t)

  (setq org-refile-targets '((nil . (:regexp . "Week of"))
                             (nil . (:regexp . "RESOLVED"))))

  (ar/vsetq org-ellipsis "…")
  (ar/vsetq org-fontify-emphasized-text t)

  ;; Fontify code in code blocks.
  (ar/vsetq org-src-fontify-natively t)

  ;; When exporting anything, do not insert in kill ring.
  (setq org-export-copy-to-kill-ring nil)

  ;; Display images inline when running in GUI.
  (ar/vsetq org-startup-with-inline-images (display-graphic-p))
  (ar/vsetq org-src-tab-acts-natively t)

  ;; Prevent inadvertently editing invisible areas in Org.
  (ar/vsetq org-catch-invisible-edits 'error)
  (ar/vsetq org-cycle-separator-lines 2)
  (ar/vsetq org-image-actual-width nil)
  (ar/vsetq org-hide-emphasis-markers t)

  ;; All Org leading stars become invisible.
  (ar/vsetq org-hide-leading-stars t)

  ;; Skip Org's odd indentation levels (1, 3, ...).
  (ar/vsetq org-odd-levels-only t)

  ;; Enable RET to follow Org links.
  (ar/vsetq org-return-follows-link t)

  (use-package org-cliplink
    :ensure t)

  ;; Work in progress.
  (use-package webfeeder
    :ensure t
    :config
    (defun ar/blog-date (html-file)
      (with-temp-buffer
        (insert-file-contents html-file)
        (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
               (timestamp (nth 1 (dom-by-class dom "timestamp")))
               (date (parse-time-string (dom-text timestamp))))
          (encode-time (list 0 0 0
                             (nth 3 date)
                             (nth 4 date)
                             (nth 5 date)
                             nil -1 nil)))))

    (defun ar/blog-entry-fpaths (&optional filter)
      (let ((fpaths '())
            (fpath))
        (with-current-buffer (find-file-noselect (expand-file-name
                                                  "~/stuff/active/blog/index.org"))
          (org-element-map (org-element-parse-buffer 'headline)
              'headline
            (lambda (headline)
              (if (org-element-property :CUSTOM_ID headline)
                  (progn
                    (setq fpath (format "%s/index.html" (org-element-property :CUSTOM_ID headline)))
                    (if (and (f-exists-p fpath)
                             (or (not filter)
                                 (string-match-p filter (buffer-substring-no-properties
                                                         (org-element-property :begin headline)
                                                         (org-element-property :end headline)))
                                 ))
                        (add-to-list 'fpaths fpath)
                      (message "Skipping %s" fpath)))
                (message "No custom ID for %s" (ar/org-split-export--parse-headline-title
                                                (org-element-property :raw-value headline)))))))
        fpaths))

    (defun ar/export-blog-feed ()
      (interactive)
      (let ((webfeeder-date-function 'ar/blog-date)
            (default-directory (expand-file-name "~/stuff/active/blog")))
        (webfeeder-build "rss.xml"
                         "."
                         "http://xenodium.com"
                         (ar/blog-entry-fpaths)
                         :title "Alvaro Ramirez's notes"
                         :description "Alvaro's notes from a hacked up org HTML export."
                         :builder 'webfeeder-make-rss)))

    (defun ar/export-blog-emacs-feed ()
      (interactive)
      (let ((webfeeder-date-function 'ar/blog-date)
            (default-directory (expand-file-name "~/stuff/active/blog")))
        (webfeeder-build "emacs/rss.xml"
                         "."
                         "http://xenodium.com"
                         (ar/blog-entry-fpaths "emacs")
                         :title "Alvaro Ramirez's Emacs notes"
                         :description "Alvaro's Emacs notes from a hacked up org HTML export."
                         :builder 'webfeeder-make-rss))))

  (use-package ar-org-blog
    :commands (ar/org-blog-insert-image
               ar/org-blog-insert-resized-image))

  (use-package ar-ox-html
    :commands (ar/org-split-export-async
               ar/org-export-current-headline-async)
    :bind (:map org-mode-map
                ([f6] . ar/ox-export-all-async))
    :config
    (use-package ar-org)
    (use-package ox-html)
    ;; Required by code block syntax highlighting.
    (use-package htmlize
      :ensure t)

    (ar/ox-html-setup)

    (use-package ar-org-split-export))

  (use-package ob
    :bind (:map org-mode-map
                ("C-c C-c" . org-ctrl-c-ctrl-c))
    :config
    (ar/vsetq org-export-babel-evaluate nil)

    ;; We explicitly want org babel confirm evaluations.
    (ar/vsetq org-confirm-babel-evaluate t)

    (use-package ob-objc)
    (use-package ob-swift
      :ensure t)

    (use-package ob-plantuml
      :config
      (use-package org-src)

      ;; Use fundamental mode when editing plantuml blocks with C-c '
      (add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))

      (cond ((string-equal system-type "darwin")
             ;; TODO: Use something like (process-lines "brew" "--prefix" "plantuml").
             (ar/vsetq org-plantuml-jar-path "~/homebrew/Cellar/plantuml/1.2019.5/libexec/plantuml.jar")
             (setenv "GRAPHVIZ_DOT" (expand-file-name "~/homebrew/bin/dot")))
            (t
             (message "Warning: Could not find plantuml.8018.jar")
             (message "Warning: Could not find $GRAPHVIZ_DOT location"))))

    (use-package gnuplot
      :ensure t)

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((R . t)
       (ditaa . t)
       (dot . t)
       (emacs-lisp . t)
       (gnuplot . t)
       (haskell . nil)
       (js . t)
       (objc . t)
       (ocaml . nil)
       (python . t)
       (ruby . t)
       (screen . nil)
       (shell . t)
       (sql . nil)
       (sqlite . t)
       (swift . t))))

  (use-package org-crypt
    :config
    (org-crypt-use-before-save-magic)
    (ar/csetq org-crypt-disable-auto-save nil)
    (ar/csetq org-tags-exclude-from-inheritance (quote ("crypt")))
    ;;  Set to nil to use symmetric encryption.
    (ar/csetq org-crypt-key nil)))

(use-package org-agenda
  :bind (("M-a" . ar/org-agenda))
  :commands (org-agenda
             ar/org-agenda)
  :custom
  ;; Display all unscheduled todos in same buffer as agenda.
  ;; https://blog.aaronbieber.com//2016/09/24/an-agenda-for-life-with-org-mode.html
  (org-agenda-custom-commands
   '(("c" "Alvaro's agenda view"
      ((agenda "")
       (alltodo ""
                ((org-agenda-overriding-header "Unscheduled:")
                 (org-agenda-skip-function
                  '(or (org-agenda-skip-entry-if 'todo '("DONE"))
                       (org-agenda-skip-if nil '(scheduled deadline))))))
       (alltodo ""
                ((org-agenda-overriding-header "All:")))))))
  :config
  (with-eval-after-load 'fullframe
    (fullframe org-agenda-mode org-agenda-quit))

  (defun ar/org-agenda (&optional arg)
    "Agenda using my custom command."
    (interactive "P")
    (org-agenda arg "c")))
