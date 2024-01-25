;;; -*- lexical-binding: t; -*-

;; Note: I had to manually install the gnu package
;; from `list-packages'.
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (:map org-mode-map
              ("M-<RET>" . ar/org-meta-return)
              ("C-x C-q" . view-mode)
              ("C-c C-l" . ar/org-insert-link-dwim)
              ("M-[" . org-metaleft)
              ("M-]" . org-metaright)
              ("C-c C-n" . ar/org-next-entry-or-next-visible-header)
              ("C-c C-p" . ar/org-previous-entry-or-previous-visible-header))
  :custom
  (org-modules nil)
  (org-startup-folded t)
  (org-todo-keywords
   '((sequence
      "TODO(t)"
      "STARTED(s)"
      "WAITING(w!)" ;; Use @/! to log note and timestamp
      "|"
      "DONE(d!)"
      "OBSOLETE(o)"
      "CANCELLED(c)")))
  (org-refile-targets '((org-agenda-files :maxlevel . 1)))
  :validate-custom
  (org-modules '(ol-w3m
                 ol-bbdb
                 ol-bibtex
                 ol-docview
                 ol-gnus
                 ol-info
                 ol-irc
                 ol-mhe
                 ol-rmail
                 ol-eww
                 org-habit))
  (org-fontify-whole-heading-line t)
  (org-priority-start-cycle-with-default nil) ;; Start one over/under default value.
  (org-lowest-priority ?D)
  (org-default-priority ?D) ;; Ensures unset tasks have low priority.
  (org-fontify-done-headline t)
  (org-outline-path-complete-in-steps nil) ;; No need to generate in steps (I use a ivy).
  (org-priority-faces '((?A . "#ff2600")
                        (?B . "#ff5900")
                        (?C . "#ff9200")
                        (?D . "#747474")))
  (org-log-done 'time)
  (org-ellipsis " …")
  (org-fontify-emphasized-text t)

  ;; Fontify code in code blocks.
  (org-src-fontify-natively t)

  ;; Display images inline when running in GUI.
  (org-startup-with-inline-images (display-graphic-p))
  (org-src-tab-acts-natively t)

  ;; Prevent inadvertently editing invisible areas in Org.
  (org-catch-invisible-edits 'error)
  (org-cycle-separator-lines 2)
  (org-image-actual-width nil)
  (org-hide-emphasis-markers t)

  ;; All Org leading stars become invisible.
  (org-hide-leading-stars t)

  ;; Enable RET to follow Org links.
  (org-return-follows-link t)
  :hook ((org-mode . visual-line-mode))
  :config
  (defun adviced:org-yank (orig-fun &rest r)
    "Advice `adviced:org-yank' to align tables (ORIG-FUN and R)."
    (apply orig-fun r)
    (when (and (org-at-table-p)
               org-table-may-need-update)
      (org-table-align)))

  (advice-add #'org-yank
              :around
              #'adviced:org-yank)

  (defun ar/org-meta-return (&optional arg)
    (interactive "P")
    (end-of-line)
    (call-interactively 'org-meta-return))

  ;; https://ag91.github.io/blog/2019/07/01/how-to-jump-to-next-bullet-point-in-org-mode/
  (defun ar/org-next-entry-or-next-visible-header ()
    (interactive)
    (condition-case err
        (org-next-item)
      (error (org-next-visible-heading 1))))

  (defun ar/org-previous-entry-or-previous-visible-header ()
    (interactive)
    (condition-case err
        (org-previous-item)
      (error (org-previous-visible-heading 1))))

  (use-package company-org-block
    :ensure t
    :validate-custom
    (company-org-block-edit-style 'auto)
    :hook ((org-mode . (lambda ()
                         (setq-local company-backends '(company-org-block))
                         (company-mode +1)))))

  ;; A substitute for `company-org-block'.
  ;; (use-package org-block-capf
  ;;   :validate-custom
  ;;   (org-block-capf-edit-style 'auto)
  ;;   :hook ((org-mode . (defun ar/org-mode-hook ()
  ;;                        (make-local-variable completion-at-point-functions)
  ;;                        (org-block-capf-add-to-completion-at-point-functions)
  ;;                        (setq-local company-backends '(company-capf))
  ;;                        (company-mode +1)))))

  (use-package ol
    :custom
    (org-link-frame-setup '((file . find-file))) ;; open links in same window
    :config
    ;; https://kitchingroup.cheme.cmu.edu/blog/2016/11/04/New-link-features-in-org-9
    (org-link-set-parameters
     "file"
     :follow (lambda (fpath)
               (ivy-read (format "%s: " (file-name-nondirectory fpath))
                         `(("Open" . (lambda ()
                                       (find-file ,fpath)))
                           ("Reveal" . (lambda ()
                                         (dired (file-name-directory ,fpath))
                                         (re-search-forward (file-name-nondirectory ,fpath)))))
                         :action (lambda (item)
                                   (funcall (cdr item)))))))

  (use-package org-indent
    :hook ((org-mode . org-indent-mode)))

  (use-package org-goto
    :validate-custom
    (org-goto-auto-isearch nil))

  (use-package org-starless
    :hook (org-mode . org-starless-mode))

  ;; Trying out starless.
  ;; (use-package org-bullets
  ;;   :ensure t
  ;;   :hook (org-mode . org-bullets-mode)
  ;;   :validate-custom
  ;;   (org-bullets-bullet-list
  ;;    '("◉" "◎" "⚫" "○" "►" "◇")))

  (use-package org-faces
    :validate-custom
    (org-todo-keyword-faces
     '(("TODO" . (:foreground "red" :weight bold))
       ("STARTED" . (:foreground "yellow" :weight bold))
       ("DONE" . (:foreground "green" :weight bold))
       ("OBSOLETE" . (:foreground "blue" :weight bold))
       ("CANCELLED" . (:foreground "gray" :weight bold)))))

  (defun ar/org-insert-link-dwim (prefix)
    "Like `org-insert-link' but with personal dwim preferences.
With prefix, don't confirm text."
    (interactive "P")
    (let* ((point-in-link (org-in-regexp org-link-any-re 1))
           (clipboard-url (when (string-match-p "^http" (current-kill 0))
                            (current-kill 0)))
           (region-content (when (region-active-p)
                             (buffer-substring-no-properties (region-beginning)
                                                             (region-end)))))
      (cond ((and region-content clipboard-url (not point-in-link))
             (delete-region (region-beginning) (region-end))
             (insert (org-make-link-string clipboard-url region-content)))
            ((and clipboard-url (not point-in-link))
             (insert (org-make-link-string
                      clipboard-url
                      (let ((title (with-current-buffer (url-retrieve-synchronously clipboard-url)
                                     (dom-text (car
                                                (dom-by-tag (libxml-parse-html-region
                                                             (point-min)
                                                             (point-max))
                                                            'title))))))
                        (if prefix
                            title
                          (read-string "title: " title))))))
            (t
             (call-interactively 'org-insert-link)))))

  ;; Look into font-locking email addresses.
  ;; http://kitchingroup.cheme.cmu.edu/blog/category/email/
  ;; (use-package button-lock :ensure t)

  ;; When exporting anything, do not insert in kill ring.
  (setq org-export-copy-to-kill-ring nil)

  (use-package org-cliplink
    :ensure t)

  (use-package ox-epub
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
          (encode-time 0 0 0
                       (nth 3 date)
                       (nth 4 date)
                       (nth 5 date) nil -1 nil))))

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

    (defun ar/export-blog--post-process-rss (fpath)
      "Post-process (clean) feed at FPATH."
      (with-current-buffer (find-file-noselect
                            (expand-file-name fpath) t)
        (text-mode)
        (save-excursion
          (save-restriction
            (widen)
            (goto-char 0)
            (replace-string "( <span" "(<span")
            (goto-char 0)
            (replace-string "/index.html</link>" "</link>")
            (goto-char 0)
            (replace-string "/index.html</guid>" "</guid>")
            (save-buffer)))))

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
                         :builder 'webfeeder-make-rss)
        (ar/export-blog--post-process-rss (concat (file-name-as-directory default-directory)
                                                  "rss.xml"))))

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
                         :builder 'webfeeder-make-rss)
        (ar/export-blog--post-process-rss (concat (file-name-as-directory default-directory)
                                                  "emacs/rss.xml")))))

  (use-package ar-org-blog
    :commands (ar/org-blog-insert-image))

  (use-package ar-org)

  (use-package ar-ox-html
    :commands (ar/org-split-export-headline
               ar/org-split-export-async
               ar/ox-export-index-async
               ar/ox-html-export-all-async
               ar/ox-html-export-all
               ar/org-export-current-headline-async)
    :config
    (use-package ox-html)

    ;; For code block syntax highlighting.
    (use-package htmlize
      :ensure t)
    (use-package ar-org-split-export)

    (ar/ox-html-setup))

  (use-package ob
    :demand t
    :bind (:map org-mode-map
                ("C-c C-c" . org-ctrl-c-ctrl-c))
    :validate-custom
    (org-export-babel-evaluate nil)
    ;; We explicitly want org babel confirm evaluations.
    (org-confirm-babel-evaluate t)
    :config
    ;; Keep at the top and let other packages append to `org-babel-load-languages'.
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
       (C . t)
       (python . t)
       (ruby . t)
       (screen . nil)
       (shell . t)
       (sql . nil)
       (sh . nil)
       (sqlite . t)))

    ;; http://xenodium.com/emacs-chaining-org-babel-blocks
    (defun adviced:org-babel-execute-src-block (&optional orig-fun arg info params)
      "Include other source blocks using the :include header param."
      (let ((body (nth 1 info))
            (include (assoc :include (nth 2 info)))
            (named-blocks (org-element-map (org-element-parse-buffer)
                              'src-block (lambda (item)
                                           (when (org-element-property :name item)
                                             (cons (org-element-property :name item)
                                                   item))))))
        (while include
          (unless (cdr include)
            (user-error ":include without value" (cdr include)))
          (unless (assoc (cdr include) named-blocks)
            (user-error "source block \"%s\" not found" (cdr include)))
          (setq body (concat (org-element-property :value (cdr (assoc (cdr include) named-blocks)))
                             body))
          (setf (nth 1 info) body)
          (setq include (assoc :include
                               (org-babel-parse-header-arguments
                                (org-element-property :parameters (cdr (assoc (cdr include) named-blocks)))))))
        (funcall orig-fun arg info params)))

    (advice-add 'org-babel-execute-src-block :around 'adviced:org-babel-execute-src-block)

    (use-package ob-tangle
      :commands ar/ob-tangle-current-block
      :config
      (defun ar/ob-tangle-current-block ()
        "Like `org-babel-tangle' but for current block."
        (interactive)
        (org-babel-tangle '(4))))

    (use-package ob-python
      :validate-custom
      ;; Make python source blocks export and output results by default.
      (org-babel-default-header-args:python
       '((:exports  . "both")
         (:results  . "output"))))

    (use-package ob-objc)
    (use-package ob-kotlin
      :ensure t)

    (use-package ob-restclient
      :ensure t
      :config
      (org-babel-do-load-languages 'org-babel-load-languages
                                   (append org-babel-load-languages
                                           '((restclient . t)))))

    (use-package ob-http
      :ensure t
      :config
      (org-babel-do-load-languages 'org-babel-load-languages
                                   (append org-babel-load-languages
                                           '((http . t)))))

    (use-package ob-applescript
      :ensure t
      :config
      (org-babel-do-load-languages 'org-babel-load-languages
                                   (append org-babel-load-languages
                                           '((applescript . t)))))

    (use-package ob-swift
      :ensure t
      :config
      (org-babel-do-load-languages 'org-babel-load-languages
                                   (append org-babel-load-languages
                                           '((swift . t))))

      (defun ar/org-refresh-inline-images ()
        (when org-inline-image-overlays
          (org-redisplay-inline-images)))

      ;; Automatically refresh inline images.
      (add-hook 'org-babel-after-execute-hook 'ar/org-refresh-inline-images))

    (use-package ob-plantuml
      :config
      (use-package org-src
        :validate-custom
        ;; When editing src block (via `org-edit-special'), use current window.
        (org-src-window-setup 'current-window)
        :config
        (defun adviced:org-edit-src-save (orig-fun &rest r)
          "Run `before-save-hook' on source block major mode (helps with formatters)."
          (with-demoted-errors (run-hooks #'before-save-hook))
          (apply orig-fun r))

        (advice-add #'org-edit-src-save
                    :around
                    #'adviced:org-edit-src-save))

      ;; Use fundamental mode when editing plantuml blocks with C-c '
      (add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))

      (cond ((string-equal system-type "darwin")
             (setq org-plantuml-jar-path
                   (seq-first
                    (process-lines "find"
                                   (file-name-as-directory (seq-first
                                                            (process-lines "brew"
                                                                           "--prefix"
                                                                           "plantuml")))
                                   "-iname" "plantuml.jar")))
             (setenv "GRAPHVIZ_DOT" (seq-first (process-lines "find"
                                                              (file-name-as-directory (seq-first
                                                                                       (process-lines "brew"
                                                                                                      "--prefix"
                                                                                                      "graphviz")))
                                                              "-iname" "dot"))))
            (t
             (message "Warning: Could not find plantuml.8018.jar")
             (message "Warning: Could not find $GRAPHVIZ_DOT location"))))

    (use-package gnuplot
      :ensure t))

  (use-package org-crypt
    :custom
    (org-tags-exclude-from-inheritance (quote ("crypt")))
    :validate-custom
    (org-crypt-disable-auto-save nil)
    :config
    (org-crypt-use-before-save-magic)))

;; org-present config based on
;; https://systemcrafters.net/emacs-tips/presentations-with-org-present
(use-package org-present
  :ensure t
  :bind (:map org-present-mode-keymap
              ("C-c C-n" . ar/org-present-next-item)
              ("C-c C-p" . ar/org-present-previous-item))
  :custom
  (org-present-text-scale 2)
  :hook ((org-present-mode . ar/org-present-mode-hook)
         (org-present-mode-quit . ar/org-present-mode-quit))
  :init
  (defun ar/org-present-next-item (&optional backward)
    "Present and reveal next item."
    (interactive "P")
    ;; Beginning of slide, go to previous slide.
    (if (and backward (eq (point) (point-min)))
        (org-present-prev)
      (let* ((heading-pos (ar/org-next-visible-heading-pos backward))
             (link-pos (ar/org-next-link-pos backward))
             (block-pos (ar/org-next-block-pos backward))
             (closest-pos (when (or heading-pos link-pos block-pos)
                            (apply (if backward #'max #'min)
                                   (seq-filter #'identity
                                               (list heading-pos
                                                     link-pos
                                                     block-pos))))))
        (if closest-pos
            (progn
              (cond ((eq heading-pos closest-pos)
                     (goto-char heading-pos))
                    ((eq link-pos closest-pos)
                     (goto-char link-pos))
                    ((eq block-pos closest-pos)
                     (goto-char block-pos)))
              ;; Reveal relevant content.
              (cond ((> (org-current-level) 1)
                     (ar/org-present-reveal-level2))
                    ((eq (org-current-level) 1)
                     ;; At level 1. Collapse children.
                     (org-overview)
                     (org-show-entry)
                     (org-show-children)
                     (run-hook-with-args 'org-cycle-hook 'children))))
          ;; End of slide, go to next slide.
          (org-present-next)))))

  (defun ar/org-present-previous-item ()
    (interactive)
    (ar/org-present-next-item t))

  (defun ar/org-next-visible-heading-pos (&optional backward)
    "Similar to `org-next-visible-heading' but for returning position.

Set BACKWARD to search backwards."
    (save-excursion
      (let ((pos-before (point))
            (pos-after (progn
                         (org-next-visible-heading (if backward -1 1))
                         (point))))
        (when (and pos-after (not (equal pos-before pos-after)))
          pos-after))))

  (defun ar/org-next-link-pos (&optional backward)
    "Similar to `org-next-visible-heading' but for returning position.

Set BACKWARD to search backwards."
    (save-excursion
      (let* ((inhibit-message t)
             (pos-before (point))
             (pos-after (progn
                          (org-next-link backward)
                          (point))))
        (when (and pos-after (or (and backward (> pos-before pos-after))
                                 (and (not backward) (> pos-after pos-before))))
          pos-after))))

  (defun ar/org-next-block-pos (&optional backward)
    "Similar to `org-next-block' but for returning position.

Set BACKWARD to search backwards."
    (save-excursion
      (when (and backward (org-babel-where-is-src-block-head))
        (org-babel-goto-src-block-head))
      (let ((pos-before (point))
            (pos-after (ignore-errors
                         (org-next-block 1 backward)
                         (point))))
        (when (and pos-after (not (equal pos-before pos-after)))
          ;; Place point inside block body.
          (goto-char (line-beginning-position 2))
          (point)))))

  (defun ar/org-present-reveal-level2 ()
    (interactive)
    (let ((loc (point))
          (level (org-current-level))
          (heading))
      (ignore-errors (org-back-to-heading t))
      (while (or (not level) (> level 2))
        (setq level (org-up-heading-safe)))
      (setq heading (point))
      (goto-char (point-min))
      (org-overview)
      (org-show-entry)
      (org-show-children)
      (run-hook-with-args 'org-cycle-hook 'children)
      (goto-char heading)
      (org-show-subtree)
      (goto-char loc)))

  (defun ar/org-present-mode-quit ()
    (ar/drop-material-org-present-tweaks)
    (setq-local face-remapping-alist nil)
    (org-starless-mode +1)
    (hide-mode-line-mode -1)
    (visual-fill-column-mode -1)
    (visual-line-mode -1))

  (defun ar/org-present-mode-hook ()
    (ar/load-material-org-present-tweaks)
    (setq-local face-remapping-alist '((header-line (:height 5.0) header-line)
                                       (default (:height 1.2) default)
                                       (org-level-1 (:height 1.50) org-level-1)
                                       (org-block-begin-line (:height 0) org-block-begin-line)
                                       (org-block-end-line (:height 0) org-block-end-line)))
    ;; Starless breaks padding between folded headings.
    (org-starless-mode -1)
    (org-show-children)
    (hide-mode-line-mode +1)
    ;; Add padding at top of each slide.
    (setq-local header-line-format (propertize " " 'face '(:height 300)))
    (visual-fill-column-mode +1)
    (visual-line-mode +1)
    (run-hook-with-args 'org-cycle-hook 'children))
  :config
  (use-package visual-fill-column
    :custom
    (visual-fill-column-width 70)
    (visual-fill-column-center-text t)
    :ensure t)
  (add-hook 'org-present-after-navigate-functions
            (defun ar/org-present-after-navigate (buffer-name heading)
              ;; Show only top-level headlines
              (org-overview)
              ;; Unfold the current entry
              (org-show-entry)
              ;; Show only direct subheadings only
              (org-show-children)
              (run-hook-with-args 'org-cycle-hook 'children))))

;; Like org-bullets but for priorities.
(use-package org-fancy-priorities
  :ensure t
  :hook
  (org-mode . org-fancy-priorities-mode)
  :validate-custom
  (org-fancy-priorities-list '("HIGH" "MID" "LOW" "OPTIONAL")))

(use-package org-agenda
  :hook ((org-agenda-mode . goto-address-mode) ;; <RET> follows links.
         (org-agenda-mode . hl-line-mode)) ;; Easier to see selected row.
  :bind (("M-a" . ar/org-agenda-toggle)
         :map org-agenda-mode-map
         ;; I prefer my M-m global key bind for another purpose.
         ("M-m" . nil)
         ;; Use org-return instead since `org-return-follows-link' is set.
         ("<RET>" . org-return)
         ;; C-n/C-p for can be used for granular movement.
         ;; Use n/p for faster movement between items (jumps through sections).
         ("n" . org-agenda-next-item)
         ("P" . ar/org-agenda-previous-header)
         ("N" . ar/org-agenda-next-header)
         ("p" . org-agenda-previous-item)
         ("g" . org-agenda-redo)
         ("x" . ar/org-agenda-done)
         ("X" . ar/org-agenda-mark-done-and-add-followup)
         ("s" . ar/org-agenda-schedule-dwim)
         ("M-<up>" . ar/org-agenda-item-move-up)
         ("M-<down>" . ar/org-agenda-item-move-down)
         ("M-<left>" . org-agenda-do-date-earlier)
         ("M-<right>" . org-agenda-do-date-later)
         ("S-<left>" . ar/org-agenda-todo-previous-keyword)
         ("S-<right>" . ar/org-agenda-todo-next-keyword)
         ("1"  . ar/org-agenda-item-to-top)
         ("c" . ar/org-agenda-capture)
         ("C" . ar/org-agenda-capture))
  :commands (org-agenda
             ar/org-agenda-toggle)
  :validate-custom
  (org-agenda-scheduled-leaders '("Scheduled: " "Overdue %dd: "))
  (org-agenda-use-time-grid nil)
  ;; Default to daily view.
  (org-agenda-span 'day)
  ;; Follow mode narrows to task subtree only.
  (org-agenda-follow-indirect t)
  (org-agenda-block-separator ?\u2015)
  :config
  ;; Display all unscheduled todos in same buffer as agenda.
  ;; https://blog.aaronbieber.com//2016/09/24/an-agenda-for-life-with-org-mode.html
  ;; Handle in config to avoid post-custom handling during init loading.
  (customize-set-variable 'org-agenda-custom-commands
                          '(("c" "Alvaro's agenda view"
                             ((agenda "" ((org-agenda-sorting-strategy
                                           (quote ((agenda time-up todo-state-down priority-down alpha-down category-keep))))))
                              (alltodo ""
                                       ((org-agenda-overriding-header "Unscheduled:")
                                        (org-agenda-skip-function
                                         '(or (org-agenda-skip-entry-if 'todo '("DONE" "OBSOLETE" "CANCELLED"))
                                              (org-agenda-skip-if nil '(scheduled deadline))))))))
                            ("a" "This week"
                             ((agenda "" ((org-agenda-sorting-strategy
                                           (quote ((agenda todo-state-down priority-down alpha-down category-keep)))))))
                             nil
                             ("~/Downloads/agenda.html"))))

  ;; Trying out agenda children preview
  ;; https://github.com/alphapapa/unpackaged.el#agenda-previews
  (use-package ov
    :ensure t
    :bind (:map org-agenda-mode-map
                ("<tab>" . ar/org-agenda-tab-dwim))
    :config
    (defun ar/org-agenda-tab-dwim (prefix)
      (interactive "P")
      (if prefix
          (call-interactively 'org-agenda-goto)
        (ar/org-agenda-toggle-preview)))

    (defface ar/org-agenda-preview
      '((t (:background "black")))
      "Face for Org Agenda previews."
      :group 'org)

    (defun ar/org-agenda-toggle-preview ()
      "Toggle overlay of current item in agenda."
      (interactive)
      (require 'dash)
      (require 'ov)
      (if-let* ((overlay (ov-in 'ar/org-agenda-preview t (line-end-position) (line-end-position))))
          ;; Hide existing preview
          (ov-reset overlay)
        ;; Show preview
        (let* ((entry-contents (--> (org-agenda-with-point-at-orig-entry
                                     nil (buffer-substring (save-excursion
                                                             (ar/org-forward-to-entry-content t)
                                                             (point))
                                                           (org-entry-end-position)))
                                    s-trim
                                    (concat "\n" it "\n"))))
          (add-face-text-property 0 (length entry-contents)
                                  'ar/org-agenda-preview nil entry-contents)
          (ov (line-end-position) (line-end-position)
              'ar/org-agenda-preview t
              'before-string entry-contents))))

    (defun ar/org-forward-to-entry-content (&optional unsafe)
      "Skip headline, planning line, and all drawers in current entry.
If UNSAFE is non-nil, assume point is on headline."
      (unless unsafe
        ;; To improve performance in loops (e.g. with `org-map-entries')
        (org-back-to-heading))
      (cl-loop for element = (org-element-at-point)
               for pos = (pcase element
                           (`(headline . ,_) (org-element-property :contents-begin element))
                           (`(,(or 'planning 'property-drawer 'drawer) . ,_) (org-element-property :end element)))
               while pos
               do (goto-char pos))))


  (use-package org-super-agenda
    :ensure t
    :validate-custom
    (org-super-agenda-groups
     '((:name "Category: habit"
              :category "habit")
       (:name "Category: inbox"
              :category "inbox")
       (:name ""
              :auto-category t)))
    :config
    (org-super-agenda-mode +1))

  (with-eval-after-load 'fullframe
    (fullframe org-agenda-mode
               org-agenda-quit))

  ;; A little formatting of agenda view.
  ;;   Tuesday     1 October 2019
  ;; ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░ DONE High Pass the salt.
  ;; (let ((spaces (make-string 32 (string-to-char "░"))))
  ;;   (map-put org-agenda-prefix-format 'agenda (concat spaces " ")))
  ;; (map-put org-agenda-prefix-format 'todo " %i %-31:c")
  (map-put org-agenda-prefix-format 'agenda "    % s")
  (map-put org-agenda-prefix-format 'todo "    ")

  ;; https://pages.sachachua.com/.emacs.d/Sacha.html#org32b4908
  (defun ar/org-agenda-done (&optional arg)
    "Mark current TODO as done.
This changes the line at point, all other lines in the agenda referring to
the same tree node, and the headline of the tree node in the Org-mode file."
    (interactive "P")
    (org-agenda-todo "DONE"))

  ;; https://pages.sachachua.com/.emacs.d/Sacha.html#orgade1aa9
  (defun ar/org-agenda-mark-done-and-add-followup ()
    "Mark the current TODO as done and add another task after it.
Creates it at the same level as the previous task, so it's better to use
this with to-do items than with projects or headings."
    (interactive)
    (org-agenda-todo "DONE")
    (org-agenda-switch-to)
    (org-capture 0 "t"))

  (defun ar/org-agenda-todo-previous-keyword ()
    "Transition current task to previous state in `org-todo-keywords'"
    (interactive)
    (let ((org-inhibit-logging t)) ;; Do not ask for notes when transitioning states.
      (org-agenda-todo 'left)))

  (defun ar/org-agenda-todo-next-keyword ()
    "Transition current task to next state in `org-todo-keywords'"
    (interactive)
    (let ((org-inhibit-logging t)) ;; Do not ask for notes when transitioning states.
      (org-agenda-todo 'right)))

  (defun ar/org-agenda-item-move-up ()
    "Move the current agenda item up."
    (interactive)
    (unless (ignore-errors
              (org-save-all-org-buffers)
              (org-agenda-switch-to)
              (org-metaup)
              (switch-to-buffer (other-buffer (current-buffer) 1))
              (org-agenda-redo)
              (org-agenda-previous-line))
      ;; Something err'd. Switch back to agenda anyway.
      (switch-to-buffer (other-buffer (current-buffer) 1))))

  (defun ar/org-agenda-item-move-down ()
    "Move the current agenda item down."
    (interactive)
    (unless (ignore-errors
              (org-save-all-org-buffers)
              (org-agenda-switch-to)
              (org-metadown)
              (switch-to-buffer (other-buffer (current-buffer) 1))
              (org-agenda-redo)
              (org-agenda-next-line))
      ;; Something err'd. Switch back to agenda anyway.
      (switch-to-buffer (other-buffer (current-buffer) 1))))

  ;; From http://pragmaticemacs.com/emacs/reorder-todo-items-in-your-org-mode-agenda/
  (defun ar/org-headline-to-top ()
    "Move the current org headline to the top of its section"
    (interactive)
    ;; check if we are at the top level
    (let ((lvl (org-current-level)))
      (cond
       ;; above all headlines so nothing to do
       ((not lvl)
        (message "No headline to move"))
       ((= lvl 1)
        ;; if at top level move current tree to go above first headline
        (org-cut-subtree)
        (beginning-of-buffer)
        ;; test if point is now at the first headline and if not then
        ;; move to the first headline
        (unless (looking-at-p "*")
          (org-next-visible-heading 1))
        (org-paste-subtree))
       ((> lvl 1)
        ;; if not at top level then get position of headline level above
        ;; current section and refile to that position. Inspired by
        ;; https://gist.github.com/alphapapa/2cd1f1fc6accff01fec06946844ef5a5
        (let* ((org-reverse-note-order t)
               (pos (save-excursion
                      (outline-up-heading 1)
                      (point)))
               (filename (buffer-file-name))
               (rfloc (list nil filename nil pos)))
          (org-refile nil nil rfloc))))))

  (defun ar/org-agenda-item-to-top ()
    "Move the current agenda item to the top of the subtree in its file"
    (interactive)
    (org-save-all-org-buffers)
    (org-agenda-switch-to)
    (ar/org-headline-to-top)
    (switch-to-buffer (other-buffer (current-buffer) 1))
    (org-agenda-redo))

  ;; From https://blog.aaronbieber.com/2016/09/25/agenda-interactions-primer.html
  (defun ar/org-agenda-capture (&optional vanilla)
    "Capture a task in agenda mode, using the date at point.

If VANILLA is non-nil, run the standard `org-capture'."
    (interactive "P")
    (if vanilla
        (org-capture)
      (let ((org-overriding-default-time (org-get-cursor-date)))
        (org-capture nil "t")))
    (org-save-all-org-buffers))

  (defun ar/org-agenda-next-header ()
    "Jump to the next header in an agenda series."
    (interactive)
    (ar/org-agenda--goto-header))

  (defun ar/org-agenda-previous-header ()
    "Jump to the previous header in an agenda series."
    (interactive)
    (ar/org-agenda--goto-header t))

  (defun ar/org-agenda--goto-header (&optional backwards)
    "Find the next agenda series header forwards or BACKWARDS."
    (let ((pos (save-excursion
                 (goto-char (if backwards
                                (line-beginning-position)
                              (line-end-position)))
                 (let* ((find-func (if backwards
                                       'previous-single-property-change
                                     'next-single-property-change))
                        (end-func (if backwards
                                      'max
                                    'min))
                        (all-pos-raw (list (funcall find-func (point) 'org-agenda-structural-header)
                                           (funcall find-func (point) 'org-agenda-date-header)))
                        (all-pos (cl-remove-if-not 'numberp all-pos-raw))
                        (prop-pos (if all-pos (apply end-func all-pos) nil)))
                   prop-pos))))
      (if pos (goto-char pos))
      (if backwards (goto-char (line-beginning-position)))))

  (defun ar/org-agenda-toggle (&optional arg)
    "Toggles between agenda using my custom command and org file."
    (interactive "P")
    (if (eq major-mode 'org-agenda-mode)
        (find-file "~/stuff/active/agenda.org")
      (let ((agenda-buffer (get-buffer "*Org Agenda*")))
        (if agenda-buffer
            ;; Switching buffer doesn't reset point location.
            (switch-to-buffer agenda-buffer)
          (org-agenda arg "c")))))

  ;; `ar/org-agenda-schedule-dwim' `ar/org-agenda-validate-marked-entries' and
  ;; `ar/org-agenda-bulk-action' are mostly lifted from org-agenda.el.
  ;; The dwim methods behave more like dired bulk commands: if there's a selection
  ;; operate on all items, otherwise operate on current point.
  (defun ar/org-agenda-schedule-dwim (&optional arg)
    (interactive "P")
    (ar/org-agenda-validate-marked-entries)
    (let ((time (and (not arg)
                     (let ((new (org-read-date
                                 nil nil nil "(Re)Schedule to" org-overriding-default-time)))
                       ;; A "double plus" answer applies to every
                       ;; scheduled time.  Do not turn it into
                       ;; a fixed date yet.
                       (if (string-match-p "\\`[ \t]*\\+\\+"
                                           org-read-date-final-answer)
                           org-read-date-final-answer
                         new)))))
      (ar/org-agenda-bulk-action (lambda ()
                                   (org-agenda-schedule arg time)))))

  ;; (defun ar/org-agenda-todo-dwim (&optional arg)
  ;;   (interactive "P")
  ;;   (org-fast-todo-selection) (completing-read
  ;;                                           "Todo state: "
  ;;                                           org-todo-keywords-1)
  ;;   (let ((state (org-fast-todo-selection))
  ;;         (org-inhibit-blocking t)
  ;;         (org-inhibit-logging 'note))
  ;;     (ar/org-agenda-bulk-action (lambda ()
  ;;                                  (org-agenda-todo state)))))

  (defun ar/org-agenda-validate-marked-entries ()
    "Ensure all marked org agenda entries in selection are valid."
    (dolist (m org-agenda-bulk-marked-entries)
      (unless (and (markerp m)
                   (marker-buffer m)
                   (buffer-live-p (marker-buffer m))
                   (marker-position m))
        (user-error "Marker %s for bulk command is invalid" m))))

  (defun ar/org-agenda-bulk-action (cmd)
    "Apply CMD to `org-agenda-bulk-marked-entries'."
    (if org-agenda-bulk-marked-entries
        (progn
          ;; Loop over all markers and apply bulk command.
          (let ((processed 0)
                (skipped 0)
                ;; Sort the markers, to make sure that parents are handled
                ;; before children.
                (entries (sort org-agenda-bulk-marked-entries
                               (lambda (a b)
                                 (cond
                                  ((eq (marker-buffer a) (marker-buffer b))
                                   (< (marker-position a) (marker-position b)))
                                  (t
                                   (string< (buffer-name (marker-buffer a))
                                            (buffer-name (marker-buffer b))))))))
                redo-at-end)
            (dolist (e entries)
              (let ((pos (text-property-any (point-min) (point-max) 'org-hd-marker e)))
                (if (not pos)
                    (progn (message "Skipping removed entry at %s" e)
                           (cl-incf skipped))
                  (goto-char pos)
                  (let (org-loop-over-headlines-in-active-region)
                    (funcall cmd))
                  ;; `post-command-hook' is not run yet.  We make sure any
                  ;; pending log note is processed.
                  (when (or (memq 'org-add-log-note (default-value 'post-command-hook))
                            (memq 'org-add-log-note post-command-hook))
                    (org-add-log-note))
                  (cl-incf processed))))
            (when redo-at-end (org-agenda-redo))
            (unless org-agenda-persistent-marks (org-agenda-bulk-unmark-all))
            (message "Acted on %d entries%s%s"
                     processed
                     (if (= skipped 0)
                         ""
                       (format ", skipped %d (disappeared before their turn)"
                               skipped))
                     (if (not org-agenda-persistent-marks) "" " (kept marked)"))))
      (funcall cmd)))

  ;; Automatically save org files after transitioning tasks.
  (advice-add 'org-agenda-todo :after 'org-save-all-org-buffers))


(use-package org-capture
  :bind (("M-c" . ar/org-capture-todo)
         :map org-capture-mode-map
         ("+" . ar/org-capture-priority-up-dwim)
         ("-" . ar/org-capture-priority-down-dwim)
         ("M-<right>" . ar/org-capture-schedule-day-later-dwim)
         ("M-<left>"  . ar/org-capture-schedule-day-earlier-dwim)
         ("C-c C-c" . ar/org-capture-finalize-dwim))
  :commands (ar/org-capture-todo
             org-capture)
  :custom
  (org-capture-templates
   '(("t" "Todo" entry (file+headline "~/stuff/active/agenda.org" "INBOX")
      "* TODO %?\nSCHEDULED: %t" :prepend t)
     ("j" "Journelly" entry (file "~/Documents/My org files/Journelly.org")
      "* %U [At home]\n%?" :prepend t)))
  :config
  (defun ar/org-capture-finalize-dwim (prefix)
    "With prefix, invoke `org-capture-finalize' else invoke `org-capture-finalize'."
    (interactive "P")
    (if prefix
        ;; Prefix has been handled. Do not propagate as it would mean something
        ;; else in org commands.
        (let ((current-prefix-arg nil))
          (call-interactively #'org-capture-refile))
      (org-capture-finalize)))

  (defun ar/org-capture-schedule-day-earlier-dwim ()
    (interactive)
    (if (ar/org-capture--special-pos-p)
        (org-schedule (point) (time-add
                               (org-get-scheduled-time (point))
                               -86400))  ;; day in seconds
      (org-metaleft)))

  (defun ar/org-capture-schedule-day-later-dwim ()
    (interactive)
    (if (ar/org-capture--special-pos-p)
        (org-schedule (point) (time-add
                               (org-get-scheduled-time (point))
                               86400))  ;; day in seconds
      (org-metaright)))

  (defun ar/org-capture--special-pos-p ()
    "Either at beginning of line or next to TODO [#A]."
    (or (looking-back "^")
        (looking-back "TODO\\s-*\\(\\[#[A-Z]\\]\\s-*\\)*")))

  (defun ar/org-capture-priority-up-dwim (N)
    "If at special location, increase priority."
    (interactive "p")
    (if (ar/org-capture--special-pos-p)
        (org-priority 'up)
      (org-self-insert-command N)))

  (defun ar/org-capture-priority-down-dwim (N)
    "If at special location, decrease priority."
    (interactive "p")
    (if (ar/org-capture--special-pos-p)
        (org-priority 'down)
      (org-self-insert-command N)))

  (defun ar/org-capture-todo (&optional vanilla)
    "Capture a todo. If VANILLA is non-nil, prompt the user for task type."
    (interactive "P")
    (if vanilla
        (org-capture)
      (org-capture nil "t"))))
