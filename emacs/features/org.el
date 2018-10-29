(require 'ar-vsetq)

(use-package org
  :ensure t
  :bind (:map org-mode-map
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
         (org-mode . yas-minor-mode))
  :config

  (setq org-todo-keywords
        '((sequence
           "TODO"
           "STARTED"
           "DONE"
           "OBSOLETE"
           "CANCELLED")))

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

  ;; Disable auto isearch within org-goto.
  (ar/vsetq org-goto-auto-isearch nil)

  ;; Enable RET to follow Org links.
  (ar/vsetq org-return-follows-link t)

  (use-package ar-org-blog
    :commands (ar/org-blog-insert-image
               ar/org-blog-insert-resized-image))

  (use-package ar-ox-html
    :bind (:map org-mode-map
                ([f6] . ar/ox-html-export))
    :config
    (use-package ob-plantuml
      :config
      (use-package org-src)

      ;; Use fundamental mode when editing plantuml blocks with C-c '
      (add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))

      ;; We explicitly want org babel confirm evaluations.
      (ar/vsetq org-confirm-babel-evaluate t)

      (cond ((string-equal system-type "darwin")
             (ar/vsetq org-plantuml-jar-path "~/homebrew/Cellar/plantuml/1.2018.5/libexec/plantuml.jar")
             (setenv "GRAPHVIZ_DOT" (expand-file-name "~/homebrew/bin/dot")))
            (t
             (message "Warning: Could not find plantuml.8018.jar")
             (message "Warning: Could not find $GRAPHVIZ_DOT location"))))

    (use-package ox-html)

    (ar/ox-html-setup))

  (use-package ob
    :bind (:map org-mode-map
                ("C-c C-c" . org-ctrl-c-ctrl-c))
    :config
    (ar/vsetq org-export-babel-evaluate nil)

    (use-package ob-objc)
    (use-package ob-swift
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
    (csetq org-crypt-disable-auto-save nil)
    (csetq org-tags-exclude-from-inheritance (quote ("crypt")))
    ;;  Set to nil to use symmetric encryption.
    (csetq org-crypt-key nil)))
