;;; -*- lexical-binding: t; -*-
(require 'ar-vsetq)
(require 'map)

(use-package org
  :ensure org-plus-contrib ;; Ensure latest org installed from elpa
  :bind (:map org-mode-map
              ("M-RET" . ar/org-meta-return)
              ("C-x C-q" . view-mode)
              ("C-c C-l" . ar/org-insert-link-dwim)
              ("M-[" . org-metaleft)
              ("M-]" . org-metaright))
  :custom
  (org-todo-keywords
   '((sequence
      "TODO(t)"
      "STARTED(s)"
      "WAITING(w@/!)"
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
  (org-ellipsis "…")
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
  :hook ((org-mode . ar/org-mode-hook-function)
         (org-mode . visual-line-mode)
         (org-mode . yas-minor-mode)
         (org-mode . smartparens-mode)
         (org-mode . org-display-inline-images))
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

  (defun ar/org-mode-hook-function ()
    (toggle-truncate-lines 0)
    (ar/vsetq show-trailing-whitespace t)
    (set-fill-column 1000)
    (use-package ar-org)
    (setq-local company-backends '(company-org-block))
    (company-mode +1))

  (defun ar/org-meta-return (&optional arg)
    (interactive "P")
    (end-of-line)
    (call-interactively 'org-meta-return))

  (use-package company-org-block)

  (use-package ol
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

  (use-package org-compat
    :config
    ;; Handle youtube org links in the form of [[youtube:XjKtkEMUYGc][Some description]]
    ;; Based on http://endlessparentheses.com/embedding-youtube-videos-with-org-mode-links.html
    (org-add-link-type
     "youtube"
     (lambda (handle)
       (browse-url (concat "https://www.youtube.com/watch?v=" handle)))
     (lambda (path desc backend)
       (cl-case backend
         (html (format
                "<p style='text-align:center;'>
                    <iframe width='420' height='315' align='middle'
                            src='https://www.youtube.com/embed/W4LxHn5Y_l4?controls=0'
                            allowFullScreen>
                    </iframe>
                 </p>"
                path (or desc "")))
         (latex (format "\href{%s}{%s}" path (or desc "video")))))))
  (use-package org-indent
    :hook ((org-mode . org-indent-mode)))

  (use-package org-goto
    :validate-custom
    (org-goto-auto-isearch nil))

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
    "Convert selected region into a link with clipboard http URL (if one is found).
Fetch and propose title from URL (if one is found). Default to `org-insert-link' otherwise."
    (interactive)
    (if (string-match-p "^http" (current-kill 0))
        (if (and (region-active-p)
                 ;; Not on link.
                 (not (org-in-regexp org-link-any-re 1)))
            (let ((region-content (buffer-substring-no-properties (region-beginning)
                                                                  (region-end))))
              (delete-region (region-beginning)
                             (region-end))
              (insert (format "[[%s][%s]]"
                              (current-kill 0)
                              region-content)))
          ;; Fetch from URL.
          (ar/org-insert-clipboard-link))
      (call-interactively 'org-insert-link)))

  ;; Look into font-locking email addresses.
  ;; http://kitchingroup.cheme.cmu.edu/blog/category/email/
  ;; (use-package button-lock :ensure t)

  ;; When exporting anything, do not insert in kill ring.
  (setq org-export-copy-to-kill-ring nil)

  (use-package org-cliplink
    :ensure t)

  (use-package ox-epub
    :ensure t)

  (use-package ox-reveal
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
    :commands (ar/org-split-export-headline
               ar/org-split-export-async
               ar/ox-html-export-all-async
               ar/ox-html-export-all
               ar/org-export-current-headline-async)
    :bind (:map org-mode-map
                ([f6] . ar/ox-html-export-all))
    :config
    (use-package ar-org)

    (use-package ox-html)

    ;; For code block syntax highlighting.
    (use-package htmlize
      :ensure t)
    (use-package ar-org-split-export)

    (ar/ox-html-setup))

  (use-package ob
    :bind (:map org-mode-map
                ("C-c C-c" . org-ctrl-c-ctrl-c))
    :validate-custom
    (org-export-babel-evaluate nil)
    ;; We explicitly want org babel confirm evaluations.
    (org-confirm-babel-evaluate t)
    :config
    (use-package ob-python
      :validate-custom
      ;; Make python source blocks export and output results by default.
      (org-babel-default-header-args:python
       '((:exports  . "both")
         (:results  . "output"))))

    (use-package ob-objc)
    (use-package ob-kotlin
      :ensure t)

    (use-package ob-swift
      :ensure t
      :config
      (org-babel-do-load-languages 'org-babel-load-languages
                                   (append org-babel-load-languages
                                           '((swift     . t))))

      (defun ar/org-refresh-inline-images ()
        (when org-inline-image-overlays
          (org-redisplay-inline-images)))

      ;; Automatically refresh inline images.
      (add-hook 'org-babel-after-execute-hook 'ar/org-refresh-inline-images)

      (defun adviced:org-babel-execute:swift (f &rest args)
        "Advice `adviced:org-babel-execute:swift' enabling swiftui header param."
        (let* ((body (nth 0 args))
               (params (nth 1 args))
               (swiftui (cdr (assoc :swiftui params)))
               (output))
          (when swiftui
            (assert (or (string-equal swiftui "preview")
                        (string-equal swiftui "interactive"))
                    nil ":swiftui must be either preview or interactive")
            (setq body (format
                        "
import Cocoa
import SwiftUI
import Foundation

let screenshotURL: URL = URL(fileURLWithPath: NSTemporaryDirectory(), isDirectory: true).appendingPathComponent(ProcessInfo.processInfo.globallyUniqueString + \".tiff\")

let preview = %s

NSApplication.shared.run {
  %s
}

extension NSApplication {
  public func run<V: View>(@ViewBuilder view: () -> V) {
    let appDelegate = AppDelegate(view())
    NSApp.setActivationPolicy(.regular)
    mainMenu = customMenu
    delegate = appDelegate
    run()
  }
}

extension NSApplication {
  var customMenu: NSMenu {
    let appMenu = NSMenuItem()
    appMenu.submenu = NSMenu()

    let quitItem = NSMenuItem(
      title: \"Quit \(ProcessInfo.processInfo.processName)\",
      action: #selector(NSApplication.terminate(_:)), keyEquivalent: \"q\")
    quitItem.keyEquivalentModifierMask = []
    appMenu.submenu?.addItem(quitItem)

    let mainMenu = NSMenu(title: \"Main Menu\")
    mainMenu.addItem(appMenu)
    return mainMenu
  }
}

class AppDelegate<V: View>: NSObject, NSApplicationDelegate, NSWindowDelegate {
  var window = NSWindow(
    contentRect: NSRect(x: 0, y: 0, width: 414 * 0.2, height: 896 * 0.2),
    styleMask: [.titled, .closable, .miniaturizable, .resizable, .fullSizeContentView],
    backing: .buffered, defer: false)

  var contentView: V

  init(_ contentView: V) {
    self.contentView = contentView
  }

  func applicationDidFinishLaunching(_ notification: Notification) {
    window.delegate = self
    window.center()
    window.contentView = NSHostingView(rootView: contentView)
    window.makeKeyAndOrderFront(nil)

    if preview {
      screenshot(view: window.contentView!, saveTo: screenshotURL)
      // Write path (without newline) so org babel can parse it
      // and also drop first chars (file://).
      FileHandle.standardOutput.write(screenshotURL.absoluteString.dropFirst(7).data(using:.utf8, allowLossyConversion:false)!)
      NSApplication.shared.terminate(self)
      return
    }

    window.setFrameAutosaveName(\"Main Window\")
    NSApp.activate(ignoringOtherApps: true)
  }
}

func screenshot(view: NSView, saveTo fileURL: URL) {
  let rep = view.bitmapImageRepForCachingDisplay(in: view.bounds)!
  view.cacheDisplay(in: view.bounds, to: rep)

  let image = NSImage(size: view.bounds.size)
  image.addRepresentation(rep)

  try! image.tiffRepresentation?.write(to: fileURL)
}"
                        (if (string-equal swiftui "preview")
                            "true"
                          "false")
                        body))
            (setq args (list body params)))
          (setq output (apply f args))
          (when org-inline-image-overlays
            (org-redisplay-inline-images))
          output))

      (advice-add #'org-babel-execute:swift
                  :around
                  #'adviced:org-babel-execute:swift))

    (use-package ob-plantuml
      :config
      (use-package org-src
        :validate-custom
        ;; When editing src block (via `org-edit-special'), use current window.
        (org-src-window-setup 'current-window))

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
       (sqlite . t))))

  (use-package org-crypt
    :custom
    (org-tags-exclude-from-inheritance (quote ("crypt")))
    ;;  Set to nil to use symmetric encryption.
    (org-crypt-key nil)
    :validate-custom
    (org-crypt-disable-auto-save nil)
    :config
    (org-crypt-use-before-save-magic)))

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
  :custom
  ;; Display all unscheduled todos in same buffer as agenda.
  ;; https://blog.aaronbieber.com//2016/09/24/an-agenda-for-life-with-org-mode.html
  (org-agenda-custom-commands
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
  :config
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
        (org-capture nil "t"))))

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
         ("M-<left>"  . ar/org-capture-schedule-day-earlier-dwim))
  :commands (ar/org-capture-todo
             org-capture)
  :custom
  (org-capture-templates
   '(("t" "Todo" entry (file+headline "~/stuff/active/agenda.org" "INBOX")
      "* TODO %?\nSCHEDULED: %t" :prepend t)))
  :config
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
