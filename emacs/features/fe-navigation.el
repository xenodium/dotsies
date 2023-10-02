;;; -*- lexical-binding: t; -*-

;; Smarter move to beginning/end of line.
(use-package mwim
  :ensure t
  :bind (:map prog-mode-map
              ("C-a" . mwim-beginning-of-code-or-line)
              ("C-e" . mwim-end-of-code-or-line)))

;; http://endlessparentheses.com/meta-binds-part-2-a-peeve-with-paragraphs.html
;; Disabling. I'm currently using M-a as shortcut to agenda.
;; (global-set-key (kbd "M-a") 'ar/backward-paragraph)
;; (global-set-key (kbd "M-e") 'ar/forward-paragraph)

(defun ar/forward-paragraph (&optional n)
  "Advance just past next blank line."
  (interactive "p")
  (let ((para-commands
         '(ar/forward-paragraph
           ar/backward-paragraph)))
    ;; Only push mark if it's not active and we're not
    ;; repeating.
    (or (use-region-p)
        (not (member this-command para-commands))
        (member last-command para-commands)
        (push-mark))
    ;; The actual movement.
    (dotimes (_ (abs n))
      (if (> n 0)
          (skip-chars-forward "\n[:blank:]")
        (skip-chars-backward "\n[:blank:]"))
      (if (search-forward-regexp
           "\n[[:blank:]]*\n[[:blank:]]*"
           nil t (cl-signum n))
          (goto-char (match-end 0))
        (goto-char
         (if (> n 0) (point-max) (point-min)))))))

(defun ar/backward-paragraph (&optional n)
  "Go back up to previous blank line."
  (interactive "p")
  (ar/forward-paragraph (- n)))

;; Centers text, distributing blank space.
(use-package olivetti
  :ensure t
  :commands olivetti-mode)

(use-package window
  :bind (("C-x 2" . ar/vsplit-last-buffer)
         ("C-x 3" . ar/hsplit-last-buffer)
         ("M-o" . other-window)
         ("M-}" . next-buffer)
         ("M-{" . next-buffer))
  :chords (("BB" . other-window)
           ("JJ" . ar/switch-to-previous-buffer))
  :validate-custom
  (split-width-threshold nil) ;; Prevent horizontal window splits.
  ;; Note: window.el is not provided at the end of the file.
  ;; Using init purely for loading functions.
  ;; Do NOT use :config. It will not be loaded.
  :init
  ;; From http://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer
  (defun ar/switch-to-previous-buffer ()
    "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) 1)))
  ;; From http://www.reddit.com/r/emacs/comments/25v0eo/you_emacs_tips_and_tricks/chldury

  (defun ar/vsplit-last-buffer ()
    "Vertically splitting the screen and open the previous buffer instead of identical buffers."
    (interactive)
    (split-window-vertically)
    (other-window
     1 nil)
    (switch-to-next-buffer))

  (defun ar/hsplit-last-buffer ()
    "Horizontally splitting the screen and open the previous buffer instead of identical buffers."
    (interactive)
    (split-window-horizontally)
    (other-window 1 nil)
    (switch-to-next-buffer))
  :config
  (add-to-list 'display-buffer-alist
               (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil))))

;; In addition to highlighting symbols, we get navigation between them.
(use-package symbol-overlay
  :ensure t
  :hook ((prog-mode . symbol-overlay-mode)
         ;; (protobuf-mode . symbol-overlay-mode)
         )
  :bind (:map symbol-overlay-mode-map
              (("M-n" . symbol-overlay-jump-next)
               ("M-p" . symbol-overlay-jump-prev)))
  :config
  ;; Override overlay background color with default background
  ;; to get rid of overlay bounding box.
  (set-face-attribute 'symbol-overlay-default-face nil
                      :background (face-attribute 'default :background)
                      :foreground "yellow"))

;; Enables C-x o (repeat o) for cycling through windows.
(use-package repeat
  :config
  (repeat-mode +1))

(use-package smart-jump
  :ensure t
  :bind ("M-." . smart-jump-go)
  :commands smart-jump-go
  :config
  (use-package dumb-jump
    :ensure t
    :validate-custom
    (dumb-jump-selector 'ivy)
    (dumb-jump-force-searcher 'rg)
    (dumb-jump-max-find-time 5)
    :config
    (add-to-list 'dumb-jump-project-denoters "TAGS"))

  (smart-jump-setup-default-registers)

  ;; Disabling as now handled by eglot.
  ;; (smart-jump-register
  ;;  :modes '(swift-mode objc-mode)
  ;;  :jump-fn 'counsel-etags-find-tag-at-point
  ;;  :pop-fn 'pop-tag-mark
  ;;  :should-jump t
  ;;  :heuristic 'point
  ;;  :async t)

  (smart-jump-register
   :modes '(emacs-lisp-mode lisp-interaction-mode)
   :jump-fn 'elisp-slime-nav-find-elisp-thing-at-point
   :pop-fn 'pop-tag-mark
   :should-jump t
   :heuristic 'error
   :async nil)

  (smart-jump-register :modes 'bazel-mode)

  (defun adviced:dumb-jump-run-command (run-command-fun &rest r)
    "Ignore RUN-COMMAND-FUN and R if project path in excluded-args."
    (let ((proj (nth 1 r))
          (exclude-args (nth 4 r)))
      (unless (seq-contains-p exclude-args proj)
        (apply run-command-fun r))))

  ;; This advice is handy for very large repositories, as it enables whitelisting
  ;; only relevant directories. It ignores repository root if explicitly excluded
  ;; and thus operates only on explicit additions.
  ;;
  ;; For example, in .dumbjump file:
  ;; -.
  ;; +sub1
  ;; +sub2

  (advice-add #'dumb-jump-run-command
              :around
              #'adviced:dumb-jump-run-command))

;; Programmatically get the visible end of window.
(use-package window-end-visible
  :ensure t
  :defer
  :config
  (defmacro ar/with-marked-visible-buffer (f)
    "Mark all visible lines in buffer. Unlike `mark-whole-buffer',
 invisible lines are not marked."
    (interactive)
    ;; Delay, in case invoking via helm/ivy and window is temporarily smaller.
    `(run-with-timer 0.001 nil
                     (lambda ()
                       (save-excursion
                         (save-restriction
                           (set-mark (window-start))
                           (goto-char (window-end-visible))
                           (activate-mark)
                           (funcall ,f)
                           (deactivate-mark)))))))

(use-package avy
  :ensure t
  :bind (("M-e" . avy-goto-char-timer)))

(use-package subword
  :bind (("C-c c" . ar/text-capitalize-word-toggle)
         ("M-DEL" . ar/text-backward-delete-subword))
  :hook ((prog-mode . subword-mode))
  :config
  ;; From http://stackoverflow.com/questions/6133799/delete-a-word-without-adding-it-to-the-kill-ring-in-emacs
  (defun ar/text-backward-delete-subword (arg)
    "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
    (interactive "p")
    (delete-region (point)
                   (progn
                     (subword-backward arg)
                     (point))))

  ;; Based on http://oremacs.com/2014/12/25/ode-to-toggle
  (defun ar/text-capitalize-word-toggle ()
    "Capitalize word toggle."
    (interactive)
    (let ((start
           (car
            (bounds-of-thing-at-point 'symbol))))
      (if start
          (save-excursion
            (goto-char start)
            (funcall
             (if (eq (char-after) (upcase (char-after)))
                 'downcase-region
               'upcase-region)
             start (1+ start)))
        (capitalize-word -1)))))

(use-package goto-line-preview
  :ensure t
  :commands goto-line-preview
  :config
  (global-set-key [remap goto-line] 'goto-line-preview))

(use-package replace
  :commands occur
  :config
  (add-hook 'occur-hook
            '(lambda ()
               ;; Focus occur by default.
               (switch-to-buffer-other-window "*Occur*")
               ;; Enable follow mode by default.
               (next-error-follow-minor-mode +1))))

(use-package xref
  :defer
  :config
  ;; I accidentally press these when I meant "global-map M->"
  ;; for end-of-buffer. Unsetting.
  (unbind-key "M-?" global-map)
  (unbind-key "M-?" global-map))

(use-package yafolding
  :ensure t
  :hook
  ((prog-mode . yafolding-mode)
   (sgml-mode . yafolding-mode))
  :bind (:map
         prog-mode-map
         ("<tab>" . ar/indent-for-tab-command-dwim))
  :config
  (use-package tab-jump-out
    :ensure t)

  (defun ar/indent-for-tab-command-dwim (&optional prefix)
      "Like `indent-for-tab-command' but jumps out or folds/unfolds if no change"
      (interactive "P")
      (let ((hash-before (buffer-hash))
            (region-active (region-active-p))
            (point-before (point)))
        (if (and yafolding-mode
                 prefix)
            (yafolding-toggle-all)
          (indent-for-tab-command prefix)
          (cond ((and (eq point-before (point))
                      (not region-active)
                      ;; buffer is unchanged.
                      (string-equal hash-before (buffer-hash))
                      (char-after)
                      (seq-contains-p tab-jump-out-delimiters (char-to-string (char-after))))
                 (forward-char prefix))
                ((and yafolding-mode
                      (eq point-before (point))
                      (not region-active)
                      ;; buffer is unchanged.
                      (string-equal hash-before (buffer-hash)))
                 (call-interactively #'yafolding-toggle-element)))))))


(defun ar/indent-for-tab-command-dwim (&optional prefix)
      "Like `indent-for-tab-command' but jumps out or folds/unfolds if no change"
      (interactive "P")
      (let ((hash-before (buffer-hash))
            (region-active (region-active-p))
            (point-before (point)))
        (if (and yafolding-mode
                 prefix)
            (yafolding-toggle-all)
          (indent-for-tab-command prefix)
          (cond ((and (eq point-before (point))
                      (not region-active)
                      ;; buffer is unchanged.
                      (string-equal hash-before (buffer-hash))
                      (char-after)
                      (seq-contains-p tab-jump-out-delimiters (char-to-string (char-after))))
                 (forward-char prefix))
                ((and yafolding-mode
                      (eq point-before (point))
                      (not region-active)
                      ;; buffer is unchanged.
                      (string-equal hash-before (buffer-hash)))
                 (call-interactively #'yafolding-toggle-element))))))
