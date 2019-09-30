(require 'ar-vsetq)
(require 'ar-csetq)
(require 'dash)

;; From http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun ar/narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or defun,
whichever applies first. Narrowing to org-src-block actually
calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is
already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if you
         ;; don't want it.
         (cond ((ignore-errors (org-edit-src-code))
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

(bind-key "C-x n n" #'ar/narrow-or-widen-dwim)

;; Smarter move to beginning/end of line.
(use-package mwim
  :ensure t
  :bind (:map prog-mode-map
              ("C-a" . mwim-beginning-of-code-or-line)
              ("C-e" . mwim-end-of-code-or-line)))

;; Centers text, distributing blank space.
(use-package olivetti :ensure t)

;; Potential native replacement for centered-cursor-mode.
;; Breaks org mode.
;; (setq scroll-preserve-screen-position t
;;       scroll-conservatively 0
;;       maximum-scroll-margin 0.5
;;       scroll-margin 99999)

(use-package centered-cursor-mode
  :ensure t
  :commands (centered-cursor-mode global-centered-cursor-mode)
  :init
  ;; Workaround to use centered-cursor-mode in --nw.
  (defvar mouse-wheel-mode nil))

(use-package window
  :bind (("C-x 2" . ar/vsplit-last-buffer)
         ("C-x 3" . ar/hsplit-last-buffer)
         ("C-<tab>" . other-window))
  :chords (("BB" . other-window)
           ("JJ" . ar/switch-to-previous-buffer))
  :custom
  (split-width-threshold nil) ;; Prevent horizontal window splits.
  :config
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
    (switch-to-next-buffer)))

;; In addition to highlighting symbols, we get navigation between them.
(use-package symbol-overlay
  :ensure t
  :hook (prog-mode . symbol-overlay-mode)
  :bind (:map symbol-overlay-mode-map
              (("M-n" . symbol-overlay-jump-next)
               ("M-p" . symbol-overlay-jump-prev)))
  :config
  ;; Override overlay background color with default background
  ;; to get rid of overlay bounding box.
  (set-face-attribute 'symbol-overlay-default-face nil
                      :background (face-attribute 'default :background)
                      :foreground "yellow"))

(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window))
  :config
  (ar/vsetq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  ;; Use larger characters for ace window shortcuts.
  ;; From http://oremacs.com/2015/02/27/ace-window-leading-char
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0))))))

(use-package smart-jump
  :ensure t
  :commands smart-jump-go
  :config
  (smart-jump-setup-default-registers)
  (ar/csetq dumb-jump-selector 'popup)
  ;; (ar/csetq dumb-jump-selector 'ivy)
  (ar/csetq dumb-jump-force-searcher 'ag)
  (ar/csetq dumb-jump-max-find-time 5)

  (defun ar/dumb-jump-run-command-advice (run-command-fun &rest r)
    "Ignore RUN-COMMAND-FUN and R if project path in excluded-args."
    (let ((proj (nth 1 r))
          (exclude-args (nth 4 r)))
      (unless (-contains-p exclude-args proj)
        (apply run-command-fun r))))

  ;; This advice is handy for very large repositories, as it enables whitelisting
  ;; only relevant directories. It ignores repository root if explicitly excluded
  ;; and thus operates only on explicit additions.
  ;;
  ;; For example, in .dumbjump file:
  ;; -.
  ;; +sub1
  ;; +sub2

  (advice-add 'dumb-jump-run-command
              :around
              'ar/dumb-jump-run-command-advice)

  :bind ("M-." . smart-jump-go))

;; Programmatically get the visible end of window.
(use-package window-end-visible
  :ensure t
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
  :config
  (global-subword-mode +1))

(use-package goto-line-preview
  :ensure t
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
  :config
  ;; I accidentally press these when I meant "global-map M->"
  ;; for end-of-buffer. Unsetting.
  (unbind-key "M-?" global-map)
  (unbind-key "M-?" global-map))
