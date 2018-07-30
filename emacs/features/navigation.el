(use-package isearch
  :commands (isearch-forward isearch-backward)
  :defer 5
  :preface
  (provide 'isearch)
  :config
  (use-package char-fold)

  (vsetq search-default-mode #'char-fold-to-regexp)

  ;; Prepopulate isearch with selectionn.
  ;; From http://www.reddit.com/r/emacs/comments/2amn1v/isearch_selected_text
  (defadvice isearch-mode (around isearch-mode-default-string
                                  (forward &optional regexp op-fun recursive-edit word-p) activate)
    "Enable isearch to start with current selection."
    (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
        (progn
          (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
          (deactivate-mark)
          ad-do-it
          (if (not forward)
              (isearch-repeat-backward)
            (goto-char (mark))
            (isearch-repeat-forward)))
      ad-do-it)))

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

(use-package swiper
  :ensure t
  :bind ("M-i" . swiper))

;; Smarter move to beginning/end of line.
(use-package mwim
  :ensure t
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

(use-package centered-cursor-mode
  :defer 0.1
  :ensure t
  :pin melpa
  :init
  ;; Workaround to use centered-cursor-mode in --nw.
  (defvar mouse-wheel-mode nil)
  :config
  (global-centered-cursor-mode +1))

(use-package window
  :chords (("BB" . other-window)
           ("JJ" . ar/switch-to-previous-buffer))
  :init
  ;; From http://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer
  (defun ar/switch-to-previous-buffer ()
    "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) 1))))

;; In addition to highlighting, we get navigation between
(use-package highlight-symbol
  :hook ((prog-mode . highlight-symbol-mode)
         (prog-mode . highlight-symbol-nav-mode))
  :ensure t
  :bind (:map highlight-symbol-nav-mode-map
              (("M-n" . highlight-symbol-next)
               ("M-p" . highlight-symbol-prev)))
  :config
  (set-face-attribute 'highlight-symbol-face nil
                      :background "default"
                      :foreground "yellow")
  (vsetq highlight-symbol-idle-delay 0.2)
  (vsetq highlight-symbol-on-navigation-p t))
