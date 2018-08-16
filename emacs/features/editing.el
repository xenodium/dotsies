(require 'ar-vsetq)
(require 'ar-csetq)

;; Prevent Extraneous Tabs.
;; From http://www.gnu.org/software/emacs/manual/html_node/eintr/Indent-Tabs-Mode.html
(setq-default indent-tabs-mode nil)

;; Show keystrokes earlier (ie. C-x)
(setq echo-keystrokes 0.1)

;; No need to keep duplicates in prompt history.
(setq history-delete-duplicates t)

(use-package expand-region
  :ensure t
  :bind ("C-c w" . er/expand-region))

(use-package dabbrev
  :config
  ;; Case-sensitive fold search search (ie. M-/ to autocomplete).
  (ar/vsetq dabbrev-case-fold-search nil))

(defun ar/yank-line-below ()
  "Yank to line below."
  (interactive)
  (save-excursion
    (move-end-of-line nil)
    (newline)
    (yank))
  (next-line))

(bind-key "M-C-y" #'ar/yank-line-below)

(use-package drag-stuff
  :ensure t
  :bind (("M-<up>" . drag-stuff-up)
         ("M-<down>" . drag-stuff-down)))

;; Remember history of things across launches (ie. kill ring).
  ;; From https://www.wisdomandwonder.com/wp-content/uploads/2014/03/C3F.html
  (use-package savehist
    :defer 2
    :config
    (ar/vsetq savehist-file "~/.emacs.d/savehist")
    (ar/vsetq savehist-save-minibuffer-history t)
    (ar/vsetq history-length 1000)
    (ar/vsetq savehist-additional-variables
           '(kill-ring
             search-ring
             regexp-search-ring
             log-edit-comment-ring))
    (savehist-mode +1))
  (use-package whitespace
    :defer 5
    ;; Automatically remove whitespace on saving.
    :hook ((before-save . whitespace-cleanup)
           (prog-mode . whitespace-mode))
    :config
    ;; When nil, fill-column is used instead.
    (ar/vsetq whitespace-line-column nil)
    ;; Highlight empty lines, TABs, blanks at beginning/end, lines
    ;; longer than fill-column, and trailing blanks.
    (ar/vsetq whitespace-style '(face empty tabs lines-tail trailing))
    (ar/vsetq show-trailing-whitespace t)
    (set-face-attribute 'whitespace-line nil
                        :foreground "DarkOrange1"
                        :background "default"))
  (use-package smartparens
    :ensure t
    :defer 0.01
    ;; Add to minibuffer also.
    :hook ((minibuffer-setup . smartparens-mode)
           (prog-mode . smartparens-strict-mode)
           (eshell-mode . smartparens-strict-mode))
    :config
    (require 'smartparens-config)
    (require 'smartparens-html)
    (require 'smartparens-python)

    ;; Removes \\(
    (sp-local-pair 'swift-mode "\\\\(" nil :actions nil)
    (sp-local-pair 'swift-mode "\\(" ")")
    (sp-local-pair 'swift-mode "<" ">")

    (defun ar/create-newline-and-enter-sexp (&rest _ignored)
      "Open a new brace or bracket expression, with relevant newlines and indent. "
      (newline)
      (indent-according-to-mode)
      (forward-line -1)
      (indent-according-to-mode))

    (sp-local-pair 'prog-mode "{" nil :post-handlers '((ar/create-newline-and-enter-sexp "RET")))
    (defun ar/kill-region-advice-fun (orig-fun &rest r)
      "Advice function around `kill-region' (ORIG-FUN and R)."
      (if (or (null (nth 2 r)) ;; Consider kill-line (C-k).
              mark-active)
          (apply orig-fun r)
        ;; Kill entire line.
        (let ((last-command (lambda ())) ;; Override last command to avoid appending to kill ring.
              (offset (- (point)
                         (line-beginning-position))))
          (apply orig-fun (list (line-beginning-position)
                                (line-end-position)
                                nil))
          (delete-char 1)
          (forward-char (min offset
                             (- (line-end-position)
                                (line-beginning-position)))))))

    (advice-add 'kill-region
                :around
                'ar/kill-region-advice-fun)

    ;; I prefer keeping C-w to DWIM kill, provided by
    ;; `ar/kill-region-advice-fun'. Removing remap.
    ;;   (define-key smartparens-strict-mode-map [remap kill-region] nil)

    (defun ar/smartparens-wrap-square-bracket (arg)
      "[] equivalent of `paredit-wrap-round'."
      (interactive "P")
      (save-excursion
        (unless (sp-point-in-symbol)
          (backward-char))
        (sp-wrap-with-pair "["))
      (insert " "))

    :bind (:map smartparens-strict-mode-map
                ([remap kill-region] . kill-region)
                ("C-c <right>" . sp-forward-slurp-sexp)
                ("C-c <left>" . sp-forward-barf-sexp)
                ("M-[" . sp-rewrap-sexp)
                :map smartparens-mode-map
                ("M-]" . ar/smartparens-wrap-square-bracket)))

(use-package region-bindings-mode
  :ensure t
  :defer 2
  :config
  (region-bindings-mode-enable))

;; Display chars/lines or row/columns in the region.
(use-package region-state
  :ensure t
  :defer 2
  :config
  (region-state-mode))

(use-package multiple-cursors :ensure t
  :after region-bindings-mode
  :init
  (global-unset-key (kbd "M-<down-mouse-1>"))
  :bind (("C-c a" . mc/mark-all-like-this)
         ("C-c n" . mc/mark-more-like-this-extended)
         ("M-1" . mc/mark-next-like-this)
         ("M-!" . mc/unmark-next-like-this)
         ("M-2" . mc/mark-previous-like-this)
         ("M-@" . mc/unmark-previous-like-this)
         ("M-<mouse-1>" . mc/add-cursor-on-click))
  :bind (:map region-bindings-mode-map
              ("a" . mc/mark-all-like-this)
              ("p" . mc/mark-previous-like-this)
              ("n" . mc/mark-next-like-this)
              ("P" . mc/unmark-previous-like-this)
              ("N" . mc/unmark-next-like-this)
              ("m" . mc/mark-more-like-this-extended)
              ("h" . mc-hide-unmatched-lines-mode)
              ("\\" . mc/vertical-align-with-space)
              ("#" . mc/insert-numbers) ; use num prefix to set the starting number
              ("^" . mc/edit-beginnings-of-lines)
              ("$" . mc/edit-ends-of-lines))
  :config
  ;; MC-friendly packages.
  (use-package phi-search :ensure t)
  (use-package phi-rectangle :ensure t)
  (use-package phi-search-mc :ensure t
    :config
    (phi-search-mc/setup-keys)))

;; From https://github.com/daschwa/emacs.d
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single
line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-end-position)))))

(use-package hungry-delete
  :defer 5
  :ensure t
  :config (global-hungry-delete-mode))

(use-package delsel
  :defer 5
  :config
  ;; Override selection with new text.
  (delete-selection-mode +1))

;; Highlight matching parenthesis.
(use-package paren
  :ensure t
  :defer 5
  :config
  (show-paren-mode +1)
  ;; Without this matching parens aren't highlighted in region.
  (ar/vsetq show-paren-priority -50)
  (ar/vsetq show-paren-delay 0)
  ;; Highlight entire bracket expression.
  (ar/vsetq show-paren-style 'expression)
  (set-face-attribute 'show-paren-match nil
                      :background "default"
                      :foreground "#FA009A"))

(use-package ar-text
  :bind (("C-c c" . ar/text-capitalize-word-toggle)
         ("C-c r" . set-rectangular-region-anchor)
         ("M-DEL" . ar/backward-delete-subword)
         ("<C-backspace>" . ar/backward-delete-subword)
         ("C-x C-d" . ar/duplicate-line)))

(use-package simple
  :config
  ;; Save external clipboard before killing other text in Emacs.
  (ar/vsetq save-interprogram-paste-before-kill t)

  ;; Don't bother saving things to the kill-ring twice, remove duplicates.
  (ar/csetq kill-do-not-save-duplicates t)

  ;; Wait a bit longer than the default (0.5 seconds) before assuming Emacs is idle.
  (ar/csetq idle-update-delay 2)

  ;; Increase mark ring size.
  (ar/csetq global-mark-ring-max 500))

;; Open rc files with conf-mode.
(use-package conf-mode
  :mode ("rc$" . conf-mode))
