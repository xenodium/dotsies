;;; -*- lexical-binding: t; -*-

;; Prevent Extraneous Tabs.
;; From http://www.gnu.org/software/emacs/manual/html_node/eintr/Indent-Tabs-Mode.html
(setq-default indent-tabs-mode nil)

;; Show keystrokes earlier (ie. C-x)
(setq echo-keystrokes 0.1)

;; No need to keep duplicates in prompt history.
(setq history-delete-duplicates t)

;; Shows keyboard macros as Emacs lisp.
(use-package elmacro
  :ensure t
  :commands (elmacro-show-last-macro
             elmacro-show-last-commands
             elmacro-clear-command-history))

(use-package change-inner
  :ensure t
  :bind (("M-+" . change-outer)
         ("M-_" . change-inner)))

(use-package expand-region
  :ensure t
  :bind ("C-c w" . er/expand-region)
  :config
  (defun ar/add-mode-expansions ()
    ;; Making expansion greedier by removing
    ;; `er/mark-word' and other mark functions.
    (setq-local er/try-expand-list
                '(er/mark-symbol
                  er/mark-outside-quotes
                  er/mark-outside-pairs
                  er/mark-comment
                  er/mark-url
                  er/mark-email
                  er/mark-defun)))
  (er/enable-mode-expansions 'text-mode 'ar/add-mode-expansions)
  (er/enable-mode-expansions 'prog-mode 'ar/add-mode-expansions))

;; underscore -> UPCASE -> CamelCase conversion of names.
(use-package string-inflection
  :ensure t
  :bind (:map
         prog-mode-map
         ("C-M-j" . string-inflection-cycle)
         :map
         c-mode-base-map
         ("C-M-j" . string-inflection-cycle)))

(use-package dabbrev
  :commands (dabbrev-expand)
  :validate-custom
  ;; Case-sensitive fold search search (ie. M-/ to autocomplete).
  (dabbrev-case-fold-search nil))

(use-package drag-stuff
  :ensure t
  :bind (("M-<up>" . drag-stuff-up)
         ("M-<down>" . drag-stuff-down)))

;; Remember history of things across launches (ie. kill ring).
;; From https://www.wisdomandwonder.com/wp-content/uploads/2014/03/C3F.html
(use-package savehist
  :defer 10
  :validate-custom
  (savehist-file "~/.emacs.d/savehist")
  (savehist-save-minibuffer-history t)
  (history-length 20000)
  :config
  (mapc (lambda (ring)
          (add-to-list 'savehist-additional-variables
                       ring))
        '(kill-ring
          search-ring
          regexp-search-ring
          last-kbd-macro
          shell-command-history
          log-edit-comment-ring))
  (savehist-mode +1))

(use-package whitespace
  ;; Automatically remove whitespace on saving.
  :hook ((before-save . whitespace-cleanup)
         (prog-mode . ar/whitespace-mode-enable))
  :validate-custom
  ;; When nil, fill-column is used instead.
  (whitespace-line-column nil)
  ;; Highlight empty lines, TABs, blanks at beginning/end, lines
  ;; longer than fill-column, and trailing blanks.
  (whitespace-style '(face empty tabs lines-tail trailing))
  :config
  (defun ar/whitespace-mode-enable ()
    "Delayed enabling of whitespace-mode to ensure fill-column is set for loaded buffer."
    (whitespace-mode -1)
    ;; Trying without, seems to be slow.
    ;; (setq-local show-trailing-whitespace t)
    (let ((buffer (current-buffer)))
      (run-with-timer 1 nil
                      (lambda ()
                        ;; Guard against deleted buffers.
                        (when (if (stringp buffer)
                                  (get-buffer buffer)
                                (buffer-name buffer))
                          (with-current-buffer buffer
                            (whitespace-mode +1)))))))

  (set-face-attribute 'whitespace-line nil
                      :foreground "DarkOrange1"
                      :background nil))

(use-package anchored-transpose
  :commands anchored-transpose
  :init
  ;; which used to be transpose-words
  (global-unset-key (kbd "M-t"))
  :bind
  (("M-t r" . anchored-transpose)
   ("M-t l" . transpose-lines)
   ("M-t w" . transpose-words)))

(use-package smartparens
  :ensure t
  :init
  (when (display-graphic-p)
    (define-key input-decode-map (kbd "C-[") [C-\[]))
  :bind
  (:map
   smartparens-strict-mode-map
   ;; I prefer keeping C-w to DWIM kill, provided by
   ;; `adviced:kill-region-advice'. Removing remap.
   ([remap kill-region] . kill-region)
   :map prog-mode-map
   ("C-<right>" . sp-forward-slurp-sexp)
   ("C-<left>" . sp-forward-barf-sexp)
   ("M-<right>" . sp-backward-barf-sexp)
   ("M-<left>"  . sp-backward-slurp-sexp)
   :map smartparens-mode-map
   ([remap kill-region] . kill-region)
   ("C-c e" . sp-change-enclosing)
   ("M-'" . ar/rewrap-sexp-dwim)
   ("M-k" . sp-backward-kill-sexp)
   ("C-M-f" . ar/forward-sexp)
   ("C-M-b" . ar/backward-sexp)
   ("C-M-n" . sp-forward-sexp)
   ("C-M-p" . sp-backward-sexp)
   ("C-M-a" . sp-beginning-of-sexp)
   ("C-M-e" . sp-end-of-sexp)
   ("C-M-u" . ar/backward-up-sexp)
   ("C-M-d" . sp-down-sexp)
   ("C-M-t" . sp-transpose-sexp)
   ("M-t t" . sp-transpose-sexp)
   ("<C-M-backspace>" . backward-kill-sexp)
   ("C-M-k" . ar/kill-sexp)
   ("C-M-w" . sp-copy-sexp))
  ;; Add to minibuffer also.
  :hook ((minibuffer-setup . smartparens-mode)
         (prog-mode . smartparens-strict-mode)
         (protobuf-mode . smartparens-strict-mode)
         (ielm-mode . smartparens-strict-mode)
         (eshell-mode . smartparens-strict-mode))
  :config
  (load "~/.emacs.d/features/config-smartparens"))

(use-package region-bindings-mode
  :ensure t
  :defer 10 ;; Don't lazy-load, to enable for regions.
  :bind (:map region-bindings-mode-map
              ("a" . mc/mark-all-dwim)
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
  (region-bindings-mode-enable))

;; Display chars/lines or row/columns in the region.
(use-package region-state
  :ensure t
  :defer 20
  :config
  (region-state-mode))

(use-package multiple-cursors :ensure t
  :after region-bindings-mode
  :commands (ar/set-mc/insert-numbers-starting-value
             multiple-cursors-mode)
  :init
  (global-unset-key (kbd "M-<down-mouse-1>"))
  :bind (("C-c a" . mc/mark-all-dwim)
         ("C-c n" . mc/mark-more-like-this-extended)
         ("M-<mouse-1>" . mc/add-cursor-on-click))
  :config
  (defalias 'mc/mark-all-lines-in-region 'mc/edit-lines)

  (defun ar/set-mc/insert-numbers-starting-value ()
    "Set starting value for inserting numbers using multiple cursors."
    (interactive)
    (set-variable 'mc/insert-numbers-default
                  (read-number "Starting value: ")))
  ;; MC-friendly packages.
  (use-package phi-search :ensure t)
  (use-package phi-rectangle :ensure t)
  (use-package phi-search-mc :ensure t
    :config
    (phi-search-mc/setup-keys)))

(use-package hungry-delete
  :defer 5
  :ensure t
  :bind (("<C-backspace>" . ar/backwards-delete-sexp-whitespace))
  :config
  (defun ar/backwards-delete-sexp-whitespace (arg)
    "Deletes whitespace character prior to current sexp.  With ARG, delete it all."
    (interactive "P")
    (save-excursion
      (save-restriction
        (backward-sexp 1)
        (if arg
            (let ((current-prefix-arg nil))
              (call-interactively 'hungry-delete-backward nil))
          (delete-backward-char 1)))))

  (global-hungry-delete-mode))

(use-package delsel
  :defer 5
  :config
  ;; Override selection with new text.
  (delete-selection-mode +1))

(use-package paren
  :defer 5
  :validate-custom
  ;; Without this matching parens aren't highlighted in region.
  (show-paren-priority -50)
  (show-paren-delay 0.3)
  ;; Highlight entire bracket expression.
  (show-paren-style 'expression)
  :config
  (show-paren-mode +1))

(use-package ar-text
  :bind (("C-c c" . ar/text-capitalize-word-toggle)
         ("C-c r" . set-rectangular-region-anchor)
         ("M-DEL" . ar/text-backward-delete-subword)))

;; Monitor system clipboard and append kill ring.
(use-package clipmon
  :ensure t
  :defer 20
  :config
  (clipmon-mode))

;; Copy formatted region as source block for AsciiDoc Bitbucket Disqus
;; GitHub GitLab HipChat HTML JIRA Markdown MediaWiki Org-mode POD
;; reStructuredText Slack.
(use-package copy-as-format
  :ensure t
  :init
  (defalias 'copy-as-symbol-reddit 'copy-as-format-markdown)
  :commands
  (copy-as-format-asciidoc
   copy-as-format-bitbucket
   copy-as-format-disqus
   copy-as-format-github
   copy-as-format-gitlab
   copy-as-format-hipchat
   copy-as-format-html
   copy-as-format-jira
   copy-as-format-markdown
   copy-as-format-mediawiki
   copy-as-format-org-mode
   copy-as-format-pod
   copy-as-format-rst
   copy-as-format-slack))

;; Make kill ring persistent across sessions.
(use-package savekill
  :ensure t
  :defer 20)

(use-package simple
  :bind (("M-u" . upcase-dwim)
         ("M-l" . downcase-dwim)
         ("M-C-y" . ar/yank-line-below))
  :validate-custom
  (kill-ring-max 1000)
  (set-mark-command-repeat-pop t "C-u is only needed once in C-u C-SPC to pop multiple locations.")
  (save-interprogram-paste-before-kill t "Increase mark ring size.")
  (kill-do-not-save-duplicates t "Don't bother saving things to the kill-ring twice, remove duplicates.")
  (idle-update-delay 2 "Wait a bit longer than the default (0.5 seconds) before assuming Emacs is idle.")
  (global-mark-ring-max 500 "Increase mark ring size.")
  ;; From https://github.com/daschwa/emacs.d
  ;; nil means no limit. Always show result.
  (eval-expression-print-level nil)
  :config
  (load "~/.emacs.d/features/config-simple"))

;; Open rc files with conf-mode.
(use-package conf-mode
  :mode ("rc$" . conf-mode))

;; Handles escaping regexes from input. For example: no need for \(\)
(use-package pcre2el
  :ensure t
  :defer 30)

(use-package re-builder
  :defer 30
  :validate-custom
  (reb-re-syntax 'string))

(use-package diverted
  :defer 20
  :config
  (add-to-list 'diverted-events
               (make-diverted-event :from 'er/expand-region
                                    :to 'indent-for-tab-command
                                    :breadcrumb (lambda ()
                                                  (diverted--pop-to-mark-command 2))))
  (diverted-mode +1))

;; No double escaping needed.
