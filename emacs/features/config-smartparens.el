;;; -*- lexical-binding: t; -*-

(defun ar/rewrap-sexp-dwim (prefix)
  "Like `sp-rewrap-sexp', but RET, DEL, SPC, and C-d remove pair.
With PREFIX, add an outer pair around existing pair."
  (interactive "P")
  (let* ((pair-prefix (format-kbd-macro (vector (read-event "Rewrap with: " t))))
         (clear-p (or (equal pair-prefix "RET")
                      (equal pair-prefix "DEL")
                      (equal pair-prefix "SPC")
                      (equal pair-prefix "C-d")))
         (available-pairs (sp--get-pair-list-context 'wrap))
         (pair (--first (equal pair-prefix (car it)) available-pairs)))
    (cond (clear-p
           (when (sp-get-enclosing-sexp)
             (sp-unwrap-sexp)))
          (pair
           (if (sp-get-enclosing-sexp)
               (sp-rewrap-sexp pair
                               prefix)
             (save-excursion
               (sp-wrap-with-pair (car pair))))))))

;; https://www.reddit.com/r/emacs/comments/dewzuy/weekly_tipstricketc_thread/f3be8kq?utm_source=share&utm_medium=web2x
(defun ar/backward-up-sexp (a)
  "Backwards up multiple sexps.
   prefix command interpretation:
     0    → to beginning of all nested sexps
     -    → to end of all nested sexps
     x|+x → x-times go back out of sexps to beginning
     -x   → x-times go out of sexps to end
     universal-command interpreted as 0"
  (interactive "P")
  (condition-case err
      (let ((arg)
            (loop))
        (cond
         ((null a) ;; back-up once
          (setq arg -1
                loop nil))
         ((eq a '-) ;; up to end of all sexps
          (setq arg 1
                loop t))
         ((numberp a)
          (cond
           ((= a 0) ;; back-up to begin of all sexps
            (setq arg -1
                  loop t))
           (t (setq arg (- a) ;; do it a times
                    loop nil))))
         (t (setq arg -1 ;; interpret `universal-command'
                  loop t)))
        (while (progn  ;; do-while loop
                 (up-list arg t t)
                 loop)))
    (scan-error ;; stay quiet
     nil)))
(defun ar/kill-sexp (&optional arg)
  "If inside symbol, kill from position to end of symbol.  With any ARG, kill current sexp."
  (interactive "P")
  (if (or arg
          (not (sp-point-in-symbol)))
      (sp-kill-sexp)
    (kill-sexp)))
(defun ar/forward-sexp (&optional arg)
  (interactive "P")
  (if arg
      (skip-syntax-forward "^ ()")
    (sp-forward-sexp)))

(defun ar/backward-sexp (&optional arg)
  (interactive "P")
  (if arg
      (skip-syntax-backward "^ ()")
    (sp-backward-sexp)))

(require 'smartparens-config)

(require 'smartparens-html)

(require 'smartparens-python)

;; Removes \\(
(sp-local-pair 'swift-mode "\\\\(" nil :actions nil)

(sp-local-pair 'swift-mode "\\(" ")")

(defun ar/create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(sp-local-pair 'prog-mode "{" nil :post-handlers '((ar/create-newline-and-enter-sexp "RET")))

(sp-local-pair 'prog-mode "[" nil :post-handlers '((ar/create-newline-and-enter-sexp "RET")))

(sp-local-pair 'prog-mode "(" nil :post-handlers '((ar/create-newline-and-enter-sexp "RET")))

(defun ar/sp-prog-skip-match-angle-bracket (_ms _mb me)
  "Non-nil if we should ignore the bracket as valid delimiter."
  (save-excursion
    (goto-char me)
    (let ((on-fn-return-type
           (sp--looking-back-p (rx "->") nil))
          (on-match-branch
           (sp--looking-back-p (rx "=>") nil))
          (on-comparison
           (sp--looking-back-p (rx (or
                                    (seq space "<")
                                    (seq space ">")
                                    (seq space "<<")
                                    (seq space ">>")))
                               nil)))
      (or on-comparison on-fn-return-type on-match-branch))))

(defun ar/sp-prog-filter-angle-brackets (_id action context)
  "Non-nil if we should allow ID's ACTION in CONTEXT for angle brackets."
  ;; See the docstring for `sp-pair' for the possible values of ID,
  ;; ACTION and CONTEXT.
  (cond
   ;; Inside strings, don't do anything with < or >.
   ((eq context 'string)
    nil)
   ;; Don't do any smart pairing inside comments either.
   ((eq context 'comment)
    nil)
   ;; Otherwise, we're in code.
   ((eq context 'code)
    (let ((on-fn-return-type
           (looking-back (rx "->") nil))
          (on-match-branch
           (looking-back (rx "=>") nil))
          (on-comparison
           (looking-back (rx (or
                              (seq space "<")
                              (seq space ">")
                              (seq space "<<")
                              (seq space ">>")))
                         nil)))
      (cond
       ;; Only insert a matching > if we're not looking at a
       ;; comparison.
       ((eq action 'insert)
        (and (not on-comparison) (not on-fn-return-type) (not on-match-branch)))
       ;; Always allow wrapping in a pair if the region is active.
       ((eq action 'wrap)
        (not on-match-branch))
       ;; When pressing >, autoskip if we're not looking at a
       ;; comparison.
       ((eq action 'autoskip)
        (and (not on-comparison) (not on-fn-return-type) (not on-match-branch)))
       ;; Allow navigation, highlighting and strictness checks if it's
       ;; not a comparison.
       ((eq action 'navigate)
        (and (not on-comparison) (not on-fn-return-type) (not on-match-branch))))))))

(sp-local-pair 'protobuf-mode "'" "'")

(sp-local-pair 'prog-mode "/*" "*/")

(sp-local-pair 'prog-mode "<" ">"
               :when '(ar/sp-prog-filter-angle-brackets)
               :skip-match 'ar/sp-prog-skip-match-angle-bracket)

(defun adviced:kill-region-advice (orig-fun &rest r)
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

(advice-add #'kill-region
            :around
            #'adviced:kill-region-advice)
