;;; company-org-block.el --- Company backend to complete org blocks.

;;; Commentary:
;; Enable in org-mode to complete source blocks.
;;
;; Completes only when there's a "<" before.

;;; Code:

(defun company-org-block (command &optional arg &rest ignored)
  "Complete org babel languages into source blocks."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-org-block))
    (prefix (company-org-block--grap-symbol-cons))
    (candidates (company-org-block--candidates arg))
    (post-completion
     (company-org-block--expand arg))))

(defun company-org-block--candidates (prefix)
  "Return a list of org babel languages matching PREFIX."
  (seq-filter (lambda (language)
                (string-prefix-p prefix language))
              (mapcar (lambda (mode)
                        (format "%s" (car mode)))
                      org-babel-load-languages)))

(defun company-org-block--expand (insertion)
  "Replace INSERTION with actual source block."
  (delete-region (point) (- (point) (1+ ;; Include "<" in length.
                                     (length insertion))))
  (insert (format "#+begin_src %s\n" insertion))
  (insert "  ")
  ;; Saving restores point to location inside code block.
  (save-excursion
    (insert "\n#+end_src")))

(defun company-org-block--grap-symbol-cons ()
  "Return cons with symbol and t whenever prefix of < is found.
      For example:
          \"<e\" -> (\"e\" . t)
      "
  (when (looking-back "<\\([^ ]*\\)" (line-beginning-position))
    (if (match-string-no-properties 1)
        (cons (match-string-no-properties 1) t)
      nil)))

(provide 'company-org-block)
