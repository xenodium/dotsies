;;; company-org-block.el --- Company backend to complete org blocks.

;;; Commentary:
;; Enable in org-mode to complete source blocks.
;;
;; Completes only when there's a "<" before.

;;; Code:

(require 'map)
(require 'org)
(require 'seq)

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
              ;; Flatten `org-babel-load-languages' and
              ;; `org-structure-template-alist', join, and sort.
              (seq-sort
               #'string-lessp
               (append
                (mapcar #'prin1-to-string
                        (map-keys org-babel-load-languages))
                (map-values org-structure-template-alist)))))

(defun company-org-block--template-p (template)
  (seq-contains (map-values org-structure-template-alist)
                template))

(defun company-org-block--expand (insertion)
  "Replace INSERTION with actual source block."
  (delete-region (point) (- (point) (1+ ;; Include "<" in length.
                                     (length insertion))))
  (if (company-org-block--template-p insertion)
      (company-org-block--wrap-point insertion
                                     ;; May be multiple words.
                                     ;; Take the first one.
                                     (nth 0 (split-string insertion)))
    (company-org-block--wrap-point (format "src %s" insertion)
                                   "src")))

(defun company-org-block--wrap-point (begin end)
  "Wrap point with block using BEGIN and END.  For example:
#+begin_BEGIN
  |
#+end_END"
  (insert (format "#+begin_%s\n" begin))
  (insert "  ")
  ;; Saving excursion restores point to location inside code block.
  (save-excursion
    (insert (format "\n#+end_%s" end))))

(defun company-org-block--grap-symbol-cons ()
  "Return cons with symbol and t whenever prefix of < is found.
For example: \"<e\" -> (\"e\" . t)"
  (when (looking-back "<\\([^ ]*\\)" (line-beginning-position))
    (if (match-string-no-properties 1)
        (cons (match-string-no-properties 1) t)
      nil)))

(provide 'company-org-block)
