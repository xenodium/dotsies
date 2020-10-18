;;; company-org-block.el --- Company backend to complete org blocks.

;;; Commentary:
;; Enable in org-mode to complete source blocks.
;;
;; Completes only when there's a "<" before.

;;; Code:

(require 'map)
(require 'org)
(require 'seq)

(defcustom company-org-block-complete-at-bol t
  "If t, detect completion when at begining of line, otherwise detect
 completion anywhere."
  :type 'boolean)

(defcustom company-org-block-explicit-lang-defaults t
  "If t, insert org-babel-default-header-args:lang into block header."
  :type 'boolean)

(defcustom company-org-block-edit-mode 'auto
  "Customize whether edit mode, post completion was inserted."
  :type '(choice
	  (const :tag "nil: no edit after insertion" nil)
	  (const :tag "prompt: ask before edit" prompt)
	  (const :tag "auto edit, no prompt" auto)))

;; (defvar company-org-block-auto-major-edit t "If t, enter
;; major mode after completion.")

(defvar company-org--regexp "<\\([^ ]*\\)")

(defun company-org-block (command &optional arg &rest ignored)
  "Complete org babel languages into source blocks."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-org-block))
    (prefix (when (derived-mode-p 'org-mode)
              (company-org-block--grab-symbol-cons)))
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
                (map-values org-structure-template-alist)
                (map-values org-babel-tangle-lang-exts)))))

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

    (company-org-block--wrap-point (format "src %s%s"
                                           insertion
                                           (if company-org-block-explicit-lang-defaults
                                               (company-org-block--lang-header-defaults "insertion")
                                             ""))
                                   "src")))

(defun company-org-block--wrap-point (begin end)
  "Wrap point with block using BEGIN and END.  For example:
#+begin_BEGIN
  |
#+end_END"
  (insert (format "#+begin_%s\n" begin))
  (insert (make-string org-edit-src-content-indentation ?\s))
  ;; Saving excursion restores point to location inside code block.
  (save-excursion
    (insert (format "\n#+end_%s" end)))
  (cond ((eq company-org-block-edit-mode 'auto)
         (org-edit-special))
        ((and (eq company-org-block-edit-mode 'prompt)
              (yes-or-no-p "Edit now?"))
         (org-edit-special))))

(defun company-org-block--grab-symbol-cons ()
  "Return cons with symbol and t whenever prefix of < is found.
For example: \"<e\" -> (\"e\" . t)"
  (when (looking-back (if company-org-block-complete-at-bol
                          (concat "^" company-org--regexp)
                        company-org--regexp)
                      (line-beginning-position))
    (cons (match-string-no-properties 1) t)))

(defun company-org-block--lang-header-defaults (lang)
  "Resolve and concatenate all header defaults for LANG.

For example: \"python\" resolves to:

((:exports . \"both\")
 (:results . \"output\"))

and returns:

\" :exports both :results output\""
  (let ((lang-headers-var (intern
			   (concat "org-babel-default-header-args:" lang))))
    (if (boundp lang-headers-var)
        (seq-reduce (lambda (value element)
                      (format "%s %s %s"
                              value
                              (car element)
                              (cdr element)))
                    (eval lang-headers-var t) "")
      "")))

(provide 'company-org-block)
