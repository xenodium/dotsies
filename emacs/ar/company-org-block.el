;;; company-org-block.el --- Org blocks company backend -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Alvaro Ramirez

;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/xenodium/company-org-block

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; `company-complete' org blocks using "<" as trigger
;;
;; To enable, add `company-org-block' to `company-backend'.

;;; Code:

(require 'company)
(require 'map)
(require 'org)
(require 'seq)

(defgroup company-org-block nil
  "Completion back-end for org blocks."
  :group 'company)

(defcustom company-org-block-complete-at-bol t
  "If t, detect completion only at the beginning of lines."
  :type 'boolean)

(defcustom company-org-block-explicit-lang-defaults t
  "If t, insert org-babel-default-header-args:lang into block header."
  :type 'boolean)

(defcustom company-org-block-edit-mode 'auto
  "Customize how to enter edit mode after block is inserted."
  :type '(choice
	  (const :tag "nil: no edit after insertion" nil)
	  (const :tag "prompt: ask before edit" prompt)
	  (const :tag "auto edit, no prompt" auto)))

(defvar company-org-block--regexp "<\\([^ ]*\\)")

(defun company-org-block (command &optional arg &rest _ignored)
  "A company completion backend for org blocks.

COMMAND and ARG are sent by company itself."
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
  "Check if there is a TEMPLATE available for completion."
  (seq-contains-p (map-values org-structure-template-alist)
                  template))

(defun company-org-block--expand (insertion)
  "Replace INSERTION with generated source block."
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
                          (concat "^" company-org-block--regexp)
                        company-org-block--regexp)
                      (line-beginning-position))
    (cons (match-string-no-properties 1) t)))

(defun company-org-block--lang-header-defaults (lang)
  "Resolve and concatenate all header defaults for LANG.

For example: \"python\" resolves to:

\((:exports . \"both\")
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

;;; company-org-block.el ends here
