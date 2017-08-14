;;; company-grep.el --- Company grep support.

;;; Commentary:
;; Company grep helpers.


;;; Code:

(require 'projectile)
(require 'cl-lib)
(require 'company)

(defvar-local company-grep-grep-bin "rg")

(defvar-local company-grep-grep-format-string "%s")

(defvar-local company-grep-grep-flags "--nofilename --regexp")

(defvar-local company-grep-grep-trigger nil)

(defvar-local company-grep-grep-cleanup-fun #'identity)

(defvar-local company-grep-grep-completion-fun (lambda ()))

(defun company-grep-value (value)
  (ignore-errors
    (funcall company-grep-grep-cleanup-fun
             (apply #'process-lines (append (list company-grep-grep-bin)
                                            (split-string company-grep-grep-flags " " t)
                                            (list (format company-grep-grep-format-string (regexp-quote value)) (projectile-project-root)))))))

(defun company-grep (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-grep))
    (prefix
     (if company-grep-grep-trigger
         (company-grab-symbol-cons company-grep-grep-trigger)
       (company-grab-symbol)))
    (candidates
     (company-grep-candidates (company-grab-symbol-cons company-grep-grep-trigger)))
    (post-completion (funcall company-grep-grep-completion-fun))))

(defun company-grep-candidates (value)
  (if (consp value)
      (let ((search-term (car value))
            (trigger-found (cdr value)))
        (when trigger-found
          (company-grep-value (substring-no-properties search-term 0 (length search-term)))))))

(provide 'company-grep)

;;; company-grep.el ends here
