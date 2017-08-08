;;; company-grep.el --- Company grep support.

;;; Commentary:
;; Company grep helpers.


;;; Code:

(require 'projectile)
(require 'cl-lib)
(require 'company)

(defvar company-grep-grep-bin "rg")

(defvar company-grep-grep-format-string "%s")

(defvar company-grep-grep-flags "--nofilename --regexp")

(defun company-grep-grep-command ()
  "Combine `company-grep-grep-bin' and `company-grep-grep-flags' to create a command."
  (if (> 0 (length company-grep-grep-flags))
      (format "%s %s" company-grep-grep-bin company-grep-grep-flags)
    company-grep-grep-bin))

(defun company-grep-value (value)
  (apply #'process-lines (append (list company-grep-grep-bin)
                                 (split-string company-grep-grep-flags " " t)
                                 (list (format company-grep-grep-format-string value) (projectile-project-root)))))

(defun company-grep (command &optional arg &rest ignored)
  (interactive (list 'interactive))

  (case command
    (interactive (company-begin-backend 'company-grep))
    (prefix (company-grab-symbol))
    (candidates
     (company-grep-value (gnus-string-remove-all-properties (company-grab-symbol))))))

(provide 'company-grep)

;;; company-grep.el ends here
