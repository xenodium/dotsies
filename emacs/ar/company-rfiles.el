;;; company-rfiles.el --- Company recursive file support.

;;; Commentary:
;; Company recursive file helpers.


;;; Code:

(require 'cl-lib)
(require 'company)

(defvar-local company-rfiles-find-format-string "\*%s\*")

(defvar-local company-rfiles-find-flags ". -type f -iname")

(defun company-rfiles-find (value)
  (ignore-errors
    (mapcar (lambda (path)
              (string-remove-prefix "./" path))
            (apply #'process-lines (append (list "find")
                                           (split-string company-rfiles-find-flags " " t)
                                           (list (format company-rfiles-find-format-string value)))))))

(defun company-rfiles (command &optional arg &rest ignored)
  (interactive (list 'interactive))

  (case command
    (interactive (company-begin-backend 'company-rfiles))
    (prefix (company-grab-symbol))
    (candidates
     (company-rfiles-find (substring-no-properties
                           (company-grab-symbol) 0 (length (company-grab-symbol)))))))

(provide 'company-rfiles)

;;; company-rfiles.el ends here
