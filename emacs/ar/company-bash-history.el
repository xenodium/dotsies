;;; company-bash-history.el --- Company bash history support.

;;; Commentary:
;; Company bash history helpers.


;;; Code:

(require 'cl-lib)
(require 'company)

(defun company-bash-history-value (value)
  (with-temp-buffer
    (insert-file-contents "~/.bash_history")
    (reverse
     (delete-dups
      (split-string (buffer-string) "\n")))))

(defun company-bash-history (command &optional arg &rest ignored)
  (interactive (list 'interactive))

  (case command
    (interactive (company-begin-backend 'company-bash-history))
    (prefix
     (company-grab-symbol-cons (regexp-quote "$ ") 2))
    (candidates
     (company-bash-candidates (company-grab-symbol-cons (regexp-quote "$ ") 2)))))

(defun company-bash-candidates (value)
  (if (consp value)
      (let ((search-term (car value))
            (trigger-found (cdr value)))
        (when trigger-found
          (company-bash-history-value (substring-no-properties search-term 0 (length search-term)))))))

(provide 'company-bash-history)

;;; company-bash-history.el ends here
