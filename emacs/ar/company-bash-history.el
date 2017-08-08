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
    (prefix (company-grab-symbol))
    (candidates
     (company-bash-history-value (gnus-string-remove-all-properties (company-grab-symbol))))))

(provide 'company-bash-history)

;;; company-bash-history.el ends here
