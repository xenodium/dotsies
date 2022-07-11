;;; company-escaped-files.el --- Company grep support.

;;; Commentary:
;; Company grep helpers.


;;; Code:

(require 's)
(require 'company-files)

(defun company-escaped-files (command &optional arg &rest ignored)
  "`company-escaped-files' is like `company-files' but escapes paths for shell."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-escaped-files))
    (prefix (company-files--grab-existing-name))
    (candidates (company-files--complete arg))
    (location (cons (dired-noselect
                     (file-name-directory (directory-file-name arg))) 1))
    (post-completion (progn
                       (delete-region (point) (- (point) (length arg)))
                       ;; Remove trailing slash.
                       (let ((path (if (s-ends-with? "/" arg)
                                       (s-left -1 arg)
                                     arg)))
                         ;; Quote if spaces found.
                         (insert (if (s-contains-p " " path)
                                     (format "\"%s\"" path)
                                   path)))))
    (sorted t)
    (no-cache t)))

(provide 'company-escaped-files)

;;; company-escaped-files.el ends here
