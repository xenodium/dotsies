;;; ar-info.el --- Info support.

;;; Commentary:
;; Info helpers.


;;; Code:

(defun ar/format-info-mode ()
  "Opening .info files does not automatically set things up. Give it a little help."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (kill-buffer (current-buffer))
    (info file-name)))

(provide 'ar-info)

;;; ar-info.el ends here
