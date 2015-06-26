;;; timestamp.el --- Adds a timestamp at the beginning of each line.

;;; Commentary:
;; Enable with timestamp-mode


;;; Code:

(defvar timestamp-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [foo] 'timestamp-new-line)
    map)
  "Keymap for `timestamp-mode'.")

(defun timestamp-new-line ()
  "Insert a new line and time stamp."
  (interactive)
  (insert (format-time-string "\n%H:%M:%S > "
                              (current-time))))

(define-key timestamp-mode-map (kbd "RET") 'timestamp-new-line)

(define-derived-mode timestamp-mode text-mode "Timestamp mode")

(provide 'timestamp)

;;; timestamp.el ends here
