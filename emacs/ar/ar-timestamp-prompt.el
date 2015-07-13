;;; ar-timestamp-prompt.el --- Adds a timestamp at the beginning of each line.

;;; Commentary:
;; Enable with timestamp-prompt-mode.


;;; Code:

(defun ar/timestamp-prompt-new-line ()
  "Insert a new line and time stamp."
  (interactive)
  (insert (format "\n%s" (ar/timestamp-prompt-str))))

(defun ar/timestamp-prompt-str ()
  (format-time-string "%H:%M:%S > " (current-time)))

(defvar ar/timestamp-prompt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'ar/timestamp-prompt-new-line)
    map)
  "Keymap for `ar/timestamp-prompt-mode'.")

(define-derived-mode ar/timestamp-prompt-mode text-mode "Timestamp mode")

(add-hook 'ar/timestamp-prompt-mode-hook
          (lambda ()
            (when (eq (point-min) (point-max))
              (insert (ar/timestamp-prompt-str)))))

(provide 'ar-timestamp-prompt)

;;; ar-timestamp-prompt.el ends here
