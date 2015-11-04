;;; ar-shell.el --- Shell support.

;;; Commentary:
;; Shell helpers.

(require 'comint)

(defun ar/shell-cd (dir-path)
  "Change shell current working directory to DIR-PATH.

Like shell-pop--cd-to-cwd-shell, but without recentering."
  (unless (string-equal mode-name "Shell")
    (error "Not in Shell mode"))
  (message mode-name)
  (goto-char (point-max))
  (comint-kill-input)
  (insert (concat "cd " (shell-quote-argument dir-path)))
  (let ((comint-process-echoes t))
    (comint-send-input)))

(defun ar/shell-send-command (command)
  "Send COMMAND to shell mode."
  (assert (string-equal mode-name "Shell") nil "Not in Shell mode")
  (message mode-name)
  (goto-char (point-max))
  (comint-kill-input)
  (insert command)
  (let ((comint-process-echoes t))
    (comint-send-input)))

(provide 'ar-shell)

;;; ar-shell.el ends here
