;;; ar-shell.el --- Shell support.

;;; Commentary:
;; Shell helpers.

(require 'cl)
(require 'comint)
(require 'shell)

;;; Code:

(defun ar/shell--assert-shell-mode ()
  "Assert current major mode is `shell-mode'."
  (assert (string-equal major-mode 'shell-mode) nil "Not in Shell mode"))

(defun ar/shell-cd (dir-path)
  "Change shell current working directory to DIR-PATH.

Like `shell-pop--cd-to-cwd-shell', but without recentering."
  (ar/shell--assert-shell-mode)
  (ar/shell-send-command (concat "cd " (shell-quote-argument dir-path))))

(defun ar/shell-send-command (command &optional no-newline)
  "Send COMMAND to shell mode.
Optional argument NO-NEWLINE to avoid inserting a newline (ie. execute/return)."
  (ar/shell--assert-shell-mode)
  (goto-char (point-max))
  (comint-kill-input)
  (insert command)
  (comint-send-input no-newline))

(provide 'ar-shell)

;;; ar-shell.el ends here






