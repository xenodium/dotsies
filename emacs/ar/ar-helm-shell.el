;;; helm-shell.el --- Helm shell support.

;;; Commentary:
;; Helm shell helpers.

(require 'ar-helm)
(require 'ar-shell)

;;; Code:

(defun ar/helm-shell-search-history ()
  "Narrow down bash history with helm."
  (interactive)
  (assert (string-equal mode-name "Shell") nil "Not in Shell mode")
  (ar/helm-helm "bash history"
                (with-temp-buffer
                  (insert-file-contents "~/.bash_history")
                  (reverse
                   (delete-dups
                    (split-string (buffer-string) "\n"))))
                (lambda (command)
                  (ar/shell-send-command command t))))

(provide 'ar-helm-shell)

;;; ar-helm-shell.el ends here
