;;; ar-helm-text.el --- Helm text manipulation support.

;;; Commentary:
;; Helm text manipulation helpers.


;;; Code:

(require 'simple)
(require 'helm-source)

(defun ar/helm-text-insert-file-path (root-dir file-pattern &optional mod-function)
  "Insert a file path relative to ROOT-DIR.  Filter with FILE-PATTERN.
Optionally modify results with MOD-FUNCTION."
  (helm :sources
        (helm-build-sync-source "Quick file path insert"
          :candidates
          (let ((candidates (split-string
                             (shell-command-to-string
                              (format "find %s -iname %s"
                                      root-dir
                                      file-pattern)))))
            (if mod-function
                (mapcar mod-function
                        candidates)
              candidates))
          :action (lambda (path)
                    (insert path)))
        :buffer "*quick file path insert*"
        :truncate-lines t))

(provide 'ar-helm-text)

;;; ar-helm-text.el ends here
