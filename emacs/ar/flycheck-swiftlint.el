;;; flycheck-swiftlint.el --- Flycheck: Swift support for Apple swift-mode
;;
;; Usage:
;;
;; (with-eval-after-load 'flycheck
;;   (add-hook 'flycheck-mode-hook #'flycheck-swiftlint-setup))

;;; Code:

(require 'flycheck)

(defun flycheck-swiftlint-command ()
  (list "swiftlint" "--path" (buffer-file-name)))

(flycheck-def-config-file-var
    flycheck-swiftlint-config-file
    flycheck-swiftlint ".swiftlint.yml"
  :safe #'stringp)

(flycheck-define-checker swiftlint
  "A Swift syntax checker using Swiftlint"
  :command ("swiftlint"
            (config-file "--config" flycheck-swiftlint-config-file)
            "--path" source)
  :error-patterns ((error line-start "<unknown>:" line
                          ": " "error: " (optional (message)) line-end)
                   (info line-start (or "<stdin>" (file-name)) ":" line ":" column
                         ": " "note: " (optional (message)) line-end)
                   (warning line-start (or "<stdin>" (file-name)) ":" line ":" column
                            ": " "warning: " (optional (message)) line-end)
                   (error line-start (or "<stdin>" (file-name)) ":" line ":" column
                          ": " "error: " (optional (message)) line-end))
  :modes swift-mode)

(provide 'flycheck-swiftlint)

;;; flycheck-swiftlint.el ends here
