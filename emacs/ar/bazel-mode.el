;;; bazel-mode.el --- Bazel mode support.

;;; Commentary:
;; Bazel mode helpers.


;;; Code:

(require 'prog-mode)

;; Based on `python-mode-syntax-table'
(defvar bazel-mode-syntax-table
  (let ((table (make-syntax-table)))
    (let ((symbol (string-to-syntax "_"))
          (sst (standard-syntax-table)))
      (dotimes (i 128)
        (unless (= i ?_)
          (if (equal symbol (aref sst i))
              (modify-syntax-entry i "." table)))))
    (modify-syntax-entry ?$ "." table)
    (modify-syntax-entry ?% "." table)
    ;; exceptions
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?` "$" table)
    table)
  "Syntax table for BUILD files.")

(define-derived-mode bazel-mode prog-mode "Bazel"
  :syntax-table bazel-mode-syntax-table
  (setq-local comment-start "# "))

(defun bazel-mode--hook-fun ()
  "Configure mode in hook."
  (setq-local imenu-generic-expression
              '(("Build rule" "name *= *\"\\(.*\\)\"" 1))))

(add-hook 'bazel-mode-hook #'bazel-mode--hook-fun)

(add-to-list 'auto-mode-alist '("BUILD\\'" . bazel-mode))
(add-to-list 'auto-mode-alist '("\\.bzl\\'" . bazel-mode))

(provide 'bazel-mode)

;;; bazel-mode.el ends here
