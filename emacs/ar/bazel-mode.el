;;; bazel-mode.el --- Bazel mode support.

;;; Commentary:
;; Bazel mode helpers.


;;; Code:

(require 'prog-mode)

(define-derived-mode bazel-mode prog-mode "Bazel")

(defun bazel-mode--hook-fun ()
  "Configure mode in hook."
  (setq-local imenu-generic-expression
              '(("Build rule" "name *= *\"\\(.*\\)\"" 1))))

(add-hook 'bazel-mode-hook #'bazel-mode--hook-fun)

(add-to-list 'auto-mode-alist '("BUILD\\'" . bazel-mode))

(provide 'bazel-mode)

;;; bazel-mode.el ends here
