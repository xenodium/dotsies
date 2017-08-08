;;; bazel-mode.el --- Bazel mode support.

;;; Commentary:
;; Bazel mode helpers.


;;; Code:

(require 'python)

(define-derived-mode bazel-mode python-mode "Bazel")

(defun ar/bazel-mode--imenu-index-fun ()
  "Implemented, since `python-mode' set `imenu-create-index-function'."
  (let ((mode-imenu (imenu-default-create-index-function))
        (custom-imenu (imenu--generic-function imenu-generic-expression)))
    (append mode-imenu custom-imenu)))

(defun ar/bazel-mode-hook-fun ()
  "Configure mode in hook."
  (setq-local imenu-create-index-function #'ar/bazel-mode--imenu-index-fun)
  (setq-local imenu-generic-expression
              '(("Build rule" "name *= *\"\\(.*\\)\"" 1))))

(add-hook 'bazel-mode-hook #'ar/bazel-mode-hook-fun)

(add-to-list 'auto-mode-alist '("BUILD\\'" . bazel-mode))

(provide 'bazel-mode)

;;; bazel-mode.el ends here
