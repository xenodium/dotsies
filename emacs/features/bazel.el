(use-package bazel-mode
  :mode ("BUILD\\'" . bazel-mode)
  :hook (bazel-mode . ar/bazel-mode-hook-fun)
  :init
  (defun ar/bazel-mode-hook-fun ()
    (ar/buffer-run-for-saved-file-name "buildifier" "BUILD")
    (setq-local company-backends '(company-bazel company-rfiles))
    (company-mode +1))
  :config
  (use-package ar-bazel)
  (use-package company-bazel))
