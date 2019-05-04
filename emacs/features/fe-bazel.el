(use-package bazel-mode
  :commands ar/bazel-find-staged
  :mode (("\\.bzl\\'" . bazel-mode)
         ("BUILD\\'" . bazel-mode))
  :hook (bazel-mode . ar/bazel-mode-hook-fun)
  :init
  (defun ar/bazel-mode-hook-fun ()
    (ar/buffer-run-for-saved-file-name "buildifier" "BUILD")
    (setq-local company-backends '(company-bazel company-rfiles)))
  :config
  (use-package ar-bazel)
  (use-package company-bazel)
  (use-package ar-counsel-find)

  (defun ar/bazel-find-staged ()
    "Call the \"find\" shell command and fuzzy narrow using ivy."
    (interactive)
    (ar/counsel-find--in-paths (list (ar/bazel-bin-dir)
                                     (ar/bazel-genfiles-dir)
                                     (ar/bazel-out-dir)))))
