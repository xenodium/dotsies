;;; -*- lexical-binding: t; -*-

(use-package swift-mode
  :ensure t
  :mode ("\\.swift\\'" . swift-mode)
  :hook (swift-mode . ar/swift-mode-hook)
  :validate-custom
  (swift-mode:basic-offset 2)
  :config
  (load "~/.emacs.d/features/config-swift"))

;; Not yet on melpa.
;; (use-package flycheck-bazel
;;   :commands (flycheck-bazel-setup)
;;   :hook ((bazel-mode . flycheck-bazel-setup)))

(use-package applescript-mode
  :ensure t
  :mode ("\\.applescript\\'" . applescript-mode))
