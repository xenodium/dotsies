(use-package swift-mode :ensure t
  :mode ("\\.swift\\'" . swift-mode)
  :hook (swift-mode . ar/swift-mode-hook)
  :init
  (defun ar/swift-mode-hook ()
    "Called when entering `swift-mode'."
    (add-to-list 'flycheck-checkers 'swiftlint)

    (if (buffer-file-name)
        (let ((lint-config-file (locate-dominating-file (buffer-file-name)
                                                        ".swiftlint.yml")))
          (when lint-config-file
            (setq-local flycheck-swiftlint-config-file
                        (concat (file-name-as-directory
                                 lint-config-file)
                                ".swiftlint.yml"))))
      (message "No buffer filename in swift mode."))

    (setq-local company-backends '((company-sourcekit
                                    company-swimports
                                    company-yasnippet
                                    company-dabbrev-code
                                    company-keywords
                                    company-capf)))

    (add-hook 'after-save-hook 'ar/after-swift-save nil t))

  (defun ar/after-swift-save ()
    (call-process "swiftformat" nil "*swiftformat*" t "--indent" "2" buffer-file-name)
    (call-process "swiftlint" nil "*swiftlint*" t "autocorrect"
                  "--config" flycheck-swiftlint-config-file
                  "--path" buffer-file-name))
  :config
  (require 'flycheck)
  (ar/vsetq swift-mode:basic-offset 2)
  (require 'ar-vsetq)
  (require 'company-swimports)
  (require 'flycheck-swiftlint))
