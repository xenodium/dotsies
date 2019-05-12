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
    (let ((swift-format-config-dpath (locate-dominating-file (buffer-file-name)
                                                             ".swift-format.json")))
      (if swift-format-config-dpath
          (call-process "swift-format" nil "*swift-format*" t
                        "--configuration" (expand-file-name (concat (file-name-as-directory
                                                                     swift-format-config-dpath)
                                                                    ".swift-format.json"))
                        "-i"
                        "-m" "format"
                        buffer-file-name)
        (message "No .swift-format.json found")))
    ;; (call-process "swiftlint" nil "*swiftlint*" t "autocorrect"
    ;;               "--config" flycheck-swiftlint-config-file
    ;;               "--path" buffer-file-name)
    )
  :config
  (require 'flycheck)
  (ar/vsetq swift-mode:basic-offset 2)
  (require 'ar-vsetq)
  (require 'company-swimports)
  (require 'flycheck-swiftlint)

  (require 'info)
  (info-lookup-maybe-add-help
   :mode 'swift-mode
   :regexp "[#@_a-zA-Z][_a-zA-Z0-9]*"
   :doc-spec '(("(swift)Index" nil "['`‘]" "['’]")))

  (use-package lsp-sourcekit
    :config
    (setenv "SOURCEKIT_TOOLCHAIN_PATH"
            "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain")
    (setq lsp-sourcekit-executable (expand-file-name "~/local/bin/sourcekit-lsp"))))
