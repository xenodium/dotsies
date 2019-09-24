(use-package swift-mode :ensure t
  :mode ("\\.swift\\'" . swift-mode)
  :hook (swift-mode . ar/swift-mode-hook)
  :init
  (defun ar/swift-mode-hook ()
    "Called when entering `swift-mode'."
    (set-fill-column 100)
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

    (setq-local company-backends '((company-swimports)))

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

  ;; Unset swift-mode:send-region. I prefer my default (ivy-resume).
  (bind-key "C-c C-r" nil swift-mode-map)

  (require 'info)
  (info-lookup-maybe-add-help
   :mode 'swift-mode
   :regexp "[#@_a-zA-Z][_a-zA-Z0-9]*"
   :doc-spec '(("(swift)Index" nil "['`‘]" "['’]")))

  ;; (use-package lsp-sourcekit
  ;;   :config
  ;;   (setenv "SOURCEKIT_TOOLCHAIN_PATH"
  ;;           "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain")
  ;;   (setq lsp-sourcekit-executable (expand-file-name "~/local/bin/sourcekit-lsp")))
  (defun ar/swift-public-interface ()
    "Open an occur buffer with file's public interface."
    (interactive)
    (assert (eq major-mode 'swift-mode) nil "Not in swift-mode")
    (let ((list-matching-lines-face nil))
      (occur "\\(public\\)\\|\\(open\\)"))))

(use-package applescript-mode
  :ensure t
  :mode ("\\.applescript\\'" . swift-mode))

(use-package swift-playground-mode
  :disabled ;; Need to think about how to integrate better into workflow.
  :ensure t
  :hook (swift-mode . swift-playground-global-mode))

;; Curated Swift 5.1 documentation with cross-references, and keyword/topic indices.
(use-package swift-helpful
  :ensure t
  :commands swift-helpful)
