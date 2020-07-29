;;; -*- lexical-binding: t; -*-
(use-package swift-mode :ensure t
  :mode ("\\.swift\\'" . swift-mode)
  :after reformatter
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
    (setq-local company-backends '(company-swimports company-capf)))
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
  ;; (call-process "swiftlint" nil "*swiftlint*" t "autocorrect"
  ;;               "--config" flycheck-swiftlint-config-file
  ;;               "--path" buffer-file-name)
  (when (require 'reformatter nil 'noerror)
    (reformatter-define swift-format
      :program "swift-format"
      :args (let ((buffer (current-buffer))
                  (config-file (locate-dominating-file (buffer-file-name)
                                                       ".swift-format.json"))
                  (temp-file-path (make-temp-file "swift-format-")))
              (with-temp-file temp-file-path
                (insert-buffer buffer))
              (if config-file
                  (list "--configuration" config-file "-m" "format" temp-file-path))
              (list "-m" "format" temp-file-path)))
    (add-hook 'swift-mode-hook 'swift-format-on-save-mode))

  (defun ar/xcode-info ()
    (interactive)
    (shell-command "system_profiler SPDeveloperToolsDataType"))

  ;; (use-package lsp-mode
  ;;   :hook (swift-mode . lsp-deferred)
  ;;   :commands (lsp lsp-deferred))

  ;; (use-package lsp-sourcekit
  ;;   :after lsp-mode
  ;;   :config
  ;;   ;; (setq lsp-sourcekit-extra-args (list "--log-level" "info"))
  ;;   (setq lsp-sourcekit-executable "/Applications/Xcode-beta.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))

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

;; Not yet on melpa.
;; (use-package flycheck-bazel
;;   :commands (flycheck-bazel-setup)
;;   :hook ((bazel-mode . flycheck-bazel-setup)))

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

;; (defun ar/swift-args-to-docstring (text)
;;   "Create a docstring from a Swift function parameter TEXT (comma-separated)."
;;   (let* ((args (ar/swift-split-args text)))
;;     (when (> (length args) 0)
;;       (mapconcat
;;            (lambda (x)
;;              (format "///   - %s: " (nth 0 x)))
;;            args
;;            "\n"))))

;; (defun ar/swift-split-args (arg-string)
;;   "Split a js argument string into ((name, default)..) tuples.  ARG-STRING."
;;   (mapcar (lambda (x)
;;              (split-string x "[[:blank:]]*=[[:blank:]]*" t))
;;            (split-string arg-string "[[:blank:]]*,[[:blank:]]*" t)))
