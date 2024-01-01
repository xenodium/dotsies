;;; -*- lexical-binding: t; -*-

(use-package swift-mode
  :ensure t
  :mode ("\\.swift\\'" . swift-mode)
  :hook (swift-mode . ar/swift-mode-hook)
  :validate-custom
  (swift-mode:basic-offset 2)
  :config
  (defun ar/swift-mode-hook ()
    "Called when entering `swift-mode'."
    (set-fill-column 100)
    (if (buffer-file-name)
        (when-let ((lint-config-file (locate-dominating-file (buffer-file-name)
                                                             ".swiftlint.yml")))
          (setq-local flycheck-swiftlint-config-file
                      (concat (file-name-as-directory
                               lint-config-file)
                              ".swiftlint.yml")))
      (message "No buffer filename in swift mode."))
    (setq-local company-backends '(company-bazel-swift-imports company-capf company-yasnippet)))

  (require 'flycheck)
  (add-to-list 'flycheck-checkers 'swiftlint)

  (require 'company-bazel-swift-imports)
  (require 'flycheck-swiftlint)

  ;; Unset swift-mode:send-region. I prefer my default (ivy-resume).
  (require 'bind-key)
  (bind-key "C-c C-r" nil swift-mode-map)

  (require 'info-look)
  (info-lookup-maybe-add-help
   :mode 'swift-mode
   :regexp "[#@_a-zA-Z][_a-zA-Z0-9]*"
   :doc-spec '(("(swift)Index" nil "['`‘]" "['’]")))

  (require 'reformatter)
  (reformatter-define swift-format
    :program "swift-format"
    :args '("format"))
  (add-hook 'swift-mode-hook 'swift-format-on-save-mode)

  (use-package eglot
    :ensure t
    :hook (swift-mode . eglot-ensure)
    :config
    (message "warning: `jsonrpc--log-event' is ignored.")
    (fset #'jsonrpc--log-event #'ignore)
    (add-to-list 'eglot-server-programs '(swift-mode . ("/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))))

  ;; (use-package lsp-mode
  ;;   :ensure t
  ;;   :hook (swift-mode . lsp-deferred)
  ;;   :commands (lsp lsp-deferred)
  ;;   :validate-custom
  ;;   (lsp-restart 'auto-restart)
  ;;   :bind
  ;;   (:map
  ;;    lsp-signature-mode-map
  ;;    ;; Unset, as I prefer overlay-jump.
  ;;    ([remap lsp-signature-next] . nil)
  ;;    ([remap lsp-signature-previous] . nil)))

  ;; (use-package lsp-sourcekit
  ;;   :ensure t
  ;;   :after lsp-mode
  ;;   :config
  ;;   ;; (setq lsp-sourcekit-extra-args (list "--log-level" "info"))
  ;;   ;; (setq lsp-sourcekit-executable "/Users/alvaro/stuff/active/code/third_party/sourcekit-lsp/.build/x86_64-apple-macosx/debug/sourcekit-lsp")
  ;;   (setq lsp-sourcekit-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))

  (defun ar/xcode-signing-info ()
    "Copy an Xcode signing identity."
    (interactive)
    (let ((identities (seq-filter (lambda (line)
                                    (string-match "\"$" line))
                                  (process-lines "security" "find-identity" "-v" "-p" "codesigning")))
          (choice))
      (when (seq-empty-p identities)
        (error "No identifies found"))
      (setq choice (completing-read "Identity: " identities))
      (kill-new choice)
      (message "Copied: %s" choice)))

  (defun ar/xcode-info ()
    (interactive)
    (let ((buffer "*Xcode info*"))
      (shell-command "system_profiler SPDeveloperToolsDataType" buffer)
      ;; Select window and make read-only (q closes window).
      (with-current-buffer buffer
        (view-mode +1)
        (select-window (get-buffer-window buffer)))))

  (defun ar/swift-package-init ()
    "Execute `swift package init', with optional name and completing type."
    (interactive)
    (let* ((name (read-string "name (default): "))
           (type (completing-read
                  "project type: "
                  ;; Splits "--type empty|library|executable|system-module|manifest"
                  (split-string
                   (nth 1 (split-string
                           (string-trim
                            (seq-find
                             (lambda (line)
                               (string-match "--type" line))
                             (process-lines "swift" "package" "init" "--help")))
                           "   "))
                   "|")))
           (command (format "swift package init --type %s" type)))
      (unless (string-empty-p name)
        (append command "--name " name))
      (shell-command command))
    (dired default-directory)
    (revert-buffer))

  (defun ar/swift-public-interface ()
    "Open an occur buffer with file's public interface."
    (interactive)
    (assert (eq major-mode 'swift-mode) nil "Not in swift-mode")
    (let ((list-matching-lines-face nil))
      (occur "\\(public\\)\\|\\(open\\)")))

  ;; Curated Swift 5.1 documentation with cross-references, and keyword/topic indices.
  (use-package swift-helpful
    :ensure t
    :commands swift-helpful))

;; Not yet on melpa.
;; (use-package flycheck-bazel
;;   :commands (flycheck-bazel-setup)
;;   :hook ((bazel-mode . flycheck-bazel-setup)))
