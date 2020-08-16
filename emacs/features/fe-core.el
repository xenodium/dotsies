;;; -*- lexical-binding: t; -*-

(use-package with-editor
  :ensure t
  :hook ((eshell-mode . with-editor-export-editor)
         (term-exec . with-editor-export-editor)
         (shell-mode . with-editor-export-editor))
  :config
  ;; Requires ~/.hgrc
  ;; [merge-tools]
  ;; emacsclient.args = --eval '(ediff-merge-with-ancestor "$local" "$other" "$base" nil "$output")'
  (setenv "HGMERGE" "emacsclient")
  ;; Growl-workalike for Emacs.
  (use-package alert
    :ensure t
    :commands alert
    :validate-custom
    (alert-default-style (cond ((string-equal system-type "darwin")
				'osx-notifier)
			       ((string-equal system-type "gnu/linux")
				'notifications)
			       (t
				(error "Unrecognized system for alert"))))))
