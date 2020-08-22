;;; -*- lexical-binding: t; -*-
;; Needs .ctags and .globalrc in $HOME.

(use-package counsel-gtags
  :ensure t
  :disabled ;; trying out counsel-etags
  :commands counsel-gtags-mode
  :bind (:map
         counsel-gtags-mode-map
         ("M-." . counsel-gtags-dwim)
         ("M-," . counsel-gtags-go-backward))
  :hook ((swift-mode . counsel-gtags-mode)
         (swift-mode . ggtags-mode)))

;; Needs .ctags and .globalrc in $HOME.
(use-package ggtags
  :ensure t
  :disabled ;; trying out counsel-etags
  :commands ggtags-mode)

(use-package etags
  :defer
  :validate-custom
  (tags-revert-without-query t)
  (tags-add-tables nil))

;; Note: Also used by smart-jump.
(use-package counsel-etags
  :ensure t
  :commands counsel-etags-find-tag-at-point
  :validate-custom
  (counsel-etags-update-interval 60)
  :hook (prog-mode . (lambda ()
                       (add-hook 'after-save-hook
                                 'counsel-etags-virtual-update-tags
                                 'append
                                 'local)))
  :config
  ;; Can also be added to ~/.ctags as --exclude="bazel-*"
  (add-to-list 'counsel-etags-ignore-filenames "bazel-*")
  (add-to-list 'counsel-etags-ignore-filenames "*/*.xcodeproj/*")

  (defun counsel-etag-grep-action ()
    (interactive)
    (ivy-exit-with-action
     (lambda (_)
       (funcall counsel-etags-fallback-grep-function
                (if (string-empty-p ivy-text)
                    counsel-etags-last-tagname-at-point
                  (format "%s %s" counsel-etags-last-tagname-at-point ivy-text))))))

  (bind-key "C-s" #'counsel-etag-grep-action counsel-etags-find-tag-map)

  (defun ar/counsel-etags-async-ripgrep (&optional default-keyword prompt root)
    "A non-blocking alternative to `counsel-etags-grep'."
    (interactive)
    (unless (counsel-etags-has-quick-grep)
      (error "rg command-line utility not found"))

    (let ((keyword (if default-keyword default-keyword ""))
          (default-directory (file-truename (or root
                                                (counsel-etags-locate-project))))
          (options (concat
                    (mapconcat (lambda (e)
                                 (format "-g=!%s/*" e))
                               counsel-etags-ignore-directories " ")
                    " "
                    (mapconcat (lambda (e)
                                 (format "-g=!%s" e))
                               counsel-etags-ignore-filenames " "))))

      ;; Momentarily override `counsel-git-grep-action'. Use `counsel-etags-open-file-api' instead.
      (cl-letf (((symbol-function 'counsel-git-grep-action) `(lambda (item)
                                                               ;; when grepping, we grepping in project root
                                                               (counsel-etags-open-file-api item
                                                                                            ,default-directory
                                                                                            ,keyword))))
        (counsel-rg keyword default-directory options prompt))))

  (setq counsel-etags-fallback-grep-function #'ar/counsel-etags-async-ripgrep))
