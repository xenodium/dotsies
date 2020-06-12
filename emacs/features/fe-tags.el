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
  :validate-custom
  (tags-revert-without-query t))

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
  (defun counsel-etags-fallback-rg-search (&optional default-keyword prompt root)
    "A non-blocking alternative to counsel-etags-grep."
    (unless (counsel-etags-has-quick-grep)
      (error "ripgrep command-line utility not found"))
    (let ((text (if default-keyword default-keyword ""))
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

      (counsel-rg text default-directory options prompt)))

  (defvar ar/counsel-etags-last-tag-searched "")

  (defun adviced:counsel-etags-open-tag-cand (orig-fun &rest r)
    "Additional support for multiple cursors."
    (let ((tagname (nth 0 r)))
      (setq ar/counsel-etags-last-tag-searched tagname)
      (setq )
      (apply orig-fun r)))

  (advice-add #'counsel-etags-open-tag-cand
              :around
              #'adviced:counsel-etags-open-tag-cand)

  (defun adviced:counsel-etags-grep (orig-fun &rest r)
    "Additional support for multiple cursors."
    (let ((default-keyword (nth 0 r))
          (hint (nth 1 r))
          (root) (nth 2))
      (counsel-etags-fallback-rg-search default-keyword hint root)))

  (advice-add #'counsel-etags-grep
              :around
              #'adviced:counsel-etags-grep)

  (defun ar/counsel-etags-search-action (item)
    (counsel-etags-fallback-rg-search ar/counsel-etags-last-tag-searched
                                      "Search: "
                                      (or (counsel-etags-tags-file-directory)
                                          (error "No TAGS found"))))

  (ivy-add-actions
   'counsel-etags-find-tag '(("s" ar/counsel-etags-search-action "fallback search"))))


;; --langdef=swift
;; --langmap=swift:+.swift

;; --kinddef-swift=v,variable,variables
;; --kinddef-swift=f,function,functions
;; --kinddef-swift=s,struct,structs
;; --kinddef-swift=c,class,classes
;; --kinddef-swift=p,protocol,protocols
;; --kinddef-swift=e,enum,enums
;; --kinddef-swift=t,typealias,typealiases

;; --regex-swift=/(var|let)[ \t]+([^:=]+).*$/\2/v/
;; --regex-swift=/func[ \t]+([^\(\)]+)\([^\(\)]*\)/\1/f/
;; --regex-swift=/struct[ \t]+([^:\{]+).*$/\1/s/
;; --regex-swift=/class[ \t]+([^:\{]+).*$/\1/c/
;; --regex-swift=/protocol[ \t]+([^:\{]+).*$/\1/p/
;; --regex-swift=/enum[ \t]+([^:\{]+).*$/\1/e/
;; --regex-swift=/(typealias)[ \t]+([^:=]+).*$/\2/v/
