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
                                 'local))))
