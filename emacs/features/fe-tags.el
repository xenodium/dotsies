;; Needs .ctags and .globalrc in $HOME.
(use-package counsel-gtags
  :ensure t
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
  :commands ggtags-mode)
