;; Needs .ctags and .globalrc in $HOME.
(use-package counsel-gtags
  :ensure t
  :bind (:map
         counsel-gtags-mode-map
         ("M-." . counsel-gtags-dwim)
         ("M-," . counsel-gtags-go-backward))
  :hook ((swift-mode . counsel-gtags-mode)
         (swift-mode . ggtags-mode)))
