;;; -*- lexical-binding: t; -*-

(use-package hydra
  :ensure t
  :hook (vc-git-log-edit-mode . hydra-vc-log-edit/body)
  :bind (("M-s" . hydra-search/body)
         ("C-c s" . hydra-search/body)
         ("C-c x" . hydra-quick-insert/body)
         ("C-c o" . ar/hydra-open-dwim)
         ("C-c g" . hydra-git-gutter/body)
         ("C-c 1" . hydra-profile/body))
  :validate-custom
  (hydra-is-helpful t)
  :config
  (load "~/.emacs.d/features/config-hydra"))
