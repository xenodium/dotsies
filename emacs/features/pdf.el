(use-package pdf-tools
  :ensure t
  ;; :ensure-system-package poppler
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  ;; (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t))
