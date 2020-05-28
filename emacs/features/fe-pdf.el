;;; -*- lexical-binding: t; -*-
(use-package pdf-tools
  :ensure t
  ;; :ensure-system-package poppler
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
              ;; Swiper doesn't work in pdf-tools.
              ("C-s" . 'isearch-forward))
  :config
  ;; (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t))
