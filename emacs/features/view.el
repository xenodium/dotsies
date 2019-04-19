(use-package view
  :hook ((view-mode . goto-address-mode))
  :bind (:map view-mode-map
              ("C-x C-q" . view-mode)
              ("a" . mwim-beginning-of-code-or-line)
              ("e" . mwim-end-of-code-or-line)
              ("p" . previous-line)
              ("n" . next-line)
              ("f" . forward-char)
              ("b" . backward-char)
              ("v" . ccm-scroll-up)
              ("SPC" . ccm-scroll-up)))

(define-global-minor-mode ar/global-view-mode view-mode
  (lambda ()
    (if (and ar/global-view-mode
             (not noninteractive)
             (derived-mode-p 'prog-mode
                             'org-mode))
        (view-mode +1)
      (view-mode -1))))
