(use-package view
  :commands (global-view-mode
             view-mode)
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
              ("SPC" . ccm-scroll-up))
  :config
  (define-global-minor-mode global-view-mode view-mode
    (lambda ()
      (if (and (not noninteractive)
               (derived-mode-p 'prog-mode
                               'outline-mode
                               'text-mode))
          (view-mode +1)
        (message "Ignoring view-mode for %s" major-mode)
        (view-mode -1)))))
