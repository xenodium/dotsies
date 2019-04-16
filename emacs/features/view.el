(use-package view
  :hook ((prog-mode . ar/view-mode)
         (org-mode . ar/view-mode)
         (view-mode . goto-address-mode))
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
  (defun ar/view-mode ()
    (unless noninteractive
      (view-mode +1))))
