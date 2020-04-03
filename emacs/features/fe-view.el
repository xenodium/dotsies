;;; -*- lexical-binding: t; -*-
(use-package view
  :init
  (setq-default cursor-type 'box)
  :commands (global-view-mode
             view-mode)
  :hook ((view-mode . goto-address-mode)
         (view-mode . ar/view-mode-set-cursor-type))
  :bind (:map view-mode-map
              ("C-x C-q" . view-mode)
              ("a" . mwim-beginning-of-code-or-line)
              ("e" . mwim-end-of-code-or-line)
              ("p" . previous-line)
              ("n" . next-line)
              ("f" . forward-char)
              ("b" . backward-char)
              ("v" . ccm-scroll-up)
              ("q" . ar/view-mode-quit-window)
              ("SPC" . ccm-scroll-up))
  :config
  (defun ar/view-mode-quit-window (&optional kill window)
    "Like `quit-window' but also kills buffer"
    (interactive "P")
    (quit-restore-window window 'kill))

  (defun ar/view-mode-set-cursor-type ()
    (setq cursor-type (if view-mode 'hollow 'box)))

  (define-global-minor-mode global-view-mode view-mode
    (lambda ()
      (if (and (not noninteractive)
               (derived-mode-p 'prog-mode
                               'outline-mode
                               'text-mode))
          (view-mode +1)
        (message "Ignoring view-mode for %s" major-mode)
        (view-mode -1)))))
