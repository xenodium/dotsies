(use-package markdown-mode
  :ensure t
  :mode (("\\.text\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode))
  :hook ((markdown-mode . ar/markdown-mode-hook)
         (markdown-mode . ar/whitespace-mode-enable))
  :validate-custom (markdown-asymmetric-header t)
  :bind (:map
         markdown-mode-map
         ("M-<left>" . markdown-promote)
         ("M-<right>" . markdown-demote)
         ("M-[" . markdown-promote)
         ("M-]" . markdown-demote))
  :config
  (defun ar/markdown-mode-hook ()
    "Called when entering `markdown-mode'."
    (set-fill-column 80))

  (defun adviced:markdown-demote (orig-fun &rest r)
    ;; If not on anything, demote as header.
    (if (not (or (thing-at-point-looking-at markdown-regex-header-atx)
                 (thing-at-point-looking-at markdown-regex-header-setext)
                 (thing-at-point-looking-at markdown-regex-hr)
                 (markdown-cur-list-item-bounds)
                 (markdown-table-at-point-p)
                 (thing-at-point-looking-at markdown-regex-bold)
                 (thing-at-point-looking-at markdown-regex-italic)))
        (markdown-insert-header-atx-1)
      (apply orig-fun r)))

  (advice-add #'markdown-demote
              :around
              #'adviced:markdown-demote))
