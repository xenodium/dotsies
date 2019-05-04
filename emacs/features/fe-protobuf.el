(use-package protobuf-mode
  :ensure t
  :mode (("\\.proto\\'" . protobuf-mode))
  :hook ((protobuf-mode . goto-address-prog-mode))
  :config
  (defun ar/reindex-proto-fields ()
    "From within a proto message, reindex all proto field tags."
    (interactive)
    (save-excursion
      (save-restriction
        (narrow-to-defun)
        (goto-char (point-min))
        (let ((counter 1))
          (while (search-forward-regexp "\\(\\(\\(optional\\)\\|\\(required\\)\\).*= *\\)[1-9]+" nil t)
            (replace-match (format "\\1%d" counter) t nil)
            (setq counter (+ counter 1))))))))
