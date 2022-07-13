;;; -*- lexical-binding: t; -*-

(use-package info-look
  :commands (info-lookup-symbol
             info-lookup-maybe-add-help)
  :config
  (add-to-list 'Info-directory-list "~/.emacs.d/info")

  (defun ar/format-info-mode ()
    "Opening .info files does not automatically set things up. Give it a little help."
    (interactive)
    (let ((file-name (buffer-file-name)))
      (kill-buffer (current-buffer))
      (info file-name))))
