;;; -*- lexical-binding: t; -*-

(use-package sqlite-mode
  :config
  ;; https://christiantietze.de/posts/2024/01/emacs-sqlite-mode-open-sqlite-files-automatically
  (defun ar/sqlite-redirect-open ()
    "Runs `sqlite-mode-open-file' on the file name visited by the
current buffer, killing it."

    (let ((file-name buffer-file-name)
          (directory default-directory))
      (kill-current-buffer)
      (sqlite-mode-open-file file-name)
      (setq default-directory directory)))

  (add-to-list 'magic-mode-alist '("SQLite format 3\x00" . ar/sqlite-redirect-open))

  (use-package sqlite-mode-extras
    :hook ((sqlite-mode . sqlite-extras-minor-mode))))
