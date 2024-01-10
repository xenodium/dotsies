;;; -*- lexical-binding: t; -*-

(use-package sqlite-mode
  :config
  ;; https://christiantietze.de/posts/2024/01/emacs-sqlite-mode-open-sqlite-files-automatically
  (defun ar/sqlite-redirect-open ()
    "Runs `sqlite-mode-open-file' on the file name visited by the
current buffer, killing it."

    (let ((file-name buffer-file-name))
      (kill-current-buffer)
      (sqlite-mode-open-file file-name)))

  (add-to-list 'magic-mode-alist '("SQLite format 3\x00" . ar/sqlite-redirect-open))

  (use-package sqlite-mode-extras
    :bind (:map
           sqlite-mode-map
           ("n" . next-line)
           ("p" . previous-line)
           ("b" . sqlite-mode-extras-backtab-dwim)
           ("f" . sqlite-mode-extras-tab-dwim)
           ("+" . sqlite-mode-extras-add-row)
           ("D" . sqlite-mode-extras-delete-row-dwim)
           ("C" . sqlite-mode-extras-compose-and-execute)
           ("E" . sqlite-mode-extras-execute)
           ("S" . sqlite-mode-extras-execute-and-display-select-query)
           ("DEL" . sqlite-mode-extras-delete-row-dwim)
           ("g" . sqlite-mode-extras-refresh)
           ("<backtab>" . sqlite-mode-extras-backtab-dwim)
           ("<tab>" . sqlite-mode-extras-tab-dwim)
           ("RET" . sqlite-mode-extras-ret-dwim))))
