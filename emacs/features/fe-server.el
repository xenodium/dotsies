;;; -*- lexical-binding: t; -*-

(use-package server
  :defer 10
  :config
  (unless (server-running-p)
    (server-start)))

(defun ar/emacs-server-file ()
  "Emacs server file"
  (nth 0 (split-string (shell-command-to-string "lsof -c Emacs | grep server | tr -s ' ' | cut -d' ' -f 8") "\n")))
