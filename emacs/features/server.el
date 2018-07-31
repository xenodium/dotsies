(use-package server
  :defer 10
  :config
  (unless (server-running-p)
    (server-start)))
