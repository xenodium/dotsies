;;; -*- lexical-binding: t; -*-

;; Partially use path in buffer name.
(use-package uniquify
  :validate-custom
  (uniquify-buffer-name-style 'forward))

(use-package ibuffer
  :bind (:map ibuffer-mode-map)
  ("C-k" . ibuffer-do-delete))
