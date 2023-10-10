;;; -*- lexical-binding: t; -*-

(use-package chatgpt-shell
  :commands
  chatgpt-shell
  :validate-custom
  ((chatgpt-shell-openai-key
    (lambda ()
      (nth 0 (process-lines "pass" "show" "openai-key")))))
  :bind
  (("C-c C-e" . chatgpt-shell-prompt-compose)))

(use-package dall-e-shell
  :validate-custom
  ((dall-e-shell-openai-key
    (lambda ()
      (nth 0 (process-lines "pass" "show" "openai-key"))))))
