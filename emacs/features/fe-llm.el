;;; -*- lexical-binding: t; -*-

(use-package chatgpt-shell
  :commands
  (chatgpt-shell
   chatgpt-shell-prompt-compose)
  :validate-custom
  ((chatgpt-shell-openai-key
    (lambda ()
      (nth 0 (process-lines "pass" "show" "openai-key")))))
  :bind (("C-c C-e" . chatgpt-shell-prompt-compose)
         :map org-mode-map
         ("C-c C-e" . chatgpt-shell-prompt-compose)
         :map eshell-mode-map
         ("C-c C-e" . chatgpt-shell-prompt-compose)
         :map mu4e-compose-mode-map
         ("C-c C-e" . chatgpt-shell-prompt-compose)
         :map emacs-lisp-mode-map
         ("C-c C-e" . chatgpt-shell-prompt-compose))
  :config
  (add-to-list 'display-buffer-alist
               (cons '(major-mode . chatgpt-shell-prompt-compose-mode)
                     '((display-buffer-reuse-window
                        display-buffer-in-direction)
                       (reusable-frames . visible)
                       (direction . left)
                       (window-width . 0.35)))))

(use-package dall-e-shell
  :validate-custom
  (dall-e-shell-model-version "dall-e-3")
  (dall-e-shell-openai-key
   (lambda ()
     (nth 0 (process-lines "pass" "show" "openai-key"))))
  (dall-e-shell-image-output-directory "~/Downloads/DALL-E"))
