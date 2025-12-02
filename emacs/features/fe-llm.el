;;; -*- lexical-binding: t; -*-

(add-to-list 'load-path "/Users/alvaro/stuff/active/code/chatgpt-shell/")
(add-to-list 'load-path "/Users/alvaro/stuff/active/code/shell-maker/")
(add-to-list 'load-path "/Users/alvaro/stuff/active/code/ob-chatgpt-shell/")
(add-to-list 'load-path "/Users/alvaro/stuff/active/code/ob-dall-e-shell/")
(add-to-list 'load-path "/Users/alvaro/stuff/active/code/dall-e-shell/")
(add-to-list 'load-path "/Users/alvaro/stuff/active/code/acp.el/")
(add-to-list 'load-path "/Users/alvaro/stuff/active/code/agent-shell/")

(use-package eca :ensure t)

(use-package acp
  :config
  (use-package agent-shell
    :commands
    (agent-shell-anthropic-start-claude-code
     agent-shell-google-start-gemini
     agent-shell-openai-start-codex
     agent-shell-goose-start-agent
     agent-shell)
    :config
    (setq agent-shell-google-authentication
          (agent-shell-google-make-authentication
           :api-key (lambda () (nth 0 (process-lines "pass" "show" "google-key")))))
    (setq agent-shell-anthropic-authentication
          (agent-shell-anthropic-make-authentication
           :login t))
    (setq agent-shell-openai-authentication
          (agent-shell-openai-make-authentication
           :api-key (lambda () (nth 0 (process-lines "pass" "show" "openai-key")))))
    (setq agent-shell-goose-authentication
          (agent-shell-make-goose-authentication
           :openai-api-key (lambda () (nth 0 (process-lines "pass" "show" "openai-key")))))))

(use-package chatgpt-shell
  :commands
  (chatgpt-shell
   chatgpt-shell-prompt-compose)
  :validate-custom
  ((chatgpt-shell-model-version "chatgpt-4o-latest")
   (chatgpt-shell-deepseek-key
    (lambda ()
      (nth 0 (process-lines "pass" "show" "deepseek-key"))))
   (chatgpt-shell-openai-key
    (lambda ()
      (nth 0 (process-lines "pass" "show" "openai-key"))))
   (chatgpt-shell-perplexity-key
    (lambda ()
      (nth 0 (process-lines "pass" "show" "perplexity-key"))))
   (chatgpt-shell-kagi-key
    (lambda ()
      (nth 0 (process-lines "pass" "show" "kagi-key"))))
   (chatgpt-shell-openrouter-key
    (lambda ()
      (nth 0 (process-lines "pass" "show" "openrouter-key"))))
   (chatgpt-shell-google-key
    (lambda ()
      (nth 0 (process-lines "pass" "show" "google-key"))))
   (chatgpt-shell-anthropic-key
    (lambda ()
      (nth 0 (process-lines "pass" "show" "anthropic-key")))))
  :bind (("C-c C-e" . chatgpt-shell-prompt-compose)
         :map smartparens-mode-map
         (("C-c e" . chatgpt-shell-quick-insert))
         :map c-mode-map
         (("C-c e" . chatgpt-shell-quick-insert))
         :map org-mode-map
         (("C-c C-e" . chatgpt-shell-prompt-compose))
         :map objc-mode-map
         (("C-c C-e" . chatgpt-shell-prompt-compose))
         :map eshell-mode-map
         (("C-c C-e" . chatgpt-shell-prompt-compose))
         :map emacs-lisp-mode-map
         (("C-c C-e" . chatgpt-shell-prompt-compose))))

(use-package ob-chatgpt-shell
  :commands (org-babel-execute:chatgpt-shell)
  :config
  (ob-chatgpt-shell-setup))

(use-package ob-dall-e-shell
  :commands (org-babel-execute:dall-e-shell)
  :config
  (ob-dall-e-shell-setup))

(use-package dall-e-shell
  :validate-custom
  (dall-e-shell-openai-key
   (lambda ()
     (nth 0 (process-lines "pass" "show" "openai-key"))))
  (dall-e-shell-image-output-directory "~/Downloads/DALL-E"))
