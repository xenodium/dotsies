;;; -*- lexical-binding: t; -*-

(use-package chatgpt-shell
  :commands
  (chatgpt-shell
   chatgpt-shell-prompt-compose)
  :validate-custom
  ((chatgpt-shell-model-version "chatgpt-4o-latest")
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
         (("C-c C-e" . chatgpt-shell-prompt-compose)))
  :config
  (add-to-list 'display-buffer-alist
               (cons '(major-mode . chatgpt-shell-prompt-compose-mode)
                     `((display-buffer-reuse-mode-window
                        (lambda (buffer alist) ;; Use left side window if one available.
                          (when (window-combination-p (frame-root-window (selected-frame)) t)
                            (window--display-buffer buffer
                                                    (car (window-at-side-list nil 'left))
                                                    'reuse alist)))
                        display-buffer-in-direction)
                       (window-width . 0.45)
                       (direction . left)))))

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
