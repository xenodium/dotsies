;;; -*- lexical-binding: t; -*-

(use-package enlive
  :ensure t
  :defer)

(use-package ar-url
  :commands (ar/url-view-links-at))

;; Make URLs and e-mail addresses clickable or activatable wit <RET>.
(use-package goto-addr
  :functions (goto-address-at-point)
  :bind (:map goto-address-highlight-keymap
              ("<RET>" . goto-address-at-point)
              ("M-<RET>" . newline))
  :commands (goto-address-prog-mode
             goto-address-mode))

(use-package browse-url
  :after dwim-shell-command
  :config
  (defun ar/browse-url-browser (url &rest args)
    (if (and (or (string-match-p "^http[s]?://.*youtube.com" url)
                 (string-match-p "^http[s]?://.*m.youtube.com" url)
                 (string-match-p "^http[s]?://.*youtu.be" url)
                 (string-match-p "^http[s]?://.*soundcloud.com" url)
                 (string-match-p "^http[s]?://.*redditmedia.com" url)
                 (string-match-p "^http[s]?://.*bandcamp.com" url))
             (y-or-n-p "Stream from mpv? "))
        (dwim-shell-command-on-marked-files
         "Streaming"
         (format "mpv --geometry=30%%x30%%+100%%+0%% '%s'" url)
         :utils "mpv"
         :no-progress t
         :error-autofocus t
         :silent-success t)
      (funcall #'browse-url-default-browser url args)))
  (setq browse-url-browser-function #'ar/browse-url-browser))

(use-package xwidget
  :commands (xwidget-webkit-browse-url)
  :bind(:map
        xwidget-webkit-mode-map
        ("q" . kill-this-buffer))
  :config
  ;; C-x k don't ask if xwidget should be killed.
  (defun adviced:xwidget-kill-buffer-query-function (_)
    t)
  (advice-add #'xwidget-kill-buffer-query-function
              :around
              #'adviced:xwidget-kill-buffer-query-function))

(use-package css-mode
  :mode (("\\.css\\'" . css-mode)
         ("\\.rasi\\'" . css-mode)))

(use-package sgml-mode
  :bind(:map
        sgml-mode-map
        ("<tab>" . ar/indent-for-tab-command-dwim)))

(use-package auto-rename-tag
  :hook ((nxml-mode . auto-rename-tag-mode)
         (html-mode . auto-rename-tag-mode))
  :ensure t)

(use-package web-mode
  :ensure t
  :validate-custom
  (web-mode-auto-close-style 2))
