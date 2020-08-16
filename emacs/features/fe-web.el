;;; -*- lexical-binding: t; -*-

(use-package enlive
  :ensure t
  :defer 60)

(use-package ar-url
  :commands (ar/url-view-links-at))

;; Make URLs and e-mail addresses clickable or activatable wit <RET>.
(use-package goto-addr
  :bind (:map goto-address-highlight-keymap
              ("<RET>" . goto-address-at-point)
              ("M-<RET>" . newline))
  :commands (goto-address-prog-mode
             goto-address-mode))

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
  :defer 80
  :config
  (setq web-mode-auto-close-style 2))

(use-package url-handlers
  :commands ar/download-clipboard-url
  :config
  (defun ar/download-clipboard-url ()
    "Download clipboard URL to current directory."
    (interactive)
    (let ((url (current-kill 0)))
      (assert (string-match-p "^http[s]?://\\(www\\.\\)?" url)
              nil "Not a downloadable URL: %s" url)
      (url-copy-file url (concat default-directory (file-name-nondirectory url))))))
