;;; -*- lexical-binding: t; -*-
(use-package newcomment
  :bind (:map
         prog-mode-map
         ;; Better comment functionality
         ;; https://emacsredux.com/blog/2020/06/10/comment-commands-redux/
         ("M-;" . 'comment-line)
         ("C-M-;" . 'comment-line)))

(use-package prog-mode
  :after (flycheck)
  :bind (:map
         prog-mode-map
         ([f6] . recompile)
         ("C-x C-q" . view-mode))
  :hook ((prog-mode . company-mode)
         (prog-mode . flycheck-mode)
         (prog-mode . flyspell-prog-mode)
         (prog-mode . yas-minor-mode)
         ;; Trying out native alternative.
         (prog-mode . centered-cursor-mode)
         (prog-mode . rainbow-mode)
         (prog-mode . goto-address-prog-mode))
  :config
  (require 'flyspell)
  (require 'flycheck)

  (use-package reformatter
    :ensure t
    :config)

  ;; Highlight hex strings in respective color.
  (use-package rainbow-mode
    :ensure t
    :config
    ;; Enable more color highlighting cases in prog modes.
    (mapc (lambda (mode)
            (add-to-list 'rainbow-x-colors-major-mode-list mode)
            (add-to-list 'rainbow-html-colors-major-mode-list mode))
          '(objc-mode
            swift-mode))))
