;;; -*- lexical-binding: t; -*-

(use-package newcomment
  :bind (:map
         prog-mode-map
         ;; Better comment functionality
         ;; https://emacsredux.com/blog/2020/06/10/comment-commands-redux/
         ("M-;" . 'comment-line)
         ("C-M-;" . 'comment-line)))

;; prog-mode is loaded super early during launch/initialization.
;; use-package :defer is ignored, so use a timer to defer.
(run-with-idle-timer
 25 nil
 (lambda ()
   (use-package prog-mode
     :bind (:map
            prog-mode-map
            ("C-x C-q" . view-mode))
     :hook ((prog-mode . company-mode)
            ;; Trying without (for performance).
            ;;         (prog-mode . flycheck-mode)
            ;;         (prog-mode . flyspell-prog-mode)
            (prog-mode . yas-minor-mode)
            ;; Trying to go without.
            ;; (prog-mode . centered-cursor-mode)
            (prog-mode . rainbow-mode)
            (prog-mode . goto-address-prog-mode))
     :config
     (message "--->?")
     ;; Trying without (for performance).
     ;;  (require 'flyspell)
     ;;  (require 'flycheck)

     ;; Highlight hex strings in respective color.
     (use-package rainbow-mode
       :ensure t
       :config
       ;; Enable more color highlighting cases in prog modes.
       (mapc (lambda (mode)
               (add-to-list 'rainbow-x-colors-major-mode-list mode)
               (add-to-list 'rainbow-html-colors-major-mode-list mode))
             '(objc-mode
               swift-mode))))))

(use-package reformatter
  :defer 30
  :ensure t)
