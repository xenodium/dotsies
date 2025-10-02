;; Match theme color early on (smoother transition).
;; Theme loaded in features/ui.el.
(add-to-list 'default-frame-alist '(background-color . "#212121"))

(when (< emacs-major-version 29)
  ;; Same for mode-line (hide it).
  (with-eval-after-load 'faces
    (let ((line (face-attribute 'mode-line :underline)))
      (set-face-attribute 'mode-line nil
                          :background "#212121"
                          :foreground "#212121"
                          :overline line
                          :box nil)
      (set-face-attribute 'mode-line-inactive nil
                          :overline line
                          :underline line
                          :box nil))))

;; https://github.com/hlissner/doom-emacs/blob/58af4aef56469f3f495129b4e7d947553f420fca/core/core.el#L200
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

;; https://github.com/hlissner/doom-emacs/blob/58af4aef56469f3f495129b4e7d947553f420fca/core/core.el#L323
(setq frame-inhibit-implied-resize t)

;; https://github.com/hlissner/doom-emacs/blob/58af4aef56469f3f495129b4e7d947553f420fca/core/core.el#L331
(setq inhibit-compacting-font-caches t)

;; https://github.com/hlissner/doom-emacs/blob/58af4aef56469f3f495129b4e7d947553f420fca/core/core.el#L205
(setq idle-update-delay 1.0)

;; Don't want a mode line while loading init.
(setq mode-line-format nil)

;; No scrollbar by default.
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; No nenubar by default.
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; No toolbar by default.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; No tooltip by default.
(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))

;; No Alarms by default.
(setq ring-bell-function 'ignore)

(defun ar/show-welcome-buffer ()
  "Show *Welcome* buffer."
  (with-current-buffer (get-buffer-create "*Welcome*")
    (setq truncate-lines t)
    (ar/refresh-welcome-buffer)
    (setq cursor-type nil)
    (read-only-mode +1)
    (add-hook 'window-size-change-functions
              (lambda (_frame)
                (ar/refresh-welcome-buffer)) nil t)
    (add-hook 'window-configuration-change-hook
              #'ar/refresh-welcome-buffer nil t)
    (switch-to-buffer (current-buffer))
    (local-set-key (kbd "q") 'kill-this-buffer)))

(defun ar/refresh-welcome-buffer ()
  "Refresh welcome buffer content for WINDOW."
  (when-let* ((inhibit-read-only t)
              (welcome-buffer (get-buffer "*Welcome*"))
              (window (get-buffer-window welcome-buffer))
              (image-path "~/.emacs.d/emacs.png")
              (image (create-image image-path nil nil :max-height 300))
              (image-height (cdr (image-size image)))
              (image-width (car (image-size image)))
              (top-margin (floor (/ (- (window-height window) image-height) 2)))
              (left-margin (floor (/ (- (window-width window) image-width) 2)))
              (title "Welcome to Emacs"))
    (with-current-buffer welcome-buffer
      (erase-buffer)
      (setq mode-line-format nil)
      (goto-char (point-min))
      (insert (make-string top-margin ?\n))
      (insert (make-string left-margin ?\ ))
      (insert-image image)
      (insert "\n\n\n")
      (insert (make-string (floor (/ (- (window-width window) (* (string-width title) 1.2)) 2)) ?\ ))
      (insert (propertize title 'face '(:height 1.2))))))

(setq initial-scratch-message nil)
(setq inhibit-startup-screen t)

(when (< (length command-line-args) 2)
  (add-hook 'emacs-startup-hook (lambda ()
                                  (when (display-graphic-p)
                                    (ar/show-welcome-buffer)))))
