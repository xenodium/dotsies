;;; last-change-jump.el --- Jump to last text change.

;;; Commentary:
;; Jump to the last text change across all buffers.

;; Enable/disable with `global-last-change-jump-mode'.
;; Jump back to last change with `last-change-jump'.

;;; Code:

(defvar last-change-jump--last-change-marker nil "Marker to last edit.")

(define-minor-mode last-change-jump-mode
  "Jump to the last change made across all buffers.
Can only be used after enabling `global-last-change-jump-mode'"
  :lighter " jump last"
  (if last-change-jump-mode
      (add-hook 'after-change-functions #'last-change-jump--text-change t)
    (remove-hook 'after-change-functions #'last-change-jump--text-change)
    (setq last-change-jump--last-change-marker nil)))

(define-globalized-minor-mode
  global-last-change-jump-mode
  last-change-jump-mode
  last-change-jump-mode)

(defun last-change-jump ()
  "Jump to last change across all buffers."
  (interactive)
  (unless last-change-jump-mode
    (error "Mode last-change-jump-mode not enabled"))
  (unless last-change-jump--last-change-marker
    (error "No changes registered"))
  (unless (marker-buffer last-change-jump--last-change-marker)
    (error "Buffer no longer exists"))
  (switch-to-buffer (marker-buffer last-change-jump--last-change-marker))
  (goto-char last-change-jump--last-change-marker))

(defun last-change-jump--text-change (beginning end length)
  "Monitors text change BEGINNING, END, and LENGTH."
  (when (and
         ;; Is change in a focused visible buffer?
         (eq (current-buffer) (window-buffer (selected-window)))
         ;; Is change in a buffer other than a minibuffer?
         (not (minibufferp (current-buffer)))
         ;; Is a programming language, plain text, or interpreter buffer?
         (derived-mode-p 'prog-mode 'text-mode 'comint-mode))
    (setq last-change-jump--last-change-marker (copy-marker (point)))))

(provide 'last-change-jump)

;;; last-change-jump.el ends here
