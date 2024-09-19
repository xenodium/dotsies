;;; bubble.el --- Expand region simultaneously in both directions  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; Version: 0.0.1

;;; Commentary:

;; Use your preferred binding to turn `bubble-mode' on and off.
;;
;; For example:
;;
;; (global-set-key (kbd "C-c C-w") 'bubble-mode)
;;
;; Enter with "C-c C-w" or "M-x bubble-mode".
;;
;; Exit with either RET or any command outside bubble-mode.
;;
;; Expand region with "p", "C-p", or "<up>"
;; Shrink region with "n", "C-n", or "<down>"

;;; Code:

(define-minor-mode bubble-mode
  "A minor to grow region in both vertical directions."
  :lighter " jump last"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<up>") #'bubble-expand)
            (define-key map (kbd "S-<up>") #'bubble-expand-top)
            (define-key map (kbd "<down>") #'bubble-shrink)
            (define-key map (kbd "S-<down>") #'bubble-expand-bottom)
            (define-key map (kbd "C-p") #'bubble-expand)
            (define-key map (kbd "S-C-p") #'bubble-move-up)
            (define-key map (kbd "S-C-n") #'bubble-move-down)
            (define-key map (kbd "p") #'bubble-expand)
            (define-key map (kbd "n") #'bubble-shrink)
            (define-key map (kbd "C-n") #'bubble-shrink)
            map)
  (if bubble-mode
      (progn
        (add-hook 'pre-command-hook #'bubble--pre-command-hook nil t)
        (add-hook 'post-command-hook #'bubble--post-command-hook nil t)
        (unless (region-active-p)
          (goto-char (line-beginning-position))
          (set-mark (save-excursion
                      (forward-line 1)
                      (point)))
          (activate-mark))
        (message "bubble-mode enabled"))
    (remove-hook 'pre-command-hook #'bubble--pre-command-hook t)
    (remove-hook 'post-command-hook #'bubble--post-command-hook t)
    (message "bubble-mode disabled")))

(defun bubble--post-command-hook ()
  "Post command hook."
  (unless (memq this-command '(bubble-mode
                               bubble-move-up
                               bubble-move-down
                               bubble-expand
                               bubble-expand-top
                               bubble-expand-bottom
                               bubble-shrink))
    (bubble-mode -1)))

(defun bubble--pre-command-hook ()
  "Pre command hook."
  (cond ((or (eq this-command 'newline)
             (eq this-command 'newline-and-indent))
         (setq this-command 'ignore)
         (bubble-mode -1))
        ((and (or (eq this-command 'self-insert-command)
                  (eq this-command 'org-self-insert-command))
              (string-match-p "^[0-9]+$" (this-command-keys)))
         (bubble-expand (string-to-number (this-command-keys)))
         (setq this-command 'bubble-expand))))

(defun bubble-expand (&optional count)
  "Expand region by COUNT lines."
  (interactive "P")
  (unless count
    (setq count 1))
  (when (eq count 0)
    (setq count 10))
  (when (>= (point) (mark))
    (exchange-point-and-mark))
  (forward-line (- count))
  (exchange-point-and-mark)
  (forward-line count)
  (exchange-point-and-mark))

(defun bubble-expand-top ()
  "Expand region top."
  (interactive)
  (when (>= (point) (mark))
    (exchange-point-and-mark))
  (forward-line -1))

(defun bubble-expand-bottom ()
  "Expand region bottom."
  (interactive)
  (when (<= (point) (mark))
    (exchange-point-and-mark))
  (forward-line))

(defun bubble-shrink ()
  "Shrink region."
  (interactive)
  (when (< (point) (mark))
    (exchange-point-and-mark))
  (forward-line -1)
  (exchange-point-and-mark)
  (forward-line 1))

(defun bubble-move-up ()
  "Shift the region up by one line."
  (interactive)
  (when (> (point) (mark))
    (exchange-point-and-mark))
  (forward-line -1)
  (forward-line 0)
  (exchange-point-and-mark)
  (forward-line -1)
  (end-of-line)
  (activate-mark)
  (exchange-point-and-mark))

(defun bubble-move-down ()
  "Shift the region down by one line."
  (interactive)
  (when (> (point) (mark))
    (exchange-point-and-mark))
  (forward-line)
  (forward-line 0)
  (exchange-point-and-mark)
  (forward-line)
  (end-of-line)
  (activate-mark)
  (exchange-point-and-mark))

(provide 'bubble)

;;; bubble.el ends here
