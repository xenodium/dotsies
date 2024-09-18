;;; bubble.el --- Simultaneously expand region in both directions  -*- lexical-binding: t -*-

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

(define-minor-mode bubble-mode
  "A minor to grow region in both vertical directions."
  :lighter " jump last"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<up>") #'bubble-expand-region)
            (define-key map (kbd "S-<up>") #'bubble-expand-region-top)
            (define-key map (kbd "<down>") #'bubble-shrink-region)
            (define-key map (kbd "S-<down>") #'bubble-expand-region-bottom)
            (define-key map (kbd "C-p") #'bubble-expand-region)
            (define-key map (kbd "p") #'bubble-expand-region)
            (define-key map (kbd "S-C-p") #'bubble-expand-region-top)
            (define-key map (kbd "n") #'bubble-shrink-region)
            (define-key map (kbd "C-n") #'bubble-shrink-region)
            (define-key map (kbd "S-C-n") #'bubble-expand-region-bottom)
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
  (unless (memq this-command '(bubble-mode
                               bubble-expand-region
                               bubble-expand-region-top
                               bubble-expand-region-bottom
                               bubble-shrink-region))
    (bubble-mode -1)))

(defun bubble--pre-command-hook ()
  (cond ((or (eq this-command 'newline)
             (eq this-command 'newline-and-indent))
         (setq this-command 'ignore)
         (bubble-mode -1))
        ((and (eq this-command 'self-insert-command)
              (string-match-p "^[0-9]+$" (this-command-keys)))
         (bubble-expand-region (string-to-number (this-command-keys)))
         (setq this-command 'bubble-expand-region))))

(defun bubble-expand-region (&optional count)
  (interactive)
  (unless count
    (setq count 1))
  (when (eq count 0)
    (setq count 10))
  (when (>= (point) (mark))  ;; Ensure point is above mark
    (exchange-point-and-mark))
  (forward-line (- count))
  (forward-char 0)
  (exchange-point-and-mark)
  (forward-line count)
  (forward-char 0)
  (exchange-point-and-mark))

(defun bubble-expand-region-top ()
  (interactive)
  (when (>= (point) (mark))
    (exchange-point-and-mark))
  (previous-line))

(defun bubble-expand-region-bottom ()
  (interactive)
  (when (<= (point) (mark))
    (exchange-point-and-mark))
  (next-line))

(defun bubble-shrink-region ()
  (interactive)
  (when (<= (point) (mark))
    (exchange-point-and-mark))
  (forward-line -1)
  (forward-char 0)
  (exchange-point-and-mark)
  (forward-line 1)
  (forward-char 0))
