(defun ar/comment-dwim ()
  "Comment current line or region."
  (interactive)
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (when (region-active-p)
      (ar/vsetq start (save-excursion
                             (goto-char (region-beginning))
                             (beginning-of-line)
                             (point))
                     end (save-excursion
                           (goto-char (region-end))
                           (end-of-line)
                           (point))))
    (comment-or-uncomment-region start end)))

(bind-key "M-;" #'ar/comment-dwim)

(defun ar/comment-dwim-next-line ()
  "Like `ar/comment-dwim', but also move to next line."
  (interactive)
  (call-interactively #'ar/comment-dwim)
  (next-line))

(bind-key "C-M-;" #'ar/comment-dwim-next-line)
