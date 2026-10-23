;;; -*- lexical-binding: t; -*-

(use-package ar-misc
  :bind (("C-x n n" . ar/misc-narrow-or-widen-dwim))
  :commands (ar/misc-clipboard-to-qr
             ar/misc-pick-font
             ar/misc-financial-times-lookup-symbol
             ar/misc-open-clipboard-file
             ar/misc-hash-region
             ar/misc-open-file-at-point
             ar/misc-download-clipboard-url
             ar/misc-diff-last-2-yanks))

(defun ar/toggle-redact-buffer ()
  "Redact buffer content matching regexp. A space redacts all."
  (interactive)
  (let* ((redacted)
         (regexp (string-trim (read-regexp "Redact regexp" 'regexp-history-last)))
         (matches (let ((results '()))
                    (when (string-empty-p regexp)
                      (setq regexp "[[:graph:]]")
                      (setq regexp-history-last regexp)
                      (add-to-history 'regexp-history regexp))
                    (save-excursion
                      (goto-char (point-min))
                      (while (re-search-forward regexp nil t)
                        (push (cons (match-beginning 0) (match-end 0)) results)))
                    (nreverse results))))
    (mapc (lambda (match)
            (dolist (overlay (overlays-in (car match) (cdr match)))
              (when (overlay-get overlay 'ar/redacted)
                (setq redacted t)
                (delete-overlay overlay)))
            (unless redacted
              (let ((ov (make-overlay (car match) (cdr match))))
                (overlay-put ov 'ar/redacted t)
                (overlay-put ov 'display (make-string (- (cdr match) (car match)) ?x)))))
          matches)))
