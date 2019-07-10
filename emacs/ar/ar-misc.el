;;; ar-misc.el --- Miscellaneous support. -*- lexical-binding: t; -*-

;;; Commentary:
;; Miscellaneous helpers.


;;; Code:

(defun ar/misc-clipboard-to-qr ()
  "Convert text in clipboard to qrcode and display within Emacs."
  (interactive)
  (let ((temp-file (concat (temporary-file-directory) "qr-code")))
    (if (eq 0 (shell-command (format "qrencode -s10 -o %s %s"
                                     temp-file
                                     (shell-quote-argument (current-kill 0)))
                             "*qrencode*"))
        (switch-to-buffer (find-file-noselect temp-file t))
      (error "Error: Could not create qrcode, check *qrencode* buffer"))))

(defcustom ar/misc-financial-symbols nil "Default financial symbols to look up (cons \"title\" \"symbol\")"
  :type 'list
  :group 'ar-misc)

(defun ar/misc-financial-times-lookup-symbol ()
  "Look up tearsheet for symbol at Financial Times."
  (interactive)
  (ivy-read "Symbol: " ar/misc-financial-symbols
            :action (lambda (item)
                      (assert (consp item) nil "List items must be a cons.")
                      (browse-url (format "https://markets.ft.com/data/funds/tearsheet/charts?s=%s"
                                          (cdr item))))))

;; From https://www.reddit.com/r/emacs/comments/b058f8/weekly_tipstricketc_thread/eilbynr
(defun ar/misc-diff-last-2-yanks ()
  "Run ediff on latest two entries in `kill-ring'."
  (interactive)
  ;; Implementation depends on `lexical-binding' being t, otherwise #'clean-up
  ;; will not be saved as closure to `ediff-cleanup-hook' and thus will lose
  ;; reference to itself.
  (let ((a (generate-new-buffer "*diff-yank*"))
        (b (generate-new-buffer "*diff-yank*")))
    (cl-labels ((clean-up ()
                          (kill-buffer a)
                          (kill-buffer b)
                          (remove-hook 'ediff-cleanup-hook #'clean-up)
                          (winner-undo)))
               (add-hook 'ediff-cleanup-hook #'clean-up)
               (with-current-buffer a
                 (insert (elt kill-ring 0)))
               (with-current-buffer b
                 (insert (elt kill-ring 1)))
               (ediff-buffers a b))))

(provide 'ar-misc)

;;; ar-misc.el ends here
