;;; ar-misc.el --- Miscellaneous support.

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

(provide 'ar-misc)

;;; ar-misc.el ends here
