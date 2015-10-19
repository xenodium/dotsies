;;; ar-input.el --- Input prompts.

;;; Commentary:
;; Input prompts helpers.


;;; Code:

(defun ar/input-clipboard-url-or-prompt ()
  "Return a URL from clipboard or prompt user for one."
  (let* ((clipboard (current-kill 0))
         (url (if (string-match "^http://" clipboard)
                  clipboard
                (read-string "URL: "))))
    (unless (string-match "^http://" url)
      (error "Not a URL"))
    url))

(provide 'ar-input)

;;; ar-input.el ends here
