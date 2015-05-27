;;; ar-string.el --- String support.

;;; Commentary:
;; String helpers.


;;; Code:

(defun ar/string-spc-join (&rest strings)
  "Join strings in STRINGS list with spaces."
  (mapconcat 'identity strings " "))

(provide 'ar-string)

;;; ar-string.el ends here
