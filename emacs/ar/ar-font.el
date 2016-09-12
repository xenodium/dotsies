;;; ar-font.el --- Font support.

;;; Commentary:
;; Font helpers.


;;; Code:

(require 'cl)

(defun ar/font-assert-installed (font-name instructions)
  (assert (member font-name
                  (font-family-list))
          nil
          (format "Error: font \"%s\" not found. %s" font-name instructions)))

(provide 'ar-font)

;;; ar-font.el ends here
