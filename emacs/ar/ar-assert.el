;;; ar-assert.el --- Aseertion support.

;;; Commentary:
;; Aseertion helpers.


;;; Code:

(require 'cl)

(defun ar/assert (value message)
  "Assert returning VALUE if not nil. MESSAGE otherwise."
  (assert value nil message)
  value)

(provide 'ar-assert)

;;; ar-assert.el ends here
