;;; ar-magit.el --- Magit support.

;;; Commentary:
;; Magit helpers.


;;; Code:

(require 'magit)

(defun ar/magit-soft-reset-head~1 ()
  "Soft reset current git repo to HEAD~1."
  (interactive)
  (magit-reset-soft "HEAD~1"))

(provide 'ar-magit)

;;; ar-magit.el ends here
