;;; ar-linux.el --- Linux support.

;;; Commentary:
;; Linux helpers.


;;; Code:

;;  From http://doc.rix.si/org/fsem.html
(defun ar/linux-p ()
  "Return t if the system is a GNU/Linux machine, nil otherwise."
  (string-equal system-type "gnu/linux"))

(defun ar/linux-init ()
  "Perform initializations for Linux."
  (unless (ar/linux-p)
    (error "Loading linux config on a different platform"))
  (setq exec-path (append exec-path '("~/local/bin"))))

(when (ar/linux-p)
  (ar/linux-init))

(provide 'ar-linux)

;;; ar-linux.el ends here
