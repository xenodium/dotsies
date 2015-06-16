;;; ar-alist.el --- alist support.

;;; Commentary:
;; alist helpers.


;;; Code:

(defmacro ar/alist-append-to-value (alist key value)
  "Modify ALIST KEY with VALUE."
  `(let ((old-value (cdr (assoc ,key ,alist))))
     (unless old-value
       (error "%s must be found" ,key))
     (unless (listp old-value)
       (error "%s must be have a list" ,key))
     (push ,value (cdr (assoc ,key ,alist)))
     ,alist))

(provide 'ar-alist)

;;; ar-alist.el ends here
