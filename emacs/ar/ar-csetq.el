;;; ar-csetq.el --- alist support.

;;; Commentary:
;; alist helpers.


;;; Code:

;; Macro for setting custom variables.
;; Similar to custom-set-variables, but more like setq.

(defmacro ar/csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))

(provide 'ar-csetq)

;;; ar-csetq.el ends here
