;;; ar-compile.el --- Compilation support.

;;; Commentary:
;; Compilation helpers.


;;; Code:

(require 'compile)

(defun ar/compile-completing ()
  "Compile with completing options."
  (interactive)
  (compile (completing-read "Compile command: " compile-history)))

(provide 'ar-compile)

;;; ar-compile.el ends here
