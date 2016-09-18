;;; ar-compile.el --- Compilation support.

;;; Commentary:
;; Compilation helpers.


;;; Code:

(require 'compile)

(defun ar/compile-completing ()
  "Compile with completing options."
  (interactive)
  (let ((compile-command (completing-read "Compile command: " compile-history)))
    (compile compile-command)
    ;; Deleting command and adding to beginning of list.
    (setq compile-history (delete compile-command compile-history))
    (add-to-list 'compile-history compile-command)))

(provide 'ar-compile)

;;; ar-compile.el ends here
