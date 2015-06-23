;;; ar-process.el --- Process support.

;;; Commentary:
;; Process helpers.


;;; Code:

(defun ar/process-call (program &rest arguments)
  "Call PROGRAM with ARGUMENTS.  Look out for *PROGRAM* buffer output."
  (apply #'call-process (append (list program
                                      nil
                                      (format "*%s*"
                                              (file-name-nondirectory program))
                                      nil)
                                arguments)))

(provide 'ar-process)

;;; ar-process.el ends here
