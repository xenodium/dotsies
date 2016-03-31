;;; ar-shell-command.el --- Shell command support.

;;; Commentary:
;; Shell command helpers.


;;; Code:

(defmacro ar/shell-command-start (name buffer command completion-fun)
  "Start shell command with NAME BUFFER name COMMAND and COMPLETION-FUN."
  `(set-process-sentinel
    (start-process-shell-command
     ,name
     ,buffer
     ,command)
    (lambda (process state)
      (assert (= (process-exit-status process) 0) nil (format "failed: %s"
                                                              ,command))
      (funcall ,completion-fun))))

(provide 'ar-shell-command)

;;; ar-shell-command.el ends here
