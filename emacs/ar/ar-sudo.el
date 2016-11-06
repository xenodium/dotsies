;;; ar-sudo.el --- Sudo support.

;;; Commentary:
;; Sudo helpers.


;;; Code:

(require 'simple)

(defun ar/sudo-shell-command (command)
  "Promt for password and execute COMMAND."
  (shell-command (format "echo %s | sudo -S %s"
                         (shell-quote-argument (read-passwd "Password: "))
                         command)))

(provide 'ar-sudo)

;;; ar-sudo.el ends here
