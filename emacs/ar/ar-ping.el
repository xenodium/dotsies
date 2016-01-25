;;; ar-ping.el --- Ping support.

;;; Commentary:
;; Ping helpers.


;;; Code:

(require 'net-utils)

(defun ar/ping-google ()
  "Ping google.com."
  (interactive)
  (let ((ping-program-options '("-c" "4")))
    (ping "google.com")))

(provide 'ar-ping)

;;; ar-ping.el ends here
