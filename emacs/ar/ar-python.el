;;; ar-python.el --- Python support.

;;; Commentary:
;; Python helpers.


;;; Code:

(defun ar/python-run-module-as-script (module-name)
  "Return output of running MODULE-NAME as a script.  Nil if module not found."
  (let ((module-path (shell-command-to-string (format "python -m %s" module-name))))
    (if (string-match-p "No module named" module-path)
        nil
      module-path)))

(defun ar/python-certifi-trust-file-path ()
  "Return certifi trust file path."
  (let ((trust-file-path (ar/python-run-module-as-script "certifi")))
    (assert trust-file-path nil "certifi not installed. Try: python -m pip install --user certifi")
    (replace-regexp-in-string
     "\\\\" "/"
     (replace-regexp-in-string
      "\n" "" trust-file-path))))

(provide 'ar-python)

;;; ar-python.el ends here
