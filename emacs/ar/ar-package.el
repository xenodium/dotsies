;;; ar-package.el --- Package support

;;; Commentary:
;; Package helpers.


;;; Code:

(require 'ar-process)
(require 'ar-python)
(require 'package)
(require 'tls)

(defun ar/package-initialize ()
  "Initialize package sources more securely.
Based on: https://glyph.twistedmatrix.com/2015/11/editor-malware.html"
  (ar/process-assert-binary-installed "gnutls-cli" "Install with: brew install gnutls or apt-get install gnutls-bin")
  (ar/python-assert-module-installed "certifi")
  (let ((trustfile
         (replace-regexp-in-string
          "\\\\" "/"
          (replace-regexp-in-string
           "\n" ""
           (ar/python-run-module-as-script "certifi")))))
    (setq tls-program
          (list
           (format "gnutls-cli --x509cafile %s -p %%p %%h" trustfile)))
    (setq gnutls-trustfiles (list trustfile)))
  (setq tls-checktrust t)
  (setq package-archives `(("gnu" . "https://elpa.gnu.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")))
  (package-initialize))

(provide 'ar-package)

;;; ar-package.el ends here
