;;; ar-osx.el --- Mac OS X support.

;;; Commentary:
;; Mac OS X helpers.


;;; Code:

;; Based on http://hints.macworld.com/article.php?story=20050526162847879
;; Convert plists on Mac OS to xml equivalent and open.
(push '(".plist'" . ar/convert-plist-to-xml) auto-mode-alist)

(defun ar/osx-convert-plist-to-xml ()
  (interactive)
  (when (string-match "plist"
                      (buffer-string))
    (shell-command-on-region (point-min) (point-max)
                             ;; yes, the temp file is necessary :-(
                             (format "plutil -convert xml1 -o /tmp/temp.plist %s; cat /tmp/temp.plist"
                                     (shell-quote-argument (buffer-file-name)))
                             t t))
  (set-buffer-modified-p nil)
  (nxml-mode))

(provide 'ar-osx)

;;; ar-osx.el ends here
