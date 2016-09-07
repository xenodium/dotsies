;;; ar-typescript.el --- Typescript support.

;;; Commentary:
;; Typescript helpers.


;;; Code:

(require 'ar-process)

(defun ar/typescript-format-buffer ()
  "Format buffer using tsfmt."
  (interactive)
  (ar/process-assert-binary-installed "tsfmt" "Install with: npm install -g typescript-formatter")
  (when (= 0 (call-process "tsfmt" nil "*tsfmt*" t "--baseDir=." "-r" (buffer-file-name)))
    (message "Formatted %s" (buffer-file-name))
    (revert-buffer nil t)))

(provide 'ar-typescript)

;;; ar-typescript.el ends here
