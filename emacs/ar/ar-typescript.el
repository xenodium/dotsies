;;; ar-typescript.el --- Typescript support.

;;; Commentary:
;; Typescript helpers.


;;; Code:



(provide 'ar-typescript)

(defun ar/typescript-format-buffer ()
  "Format buffer using tsfmt."
  (interactive)
  (call-process "tsfmt" nil "*tsfmt*" t "--baseDir=." "-r" "index.tsx")
  (revert-buffer nil t))

;;; ar-typescript.el ends here
