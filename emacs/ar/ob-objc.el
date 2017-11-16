;;; ob-objc.el --- Org babel Objectve-C support.

;;; Commentary:
;; Org babel Objectve-C helpers.

;;; Code:

(require 'ob)

(defcustom org-babel-objc-compile-command "clang -x objective-c -framework Foundation"
  "For example: \"clang -x objective-c -framework Foundation\"."
  :group 'org-babel
  :version "24.3"
  :type 'string)

(defun org-babel-execute:objc (body params)
  "Compile Objective-C BODY with org PARAMS and execute binary."
  (let* ((src-file (org-babel-temp-file "org-babel-objc-block-" ".m"))
         (cmpflag (or (cdr (assq :cmpflag params)) ""))
         (full-body (org-babel-expand-body:generic body params))
         (bin-file
          (org-babel-process-file-name
           (org-babel-temp-file "org-babel-objc-block-" org-babel-exeext))))
    (with-temp-file src-file (insert full-body))
    (org-babel-eval
     (concat org-babel-objc-compile-command " " cmpflag " " src-file " " "-o" " " bin-file) "")

    ;; Using 2>&1 since org babel does not include stderr in output from NSLog.
    (let ((results (org-babel-eval (concat (org-babel-process-file-name bin-file) " 2>&1")  "")))
      (org-babel-reassemble-table
       (org-babel-result-cond (cdr (assq :result-params params))
         (org-babel-read results)
         (let ((tmp-file (org-babel-temp-file "c-")))
           (with-temp-file tmp-file (insert results))
           (org-babel-import-elisp-from-file tmp-file)))
       (org-babel-pick-name
        (cdr (assq :colname-names params)) (cdr (assq :colnames params)))
       (org-babel-pick-name
        (cdr (assq :rowname-names params)) (cdr (assq :rownames params)))))))

(provide 'ob-objc)

;;; ob-objc.el ends here
