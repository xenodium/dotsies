;;; ar-helm.el --- Basic helm support.

;;; Commentary:
;; Basic helm helpers.


;;; Code:

(require 'helm)

(defun ar/helm-helm (title candidates on-select-function)
  (helm :sources `((name . ,title)
                   (candidates . ,candidates)
                   (action . ,on-select-function))
        :buffer "*helm-exec*"
        :candidate-number-limit 10000))

(defmacro ar/helm-defhelm (name title candidates on-select-function)
  "Create a helm command with NAME, source TITLE, CANDIDATES list and
ON-SELECT-FUNCTION."
  (declare (indent 1))
  `(defun ,name ()
     (interactive)
     (ar/helm-helm ,title
                   ,candidates
                   ,on-select-function)))

;; defhelm examples:
;;
;; (defhelm ar/insert-java-import
;;   "My Java imports"
;;   (sort (delete-dups (ar/grep "^import"
;;                               "\\*.java"
;;                               "path/to/java/1"
;;                               "path/to/java/2"))
;;         'string<)
;;   (lambda (selection)
;;     (insert selection)))

(provide 'ar-helm)

;;; ar-helm.el ends here
