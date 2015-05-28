;;; ar-helm.el --- Basic helm support.

;;; Commentary:
;; Basic helm helpers.


;;; Code:

(require 'helm)

(defvar ar/helm-search-paths '()
  "Search paths to be utilized in find and search.")

(defun ar/helm-helm (title candidates on-select-function)
  "Helm with TITLE CANDIDATES and ON-SELECT-FUNCTION."
  (helm :sources `((name . ,title)
                   (candidates . ,candidates)
                   (action . ,on-select-function))
        :buffer "*helm-exec*"
        :candidate-number-limit 10000))

(defun ar/helm-find ()
  "Search files in ar/helm-search-files variable."
  (interactive)
  (unless (> (length ar/helm-search-paths) 0)
    (error "No search paths available (see ar/helm-search-paths)"))
  (ar/helm-helm "Search in path"
                'ar/helm-search-paths
                (lambda (path)
                  (helm-find-1 path))))

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
