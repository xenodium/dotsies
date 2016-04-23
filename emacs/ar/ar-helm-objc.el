;;; helm-objc.el --- Helm Objective-C support.

;;; Commentary:
;; Helm Objective-C helpers.


;;; Code:

(require 'ar-file)
(require 'ar-helm)
(require 'ar-helm-text)
(require 'ar-objc)

(defun ar/helm-objc-import-update ()
  "Update list of imports."
  (interactive)
  (ar/helm-defhelm ar/helm-objc-import
    "My ObjC headers"
    (sort
     (delete-dups (mapcar #'file-name-nondirectory
                          (ar/file-find "\\*.h"
                                        nil
                                        "~/stuff/active")))
     'string<)
    (lambda (selection)
      (ar/objc-import nil selection))))

(defun ar/helm-objc-insert-relative-file-path ()
  (interactive)
  (ar/helm-text-insert-file-path
   "." "\\*.[hm]" (lambda (path)
                    (string-remove-prefix "./" path))))

(provide 'ar-helm-objc)

;;; helm-objc.el ends here
