;;; helm-objc.el --- Helm Objective-C support.

;;; Commentary:
;; Helm Objective-C helpers.


;;; Code:

(require 'ar-file)
(require 'ar-helm)
(require 'ar-objc)

(defun ar/helm-objc-import-update ()
  "Update list of imports."
  (interactive)
  (ar/helm-defhelm ar/helm-objc-import
    "My ObjC imports"
    (sort
     (delete-dups (mapcar #'file-name-nondirectory
                          (ar/file-find "\\*.h"
                                        "~/stuff/active")))
     'string<)
    (lambda (selection)
      (ar/objc-import nil selection))))

(provide 'ar-helm-objc)

;;; helm-objc.el ends here
