;;; company-bazel-swift-imports.el --- Company backend for Swift bazel imports.

;;; Commentary:
;; Company backend for Swift bazel imports helpers.


;;; Code:

(require 'cl-lib)
(require 'company)
(require 'ar-bazel)
(require 's)

(defun company-bazel-swift-imports--grap-symbol-cons ()
  "Return cons with symbol and t whenever prefix of \"import \" is found.
For example:
    \"import A\" -> (\"A\" . t)
"
  (when (looking-back "\\(import +\\)\\([a-zA-Z_]*\\)"
                      (line-beginning-position))
    (if (match-string-no-properties 2)
        (cons (match-string-no-properties 2) t)
      nil)))


(defun company-bazel-swift-imports (command &optional arg &rest ignored)
  "Company backend for completing Swift imports in a Bazel project."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-bazel-swift-imports))
    (prefix
     (company-bazel-swift-imports--grap-symbol-cons))
    (candidates
     (company-bazel-swift-imports-candidates (company-bazel-swift-imports--grap-symbol-cons)))
    (post-completion (when (looking-at-p "\"")
                       (forward-char)
                       (insert ",")))))

(defun company-bazel-swift-imports-candidates (value)
  "Get candidates for VALUE."
  (if (consp value)
      (let ((search-term (car value))
            (trigger-found (cdr value)))
        (when trigger-found
          (-map
           (lambda (item)
             ;; Results from BUILD files look like //package/subpackage:Target
             ;; Convert to package_subpackage_Target
             (s-replace-all (list (cons "//" "")
                                  (cons "/" "_")
                                  (cons ":" "_"))
                            item))
           (-filter
            (lambda (item)
              (when (s-contains-p search-term item t)
                item))
            (ar/bazel-workspace-build-rules)))))))

(provide 'company-bazel-swift-imports)

;;; company-bazel-swift-imports.el ends here
