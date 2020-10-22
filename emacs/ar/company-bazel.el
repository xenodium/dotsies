;;; company-bazel.el --- Company grep support.

;;; Commentary:
;; Company grep helpers.


;;; Code:

(require 'cl-lib)
(require 'company)
(require 'ar-bazel)

(defvar-local company-bazel--prefix nil)

(defun company-bazel--grap-symbol-cons ()
  "Return cons with symbol and t whenever prefix of \"// is found.
The prefix is typically seen in deps. For example:
    \"//A\" -> (\"A\" . t)
"
  (when (looking-back "\\(\\(//\\)\\|\\(:\\)\\)\\([^\"]*\\)"
                      (line-beginning-position))
    (setq company-bazel--prefix
          (match-string-no-properties 1))
    (when company-bazel--prefix
      (cons (match-string-no-properties 4) t))))

(defun company-bazel (command &optional arg &rest ignored)
  "Company backend for completing dependencies in BUILD files. See company.el for COMMAND, ARG, IGNORED details."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-bazel))
    (prefix
     (company-bazel--grap-symbol-cons))
    (candidates
     (company-bazel-candidates (company-bazel--grap-symbol-cons)))
    (post-completion (when (looking-at-p "\"")
                       (forward-char)
                       (insert ",")))))

(defun company-bazel-candidates (value)
  "Get candidates for VALUE."
  (if (consp value)
      (let ((search-term (car value))
            (trigger-found (cdr value)))
        (cond
         ((string-equal company-bazel--prefix "//")
          (when trigger-found
            (-map
             (lambda (item)
               ;; Results look like //package:Target
               ;; but prefix (//) has already been inserted by user.
               (s-chop-prefix "//" item))
             (-filter
              (lambda (item)
                (when (s-contains-p search-term item)
                  item))
              (ar/bazel-workspace-build-rules)))
            ))
         ((string-equal company-bazel--prefix ":")
          (ar/bazel-rule-names-in-build-file-path (buffer-file-name)))))))

(provide 'company-bazel)

;;; company-bazel.el ends here
