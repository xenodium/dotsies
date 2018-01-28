;;; company-bazel.el --- Company grep support.

;;; Commentary:
;; Company grep helpers.


;;; Code:

(require 'cl-lib)
(require 'company)
(require 'ar-bazel)

(defun company-bazel--grap-symbol-cons ()
  "Return cons with symbol and t whenever prefix of \"// is found.
The prefix is typically seen in deps. For example:
deps = [
    \"//\"
    \"//package:target\",
],
"
  (when (looking-back "\"//\\([^\"]*\\)"
                      (line-beginning-position))
    (when (match-string-no-properties 1)
      (cons (match-string-no-properties 1) t))))

(defun company-bazel (command &optional arg &rest ignored)
  "Company backend for completing dependencies in BUILD files. See company.el for COMMAND, ARG, IGNORED details."
  (interactive (list 'interactive))
  (case command
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
          ))))

(provide 'company-bazel)

;;; company-bazel.el ends here
