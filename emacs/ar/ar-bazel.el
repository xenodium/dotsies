;;; ar-bazel.el --- Bazel support.

;;; Commentary:
;; Bazel helpers.


;;; Code:

(require 'ar-file)
(require 's)

(defvar ar/bazel-qualify-regexp nil "For example: .*path/to/dir including WORKSPACE")

(defvar ar/bazel-compile-command "bazel build --ios_minimum_os=8.2")

(defun ar/bazel-compile ()
  "Invoke `'compile with `'completing-read a build rule in either current or parent directories."
  (interactive)
  (compile (format "%s %s" ar/bazel-compile-command
                   (ar/bazel-completing-read-build-rule))))

(defun ar/bazel-completing-read-build-rule ()
  "Find a build file in current or parent directories and `'completing-read a build rule."
  (let ((closest-build-file (ar/file-either-closest (if (equal major-mode 'dired-mode)
                                                        default-directory
                                                      (buffer-file-name)) "BUILD")))
    (assert closest-build-file nil "No BUILD found.")
    (format "%s:%s"
            (ar/bazel-qualified-package-path closest-build-file)
            (completing-read "build rule: " (ar/file-build-rule-names (with-temp-buffer
                                                                        (insert-file-contents closest-build-file)
                                                                        (buffer-string)))))))

(defun ar/bazel-qualified-package-path (path)
  "Convert PATH to google3-qualified package: /some/path/google3/package/BUILD => //package."
  (assert ar/bazel-qualify-regexp nil (format "%s must be set" 'ar/bazel-qualify-regexp))
  (replace-regexp-in-string ar/bazel-qualify-regexp "//" (s-chop-suffix "/" (file-name-directory (expand-file-name path)))))

(provide 'ar-bazel)

;;; ar-bazel.el ends here
