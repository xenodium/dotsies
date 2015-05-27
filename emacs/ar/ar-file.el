;;; ar-file.el --- File support.

;;; Commentary:
;; File helpers.


;;; Code:

(require 'ar-string)
(require 'simple)

(defun ar/file-read-image-name ()
  "Read image file name."
  (read-file-name "Choose image: " nil nil t nil
                  (lambda (file)
                    (string-match-p "\\(\\.JPG\\|\\.jpg\\|\\.PNG\\|\\.png\\)" file))))

(defun ar/file-find (filename-pattern &rest search-paths)
  "Find file with FILENAME-PATTERN and SEARCH-PATHS."
  (let* ((search-paths-string (mapconcat 'expand-file-name
                                         search-paths
                                         " "))
         (find-command (format "find %s -iname %s"
                               search-paths-string
                               filename-pattern)))
    (split-string (shell-command-to-string find-command) "\n")))

(defun ar/file-grep (regexp filename-pattern &rest search-paths)
  "Grep for REGEXP and narrow to FILENAME-PATTERN and SEARCH-PATHS."
  (let* ((grep-command (format
                        (ar/string-spc-join "grep"
                                            "--binary-file=without-match"
                                            "--recursive"
                                            "--no-filename"
                                            "--regexp=%s"
                                            "--include %s"
                                            "%s")
                        regexp
                        filename-pattern
                        (apply #'ar/string-spc-join search-paths))))
    (split-string (shell-command-to-string grep-command) "\n")))

(provide 'ar-file)

;;; ar-file.el ends here
