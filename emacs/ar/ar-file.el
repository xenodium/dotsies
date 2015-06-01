;;; ar-file.el --- File support.

;;; Commentary:
;; File helpers.


;;; Code:

(require 'ar-string)
(require 'simple)

(defun ar/file-read-image-name ()
  "Read image file name."
  (read-file-name "Choose image: " nil nil t nil
                  (lambda (path)
                    (or (string-match-p "\\(\\.JPG\\|\\.jpg\\|\\.PNG\\|\\.png\\)" path)
                        (file-directory-p path)))))

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

(defun ar/file-create-non-existent-directory ()
  "Create a non-existent directory."
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it? " parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions
             #'ar/file-create-non-existent-directory)

(provide 'ar-file)

;;; ar-file.el ends here
