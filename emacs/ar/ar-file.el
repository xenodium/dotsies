;;; ar-file.el --- File support.

;;; Commentary:
;; File helpers.


;;; Code:

(require 'ar-string)
(require 'files)
(require 'simple)
(require 'cl-lib)


;; Move buffer file.
;; From: https://sites.google.com/site/steveyegge2/my-dot-emacs-file
(defun ar/file-move (dir)
  "Move both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir (if (string-match dir "\\(?:/\\|\\\\)$")
                  (substring dir 0 -1)
                dir))
         (newname (concat dir "/" name)))

    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn (copy-file filename newname 1)
             (delete-file filename)
             (set-visited-file-name newname)
             (set-buffer-modified-p nil) t))))

(defmacro ar/file-with-current-file (file-path &rest body)
  "Open file at FILE-PATH and execute BODY."
  (declare (indent 1))
  `(with-current-buffer (find-file-noselect (expand-file-name ,file-path))
     (save-excursion
       (save-restriction
         (goto-char 0)
         (progn ,@body)))))

(defun ar/file-read-image-name ()
  "Read image file name."
  (read-file-name "Choose image: " nil nil t nil
                  (lambda (path)
                    (or (string-match-p "\\(\\.JPG\\|\\.jpg\\|\\.PNG\\|\\.png\\|\\.GIF\\|\\.gif\\)" path)
                        (file-directory-p path)))))

(defun ar/file-find (filename-pattern mod-function &rest search-paths)
  "Find file with FILENAME-PATTERN, map MOD-FUNCTION to results, look in SEARCH-PATHS."
  (cl-assert filename-pattern nil "Missing FILENAME-PATTERN")
  (cl-assert search-paths nil "Missing SEARCH-PATHS")
  (let* ((search-paths-string (mapconcat 'expand-file-name
                                         search-paths
                                         " "))
         (find-command (format "find %s -iname %s"
                               search-paths-string
                               filename-pattern))
         (results (split-string (shell-command-to-string find-command) "\n" t)))
    (if mod-function
        (mapcar mod-function
                results)
      results)))

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

(defun ar/file-parent-directory (path)
  "Get parent directory for PATH."
  (unless (equal "/" path)
    (file-name-directory (directory-file-name path))))

(defun ar/file-either-closest (start-fname &rest fnames)
  "Return the closest file, start at START-FNAME and go to parent dir until finding filename in FNAMES."
  (let ((dpath-found)
        (fpath))
    (-each-while
        fnames (lambda (fname)
                 ;; Continue while no path found.
                 (not dpath-found))
      (lambda (fname)
        (setq dpath-found
              (locate-dominating-file start-fname fname))
        (when dpath-found
          (setq fpath (concat (file-name-as-directory dpath-found)
                              fname)))))
    fpath))

(defun ar/file-open-either-closest (start-fname &rest fnames)
  "Open the closest file, start at START-FNAME and go to parent dir until finding filename in FNAMES."
  (let ((closest-fname (apply #'ar/file-either-closest start-fname fnames)))
    (cl-assert closest-fname nil "No %s found" fnames)
    (switch-to-buffer (find-file-noselect closest-fname))
    (goto-char (point-min))
    (search-forward (file-name-nondirectory start-fname))
    (backward-char (length (file-name-nondirectory start-fname)))
    closest-fname))

(defvar ar/file-build-file-names '("BUILD" "SConstruct" "Makefile" "Package.swift"))

(defun ar/file-open-closest-build-file ()
  "Open the closest build file in current or parent directory.
For example: Makefile, SConstruct, BUILD, etc.
Append `ar/file-build-file-names' to search for other file names."
  (interactive)
  (apply #'ar/file-open-either-closest (buffer-file-name) ar/file-build-file-names))

(defun ar/file-find-duplicate-filenames ()
  "Find files recursively which have the same name."
  (interactive)
  (let* ((files-hash-table (make-hash-table :test 'equal))
         (duplicate-file-names '())
         (record-function (lambda (path)
                            "Add to hash-table, key=filename value=paths."
                            ;; nil means file.
                            (unless (nth 0 (file-attributes path 'string))
                              (let ((key (file-name-nondirectory path))
                                    (values))
                                (setq values (gethash key files-hash-table))
                                (unless values
                                  (setq values '()))
                                (push path values)
                                (puthash key values files-hash-table))))))
    (mapc record-function
          (ar/file-find "\\*" nil default-directory))
    (maphash (lambda (key paths)
               (when (> (length paths) 1)
                 (mapc (lambda (path)
                         (push path duplicate-file-names))
                       paths)))
             files-hash-table)
    (when (eq (length duplicate-file-names) 0)
      (error "No duplicates found"))
    (switch-to-buffer (get-buffer-create "*Duplicates*"))
    (erase-buffer)
    (mapc (lambda (duplicate-file-name)
            (insert (format "%s\n" duplicate-file-name)))
          duplicate-file-names)))

(defun ar/file-assert-file-exists (file-path)
  "Assert FILE-PATH exists."
  (cl-assert (file-exists-p file-path) nil (format "File not found: %s" file-path))
  file-path)

(provide 'ar-file)

;;; ar-file.el ends here
