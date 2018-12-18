;;; ar-file.el --- File support.

;;; Commentary:
;; File helpers.


;;; Code:

(require 'ar-string)
(require 'files)
(require 'simple)
(require 'cl)

(defun ar/file-file-p (path)
  "Return t if PATH is file.  nil otherwise."
  (and (file-exists-p path)
       (not (nth 0 (file-attributes path 'string)))))

(defun ar/file-modification-time (file-path)
  "Return modification time for FILE-PATH or error."
  (assert (ar/file-file-p file-path) nil "File not found %s: " file-path)
  (nth 5 (file-attributes  file-path 'string)))

(defun ar/file-last-modified (file-paths)
  "Return last modified file path in FILE-PATHS."
  (assert (> (length file-paths) 0) nil "You need at least on path in FILE-PATHS")
  (let ((newest-file-path (nth 0 file-paths)))
    (mapc (lambda (file-path)
            (when (and (ar/file-file-p file-path)
                       (time-less-p (ar/file-modification-time newest-file-path)
                                    (ar/file-modification-time file-path)))
              (setq newest-file-path file-path))
            (message file-path))
          file-paths)
    newest-file-path))

;; Move buffer file.
;; From: https://sites.google.com/site/steveyegge2/my-dot-emacs-file
(defun ar/file-move (dir)
  "Move both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
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
  (assert filename-pattern nil "Missing FILENAME-PATTERN")
  (assert search-paths nil "Missing SEARCH-PATHS")
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

(defun ar/file-create-non-existent-directory ()
  "Create a non-existent directory."
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it? " parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions
             #'ar/file-create-non-existent-directory)

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
    (assert closest-fname nil "No %s found" fnames)
    (switch-to-buffer (find-file-noselect closest-fname))
    closest-fname))

(defvar ar/file-build-file-names '("BUILD" "SConstruct" "Makefile"))

(defun ar/file-open-closest-build-file ()
  "Open the closest build file in current or parent directory.
For example: Makefile, SConstruct, BUILD, etc.
Append `ar/file-build-file-names' to search for other file names."
  (interactive)
  (apply #'ar/file-open-either-closest (buffer-file-name) ar/file-build-file-names))

(defun ar/file-find-duplicate-filenames ()
  "Find files recursively which have the same name."
  (interactive)
  (let ((files-hash-table (make-hash-table :test 'equal))
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

(defun ar/file-dir-locals-directory ()
  "Get closest .dir-locals.el directory."
  (file-name-directory (if (stringp (dir-locals-find-file default-directory))
                           (dir-locals-find-file default-directory)
                         (car (dir-locals-find-file default-directory)))))

(defun ar/file-assert-file-exists (file-path)
  "Assert FILE-PATH exists."
  (assert (file-exists-p file-path) nil (format "File not found: %s" file-path))
  file-path)

(provide 'ar-file)

;;; ar-file.el ends here
