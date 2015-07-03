;;; ar-file.el --- File support.

;;; Commentary:
;; File helpers.


;;; Code:

(require 'ar-string)
(require 'simple)

(defun ar/file-file-p (path)
  "Return t if PATH is file.  nil otherwise."
  (and (file-exists-p path)
       (not (nth 0 (file-attributes path 'string)))))

;; From: http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer
(defun ar/file-rename-current ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

;;  From http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
(defun ar/file-delete-current-file ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

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
       (progn ,@body))))

(defun ar/file-read-image-name ()
  "Read image file name."
  (read-file-name "Choose image: " nil nil t nil
                  (lambda (path)
                    (or (string-match-p "\\(\\.JPG\\|\\.jpg\\|\\.PNG\\|\\.png\\|\\.GIF\\|\\.gif\\)" path)
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

(defun ar/file-parent-directory (path)
  "Get parent directory for PATH."
  (unless (equal "/" path)
    (file-name-directory (directory-file-name path))))

(defun ar/file-find-upwards (path filename)
  "Search upwards from PATH for a file named FILENAME."
  (let ((file (concat path filename))
        (parent (ar/file-parent-directory (expand-file-name path))))
    (if (file-exists-p file)
        file
      (when parent
        (ar/file-find-upwards parent filename)))))

(defun ar/file-open-closest (filename)
  "Open the closest FILENAME in current or parent dirs (handy for finding Makefiles)."
  (let ((closest-file-path (ar/file-find-upwards (buffer-file-name)
                                                 filename)))
    (unless closest-file-path
      (error (format "No %s found" filename)))
    (message closest-file-path)
    (switch-to-buffer (find-file-noselect closest-file-path))))

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
          (ar/file-find "\\*" default-directory))
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

(provide 'ar-file)

;;; ar-file.el ends here
