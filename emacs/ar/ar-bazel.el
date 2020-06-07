;;; ar-bazel.el --- Bazel support. -*- lexical-binding: t; -*-

;;; Commentary:
;; Bazel helpers.


;;; Code:

(require 'ar-file)
(require 's)
(require 'f)
(require 'dash)

(defvar ar/bazel-compile-command "bazel build")

(defvar ar/bazel-command "bazel"
  "With a \"bazel\" prefix, we'd get linked directories like:
bazel-bin, bazel-genfiles, and bazel-out.")

(defun ar/bazel-compile ()
  "Invoke `'compile with `'completing-read a build rule in either current or parent directories."
  (interactive)
  (compile (format "%s %s" ar/bazel-compile-command
                   ;; Use for narrowing down rules to closest package.
                   ;; (ar/bazel-completing-read-build-rule)
                   (completing-read "build rule: " (ar/bazel-workspace-build-rules)))))

(defun ar/bazel-completing-read-build-rule ()
  "Find a build file in current or parent directories and `'completing-read a build rule."
  (let ((closest-build-file (ar/file-either-closest (if (equal major-mode 'dired-mode)
                                                        default-directory
                                                      (buffer-file-name)) "BUILD")))
    (assert closest-build-file nil "No BUILD found.")
    (format "%s:%s"
            (ar/bazel-qualified-package-path closest-build-file)
            (completing-read "build rule: " (ar/bazel-rule-names-in-build-file-path closest-build-file)))))

(defun ar/bazel-build-rule-names (str)
  "Return build rule names in STR."
  (mapcar (lambda (match)
            (nth 1 match))
          ;; match: name = "rulename"
          (s-match-strings-all "name *= *\"\\(.*\\)\""
                               str)))

(defun ar/bazel-rule-names-in-build-file-path (file-path)
  "Get rule names in build FILE-PATH."
  (ar/bazel-build-rule-names (with-temp-buffer
                               (insert-file-contents file-path)
                               (buffer-string))))

(defun ar/bazel-qualified-rule-names-in-build-file-path (file-path)
  "Get qualified rule names in build FILE-PATH."
  (let ((package-path (ar/bazel-qualified-package-path file-path)))
    (-map (lambda (rule-name)
            (format "%s:%s" package-path rule-name))
          (ar/bazel-rule-names-in-build-file-path file-path))))

(defun ar/bazel-qualified-package-path (path)
  "Convert PATH to workspace-qualified package: /some/path/workspace/package/BUILD => //package."
  (replace-regexp-in-string (ar/bazel-workspace-path) "//" (s-chop-suffix "/" (file-name-directory (expand-file-name path)))))

(defun ar/bazel-linked-dpath (name)
  "Append NAME to bazel root path and return it."
  (let ((dpath (concat (ar/bazel-workspace-path)
                       (format "%s-%s/"
                               ar/bazel-command
                               name))))
    (unless (f-exists-p dpath)
      (message "Path not found:\"%s\" (is `ar/bazel-command' = '%s' correct?)"
               dpath ar/bazel-command))
    dpath))

(defun ar/bazel-bin-dir ()
  "Bazel bin directory path."
  (ar/bazel-linked-dpath "bin"))

(defun ar/bazel-dired-bin-dir ()
  "Open WORKSPACE's bazel-bin directory."
  (interactive)
  (find-file (ar/bazel-bin-dir)))

(defun ar/bazel-out-dir ()
  "Bazel out directory path."
  (ar/bazel-linked-dpath "out"))

(defun ar/bazel-dired-out-dir ()
  "Open WORKSPACE's bazel-out directory."
  (interactive)
  (find-file (ar/bazel-out-dir)))

(defun ar/bazel-genfiles-dir ()
  "Bazel genfiles directory path."
  (ar/bazel-linked-dpath "genfiles"))

(defun ar/bazel-dired-genfiles-dir ()
  "Open WORKSPACE's bazel-genfiles directory."
  (interactive)
  (find-file (ar/bazel-genfiles-dir)))

(defun ar/bazel-workspace-path ()
  "Get bazel project path."
  (let ((workspace (locate-dominating-file default-directory "WORKSPACE")))
    (assert workspace nil "Not in a bazel project.")
    (expand-file-name workspace)))

(defun ar/bazel-workspace-build-files ()
  "Get all BUILD files in bazel project."
  (let ((dirs (cond
               ;; If projectile is found, try finding the whitelist of directories.
               ((and (fboundp 'projectile-parse-dirconfig-file)
                     (projectile-parse-dirconfig-file))
                (or (mapcar (lambda (path)
                              (concat (projectile-project-root)
                                      path))
                            ;; Whitelisted.
                            (car (projectile-parse-dirconfig-file)))
                    ;; .projectile found, but none whitelisted.
                    ;; default to root.
                    (list (projectile-project-root))))
               ;; Default to workspace dir otherwise.
               (t
                (list (ar/bazel-workspace-path))))))
    (message "Searching %s" dirs)
    (mapcar (lambda (path)
              (expand-file-name path))
            (apply 'process-lines (nconc (list "find")
                                         dirs
                                         (list "-name" "BUILD"))))))

(defun ar/bazel-workspace-build-rules (&optional fresh-read)
  "Get all workspace qualified rules.  If FRESH-READ, skip cache."
  (if fresh-read
      (let* ((counter 0)
             (build-files)
             (length))
        (setq build-files (ar/bazel-workspace-build-files))
        (setq length (length build-files))
        (-mapcat (lambda (build-file)
                   (setq counter (1+ counter))
                   (message "Reading (%d/%d) %s" counter length build-file)
                   (ar/bazel-qualified-rule-names-in-build-file-path build-file))
                 build-files))
    (ar/bazel--read-rules-cache)))

(defun ar/bazel-cache-build-rules ()
  "Cache absolute bazel build rules."
  (interactive)
  (ar/bazel--async-body-named "bazel-cache"
   (require 'subr-x)
   (require 'projectile)
   (require 'ar-bazel)
   (let ((start-time (current-time)))
     (ar/bazel--write-rules-cache
      (ar/bazel-workspace-build-rules t)
      (ar/bazel--rules-cache-fpath))
     (message \"Cached rules in %.2f seconds.\" (float-time
                                                 (time-subtract (current-time) start-time))))))

(defun ar/bazel-insert-rule ()
  "Insert a qualified build rule, with completion."
  (interactive)
  (insert (completing-read "build rule: " (ar/bazel-workspace-build-rules))))

(defun ar/bazel-print-rules ()
  "Print all BUILD rules in workspace."
  (interactive)
  (mapc (lambda (rule)
          (message rule))
        (ar/bazel-workspace-build-rules)))

(defun ar/bazel-jump-to-build-rule ()
  "Jump to the closest BUILD rule for current file."
  (interactive)
  (assert (executable-find ar/bazel-command) nil
          "\"%s\" not found. Did you set `ar/bazel-command'?" ar/bazel-command)
  (let* ((rule-pos)
         ;; path/to/root (from path/to/root/WORKSPACE)
         (workspace-dpath (or (locate-dominating-file default-directory "WORKSPACE")
                              (error "Not in a bazel project.")))
         ;; path/to/root/package/subpackage (from path/to/root/package/subpackage/BUILD)
         (package-dpath (or (locate-dominating-file default-directory "BUILD")
                            (error "No BUILD found.")))
         ;; source.swift
         (fname (file-name-nondirectory (or buffer-file-name
                                            (error "Not visiting a file"))))
         ;; //package/subpackage
         (package (replace-regexp-in-string workspace-dpath
                                            "//" (string-remove-suffix "/" package-dpath)))
         ;; //package/subpackage:source.swift
         (qualified-file (concat package ":" fname))
         ;; attr('srcs', //package/subpackage:source.swift, //package/subpackage:*)
         (query (format "attr('srcs', '%s', '%s:*')"
                        (shell-quote-argument qualified-file)
                        (shell-quote-argument package)))
         ;; //package/subpackage:MyRule
         (qualified-rule (or (seq-find (lambda (line)
                                         (string-prefix-p "//" line))
                                       (process-lines ar/bazel-command "query" query))
                             (error "No rule including %s" fname)))
         ;; MyRule
         (rule-name (nth 1 (split-string qualified-rule ":"))))
    (find-file (concat (file-name-as-directory package-dpath) "BUILD"))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char 0)
        ;; Find position of "MyRule".
        (setq rule-pos (re-search-forward (format "\"%s\"" rule-name)))))
    (goto-char rule-pos)
    (backward-sexp)))

(defun ar/bazel--rules-cache-fpath ()
  "Bazel rules cache path for current project."
  (concat (file-name-as-directory (ar/bazel-workspace-path))
          ".bazelrules"))

(defmacro ar/bazel--async-body-named (name &rest body)
  "Execute asynchronous BODY with NAME."
  (let ((body-string (replace-regexp-in-string "^(" "(progn " (format "%s" `,@body ) t t)))
    `(let ((calling-dpath default-directory)
           (fpath (concat (temporary-file-directory)
                          (format "%s.el" ,name))))
       (with-current-buffer (find-file-noselect fpath)
         (setq default-directory calling-dpath)
         (delete-region (point-min) (point-max))
         (insert (format "(setq default-directory \"%s\")\n" calling-dpath))
         (insert "(setq load-path '(\n")
         (mapc (lambda (path)
                 (insert (format "\"%s\"\n" path)))
               load-path)
         (insert "))\n")
         (insert ,body-string)
         (write-file fpath nil))
       (async-shell-command (concat (expand-file-name invocation-name invocation-directory)
                                    " --batch -Q -l " fpath)
                            (format "*%s*" ,name)))))

(defun ar/bazel--write-rules-cache (rules &optional fpath)
  "Write bazel absolute RULES at FPATH."
  (with-temp-buffer
    (prin1 rules (current-buffer))
    (write-file (or fpath (ar/bazel--rules-cache-fpath)) nil)))

(defun ar/bazel--read-rules-cache ()
  "Read history hash in HASH-FPATH."
  (if (not (f-exists? (ar/bazel--rules-cache-fpath)))
      (list)
    (with-temp-buffer
      (insert-file-contents (ar/bazel--rules-cache-fpath))
      (read (current-buffer)))))

(provide 'ar-bazel)

;;; ar-bazel.el ends here
