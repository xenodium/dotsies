;;; ar-bazel.el --- Bazel support. -*- lexical-binding: t; -*-

;;; Commentary:
;; Bazel helpers.


;;; Code:

(require 'seq)
(require 'cl-lib)

(defvar ar/bazel-command "bazel"
  "With a \"bazel\" prefix, we'd get linked directories like:
bazel-bin, bazel-genfiles, and bazel-out.")

(defvar ar/bazel-compile-subcommand "build" "Compile subcommand.")

(defun ar/bazel-compile ()
  "Invoke `'compile with `'completing-read a build rule in either current or parent directories."
  (interactive)
  (compile (format "%s %s" ar/bazel-command ar/bazel-compile-subcommand
                   ;; Use for narrowing down rules to closest package.
                   ;; (ar/bazel-completing-read-build-rule)
                   (completing-read "build rule: " (ar/bazel-workspace-build-rules)))))

(defun ar/bazel-completing-read-build-rule ()
  "Find a build file in current or parent directories and `'completing-read a build rule."
  (let ((closest-build-file (concat (locate-dominating-file (if (equal major-mode 'dired-mode)
                                                                default-directory
                                                              (buffer-file-name)) "BUILD")
                                    "BUILD")))
    (cl-assert closest-build-file nil "No BUILD found.")
    (format "%s:%s"
            (ar/bazel-qualified-package-path closest-build-file)
            (completing-read "build rule: " (ar/bazel-rule-names-in-build-file-path closest-build-file)))))

(defun ar/bazel-rule-names-in-build-file-path (file-path)
  "Get rule names in build FILE-PATH."
  (let ((rule-names))
    (with-temp-buffer
      (insert-file-contents file-path)
      (goto-char 1)
      ;; match all: name = "rulename"
      (while (search-forward-regexp "name *= *\"\\(.*\\)\"" nil t 1)
        (push (match-string-no-properties 1) rule-names)))
    rule-names))

(defun ar/bazel-qualified-rule-names-in-build-file-path (file-path)
  "Get qualified rule names in build FILE-PATH."
  (let ((package-path (ar/bazel-qualified-package-path file-path)))
    (mapcar (lambda (rule-name)
              (format "%s:%s" package-path rule-name))
            (ar/bazel-rule-names-in-build-file-path file-path))))

(defun ar/bazel-qualified-package-path (path)
  "Convert PATH to workspace-qualified package: /some/path/workspace/package/BUILD => //package."
  (concat "//" (directory-file-name (or (file-name-directory (file-relative-name path
                                                                                 (ar/bazel-workspace-path)))
                                        ""))))

(defun ar/bazel-linked-dpath (name)
  "Append NAME to bazel root path and return it."
  (let ((dpath (concat (ar/bazel-workspace-path)
                       (format "%s-%s/"
                               ar/bazel-command
                               name))))
    (unless (file-exists-p dpath)
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
    (cl-assert workspace nil "Not in a bazel project.")
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
             (build-files (ar/bazel-workspace-build-files))
             (length (length build-files)))
        (seq-mapcat (lambda (build-file)
                      (setq counter (1+ counter))
                      (message "(%d/%d) %s" counter length
                               (ar/bazel-qualified-package-path build-file))
                      (ar/bazel-qualified-rule-names-in-build-file-path build-file))
                    build-files))
    (ar/bazel--read-rules-cache)))

(defun ar/bazel-cache-build-rules ()
  "Cache absolute bazel build rules."
  (interactive)
  (let* ((proc-name "bazel-cache")
         (buffer (get-buffer-create (format "*%s*" proc-name)))
         (emacs-bin (file-truename (expand-file-name invocation-name
                                                     invocation-directory))))
    (message "%s started" proc-name)
    (with-current-buffer buffer
      (erase-buffer))
    (set-process-sentinel
     (start-process proc-name buffer emacs-bin
                    "--quick" "--batch" "--eval"
                    (prin1-to-string
                     `(progn
                        (interactive)
                        (setq load-path ',load-path)
                        (require 'subr-x)
                        (require 'projectile)
                        (require 'ar-bazel)
                        (let ((start-time (current-time)))
                          (ar/bazel--write-rules-cache
                           (ar/bazel-workspace-build-rules t)
                           (ar/bazel--rules-cache-fpath))
                          (message "Cached bazel rules in %.2f seconds."
                                   (float-time
                                    (time-subtract (current-time) start-time)))))))
     (lambda (process state)
       (if (= (process-exit-status process) 0)
           (message "%s finished" proc-name)
         (message "%s failed, see *%s*" proc-name proc-name))))))

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
  (let* (;; path/to/root/package/subpackage (from path/to/root/package/subpackage/BUILD)
         (package-dpath (expand-file-name
                         (or (locate-dominating-file default-directory "BUILD")
                             (error "No BUILD found."))))
         ;; source.swift
         (fname (replace-regexp-in-string package-dpath
                                          "" (expand-file-name
                                              (or buffer-file-name
                                                  (error "Not visiting a file")))))
         ;; search for "source.swift"
         (needle (format "\"%s\"" fname)))
    (find-file (concat (file-name-as-directory package-dpath) "BUILD"))
    (goto-char (point-min))
    ;; search for "source.swift"
    (search-forward needle)
    (backward-char (length needle))))

(defun ar/bazel--rules-cache-fpath ()
  "Bazel rules cache path for current project."
  (concat (file-name-as-directory (ar/bazel-workspace-path))
          ".bazelrules"))

(defun ar/bazel--write-rules-cache (rules &optional fpath)
  "Write bazel absolute RULES at FPATH."
  (with-temp-buffer
    (prin1 rules (current-buffer))
    (write-file (or fpath (ar/bazel--rules-cache-fpath)) nil)))

(defun ar/bazel--read-rules-cache ()
  "Read history hash in HASH-FPATH."
  (if (not (file-exists-p (ar/bazel--rules-cache-fpath)))
      (list)
    (with-temp-buffer
      (insert-file-contents (ar/bazel--rules-cache-fpath))
      (read (current-buffer)))))

(provide 'ar-bazel)

;;; ar-bazel.el ends here
