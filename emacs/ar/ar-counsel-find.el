;;; ar-counsel-find.el --- Counsel find support.

;;; Commentary:
;; Counsel find helpers.


;;; Code:

(require 'counsel)
(require 'find-dired)
(require 's)
(require 'f)

(defvar ar/counsel-find--paths nil "Find path.")

(defvar ar/counsel-find--args nil "Find argumetns.")

(defvar ar/counsel-find--history nil "History for `ar/counsel-find'.")

(defun ar/counsel-find (arg)
  "Call the \"find\" shell command and fuzzy narrow using ivy. With ARG choose search path."
  (interactive "P")
  (when (or arg (not ar/counsel-find--paths))
    (setq ar/counsel-find--paths
          (list (s-chop-suffix "/"
                               (read-directory-name "search in: " default-directory nil t)))))
  (ar/counsel-find--in-paths ar/counsel-find--paths))

(defun ar/counsel-find--in-paths (paths)
  "Ivy narrow files found searching PATHS."
  (setq ar/counsel-find--paths (mapcar (lambda (path)
                                         (s-chop-suffix "/" path))
                                       paths))
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (kbd "C-c C-e") (lambda ()
                                       (interactive)
                                       (ivy-quit-and-run
                                         (ar/counsel-find--dired ar/counsel-find--paths
                                                                 ar/counsel-find--args))))
    (define-key kmap (kbd "C-c C-r") (lambda ()
                                       (interactive)
                                       (ivy-quit-and-run
                                         (ar/counsel-find t))))
    (ivy-read "find: "
              #'ar/counsel-find--function
              :initial-input (when (use-region-p)
                               (buffer-substring-no-properties (region-beginning)
                                                               (region-end)))
              :dynamic-collection t
              :history 'ar/counsel-find--history
              :action (lambda (file)
                        (with-ivy-window
                          (when file
                            (find-file file))))
              :unwind #'counsel-delete-process
              :keymap kmap
              :caller 'ar/counsel-find)))

(defun ar/counsel-find--command ()
  (format "find %s %s "
          (s-join " " ar/counsel-find--paths)
          ar/counsel-find--args))

(defun ar/counsel-find--function (pattern)
  "Find files ivy function matching PATTERN."
  ;; -follow to follow symlinks.
  (setq ar/counsel-find--args (format "-ipath '*%s*' -follow"
                                      (s-replace-regexp "[ ]+" "*" pattern)))
  (message "ar/counsel-find--function: %s"
           (ar/counsel-find--command))
  (or
   (ivy-more-chars)
   (progn
     (counsel--async-command (ar/counsel-find--command))
     '("" "working..."))))

;; Like `find-dired' but taking a list of directories.
(defun ar/counsel-find--dired (dpaths args)
  "Run `find' and go into Dired mode on a buffer of the output.
The command run is essentially: find DPATHS \\( ARGS \\) -ls"
  (let ((parent-dpath (f-common-parent dpaths))
        (dired-buffers dired-buffers)
        (command))
    ;; Expand and ensure it has a trailing slash.
    (setq parent-dpath (file-name-as-directory (expand-file-name parent-dpath)))
    (assert (file-directory-p parent-dpath) nil
            "find-dired needs a directory: %s" parent-dpath)
    (setq default-directory parent-dpath)
    (pop-to-buffer-same-window (get-buffer-create "*Find*"))
    ;; Kill exsiting `find' process.
    (when (and (get-buffer-process (current-buffer))
               (eq (process-status (get-buffer-process (current-buffer)))
                   'run))
      (progn
        (interrupt-process find)
        (sit-for 1)
        (delete-process find)))
    (widen)
    (kill-all-local-variables)
    (setq command (format "%s %s %s %s %s %s"
                          find-program
                          (s-join " " dpaths)
                          (shell-quote-argument "(")
                          args
                          (shell-quote-argument ")")
                          (car find-ls-option)))
    (message command)
    (shell-command (concat command "&") (current-buffer))
    (dired-mode parent-dpath "")
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map (current-local-map))
      (define-key map "\C-c\C-k" 'kill-find)
      (use-local-map map))
    (setq-local dired-sort-inhibit t)
    (setq-local revert-buffer-function
                (lambda (_1 _2) `(lambda (ignore-auto noconfirm)
                                   (ar/counsel-find--dired ,dpaths ,args))))
    (setq-local dired-subdir-alist
                (list (cons default-directory (point-min-marker))))
    (setq-local dired-subdir-switches find-ls-subdir-switches)

    (setq buffer-read-only nil)

    (erase-buffer)

    ;; Dired header.
    (insert (format "  find in %s:\n" (s-join " " dpaths)))

    ;; Dired second line is the `find' command.
    (let ((point (point)))
      (insert "  " command "\n")
      (dired-insert-set-properties point (point)))

    (setq buffer-read-only t)

    (let ((proc (get-buffer-process (current-buffer))))
      (set-process-filter proc #'find-dired-filter)
      (set-process-sentinel proc #'find-dired-sentinel)
      (move-marker (process-mark proc) (point) (current-buffer)))
    (setq mode-line-process '(":%s"))))

(provide 'ar-counsel-find)

;;; ar-counsel-find.el ends here
