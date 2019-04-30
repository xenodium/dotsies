;;; ar-counsel-find.el --- Counsel find support.

;;; Commentary:
;; Counsel find helpers.


;;; Code:

(require 'counsel)
(require 's)

(defvar ar/counsel-find--paths nil "Find path.")

(defvar ar/counsel-find--args nil "Find argumetns.")

(defvar ar/counsel-find--history nil "History for `ar/counsel-find'.")

(defun ar/counsel-find (arg)
  "Call the \"find\" shell command and fuzzy narrow using ivy. With ARG choose search path."
  (interactive "P")
  (when (or arg (not ar/counsel-find--paths))
    (setq ar/counsel-find--paths
          (list (read-directory-name "search in: " default-directory nil t))))
  (ar/counsel--find-in-paths ar/counsel-find--paths))

(defun ar/counsel--find-in-paths (paths)
  "Ivy narrow files found searching PATHS."
  (setq ar/counsel-find--paths paths)
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (kbd "C-c C-e") (lambda ()
                                       (interactive)
                                       (ivy-quit-and-run
                                         (ar/find-dired ar/counsel-find--paths
                                                        ar/counsel-find--args))))
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
  (format "find %s %s"
          (s-join " " ar/counsel-find--paths)
          ar/counsel-find--args))

(defun ar/counsel-find--function (pattern)
  "Find files ivy function matching PATTERN."
  (setq ar/counsel-find--args (format "-ipath '*%s*'"
                                      (s-replace-regexp "[ ]+" "*" pattern)))
  (message "ar/counsel-find--function: %s"
           (ar/counsel-find--command))
  (or
   (ivy-more-chars)
   (progn
     (counsel--async-command (ar/counsel-find--command))
     '("" "working..."))))

;; Like `find-dired' but taking a list of directories.
(defun ar/find-dired (dirs args)
  "Run `find' and go into Dired mode on a buffer of the output.
The command run (after changing into DIR) is essentially

    find DIRS \\( ARGS \\) -ls

except that the car of the variable `find-ls-option' specifies what to
use in place of \"-ls\" as the final argument."
  (let ((first-dir (nth 0 dirs))
        (dired-buffers dired-buffers))
    ;; Expand DIR ("" means default-directory), and make sure it has a
    ;; trailing slash.
    (setq first-dir (file-name-as-directory (expand-file-name first-dir)))
    ;; Check that it's really a directory.
    (or (file-directory-p first-dir)
	(error "find-dired needs a directory: %s" first-dir))
    (pop-to-buffer-same-window (get-buffer-create "*Find*"))

    ;; See if there's still a `find' running, and offer to kill
    ;; it first, if it is.
    (let ((find (get-buffer-process (current-buffer))))
      (when find
	(if (or (not (eq (process-status find) 'run))
		(yes-or-no-p
		 (format-message "A `find' process is running; kill it? ")))
	    (condition-case nil
		(progn
		  (interrupt-process find)
		  (sit-for 1)
		  (delete-process find))
	      (error nil))
	  (error "Cannot have two processes in `%s' at once" (buffer-name)))))

    (widen)
    (kill-all-local-variables)
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq default-directory first-dir
	  find-args args	      ; save for next interactive call
	  args (concat find-program " " (s-join " " dirs) " "
		       (if (string= args "")
			   ""
			 (concat
			  (shell-quote-argument "(")
			  " " args " "
			  (shell-quote-argument ")")
			  " "))
		       (if (string-match "\\`\\(.*\\) {} \\(\\\\;\\|\\+\\)\\'"
					 (car find-ls-option))
			   (format "%s %s %s"
				   (match-string 1 (car find-ls-option))
				   (shell-quote-argument "{}")
				   find-exec-terminator)
			 (car find-ls-option))))
    (message args)
    ;; Start the find process.
    (shell-command (concat args "&") (current-buffer))
    ;; The next statement will bomb in classic dired (no optional arg allowed)
    (dired-mode first-dir (cdr find-ls-option))
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map (current-local-map))
      (define-key map "\C-c\C-k" 'kill-find)
      (use-local-map map))
    (make-local-variable 'dired-sort-inhibit)
    (setq dired-sort-inhibit t)
    (set (make-local-variable 'revert-buffer-function)
	 `(lambda (ignore-auto noconfirm)
	    (find-dired ,dirs ,find-args)))
    ;; Set subdir-alist so that Tree Dired will work:
    (if (fboundp 'dired-simple-subdir-alist)
	;; will work even with nested dired format (dired-nstd.el,v 1.15
	;; and later)
	(dired-simple-subdir-alist)
      ;; else we have an ancient tree dired (or classic dired, where
      ;; this does no harm)
      (set (make-local-variable 'dired-subdir-alist)
	   (list (cons default-directory (point-min-marker)))))
    (set (make-local-variable 'dired-subdir-switches) find-ls-subdir-switches)
    (setq buffer-read-only nil)
    ;; dired header (title), showing find command.
    (insert "  " args ":\n")
    ;; Make second line a ``find'' line in analogy to the ``total'' or
    ;; ``wildcard'' line.
    (let ((point (point)))
      (insert "  " args "\n")
      (dired-insert-set-properties point (point)))
    (setq buffer-read-only t)
    (let ((proc (get-buffer-process (current-buffer))))
      (set-process-filter proc (function find-dired-filter))
      (set-process-sentinel proc (function find-dired-sentinel))
      ;; Initialize the process marker; it is used by the filter.
      (move-marker (process-mark proc) (point) (current-buffer)))
    (setq mode-line-process '(":%s"))))

(provide 'ar-counsel-find)

;;; ar-counsel-find.el ends here
