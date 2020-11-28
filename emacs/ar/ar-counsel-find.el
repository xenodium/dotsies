;;; ar-counsel-find.el --- Counsel find support. -*- lexical-binding: t; -*-

;;; Commentary:
;; Counsel find with option to drop to dired.


;;; Code:

(require 'counsel)
(require 'find-dired)
(require 's)
(require 'f)
(require 'cl-lib)

(defvar ar/counsel-find--paths nil
  "Find in paths.")

(defvar ar/counsel-find--args nil
  "Find arguments.")

(defvar ar/counsel-find--history nil "History for `ar/counsel-find'.")

(defun ar/counsel-find (arg)
  "Call the \"find\" shell command and fuzzy narrow using ivy.

With ARG choose search path upfront.

C-c C-e invokes `find-dired' with underlying find query.
C-x C-f changes the search path."
  (interactive "P")
  (when (or arg (not ar/counsel-find--paths))
    (setq ar/counsel-find--paths
          (list (directory-file-name
                 (read-directory-name "search in: " default-directory nil t)))))
  (ar/counsel-find--in-paths ar/counsel-find--paths))

(defun ar/counsel-find--in-paths (paths)
  "Ivy narrow files found searching PATHS."
  (setq ar/counsel-find--paths (mapcar #'directory-file-name paths))
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (kbd "C-c C-e") (lambda ()
                                       (interactive)
                                       (ivy-quit-and-run
                                         (ar/counsel-find--dired ar/counsel-find--paths
                                                                 ar/counsel-find--args))))
    (define-key kmap (kbd "C-x C-f") (lambda ()
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
              :action (lambda (fpath)
                        (with-ivy-window
                          (when fpath
                            (find-file (f-join (ar/counsel-find--common-parent)
                                               fpath)))))
              :unwind #'counsel-delete-process
              :keymap kmap
              :caller 'ar/counsel-find)))

(defun ar/counsel-find--common-parent ()
  (cl-assert (> (length ar/counsel-find--paths)
             0))
  (f-common-parent ar/counsel-find--paths))

(defun ar/counsel-find--command ()
  (format "find %s %s "
          (s-join " " ar/counsel-find--paths)
          ar/counsel-find--args))

;; Mostly copied from counsel.el.
(defun ar/counsel-find--async-sentinel (process _msg)
  "Sentinel function for an asynchronous counsel PROCESS.
Mostly copied from counsel.el, but makes find command results relative to common parent."
  (when (eq (process-status process) 'exit)
    (if (zerop (process-exit-status process))
        (progn
          (ivy--set-candidates
           (ivy--sort-maybe
            (with-current-buffer (process-buffer process)
              (mapcar (lambda (path)
                        (f-relative path
                                    (ar/counsel-find--common-parent)))
                      (split-string (buffer-string) "\n" t)))))
          (setq counsel-grep-last-line nil)
          (when counsel--async-start
            (setq counsel--async-duration
                  (time-to-seconds (time-since counsel--async-start))))
          (let ((re (ivy-re-to-str (funcall ivy--regex-function ivy-text))))
            (if ivy--old-cands
                (if (eq (ivy-alist-setting ivy-index-functions-alist) 'ivy-recompute-index-zero)
                    (ivy-set-index 0)
                  (ivy--recompute-index ivy-text re ivy--all-candidates))
              (unless (ivy-set-index
                       (ivy--preselect-index
                        (ivy-state-preselect ivy-last)
                        ivy--all-candidates))
                (ivy--recompute-index ivy-text re ivy--all-candidates))))
          (setq ivy--old-cands ivy--all-candidates)
          (if ivy--all-candidates
              (ivy--exhibit)
            (ivy--insert-minibuffer "")))
      (setq ivy--all-candidates
            (let ((status (process-exit-status process))
                  (plist (plist-get counsel--async-exit-code-plist
                                    (ivy-state-caller ivy-last))))
              (list (or (plist-get plist status)
                        (format "error code %d" status)))))
      (setq ivy--old-cands ivy--all-candidates)
      (ivy--exhibit))))

;; Mostly copied from counsel.el.
(defun ar/counsel-find--async-filter (process str)
  "Receive from PROCESS the output STR.
Mostly copied from counsel.el, but makes find command results relative to common parent."
  (with-current-buffer (process-buffer process)
    (insert str))
  (when (time-less-p (list 0 0 counsel-async-filter-update-time)
                     (time-since counsel--async-time))
    (let (numlines)
      (with-current-buffer (process-buffer process)
        (setq numlines (count-lines (point-min) (point-max)))
        (ivy--set-candidates
         (mapcar (lambda (path)
                   (f-relative path (ar/counsel-find--common-parent)))
                 (split-string (buffer-string) "\n" t))))
      (let ((ivy--prompt (format "%d++ %s" numlines (ivy-state-prompt ivy-last))))
        (ivy--insert-minibuffer (ivy--format ivy--all-candidates)))
      (setq counsel--async-time (current-time)))))

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
     (counsel--async-command (ar/counsel-find--command)
                             #'ar/counsel-find--async-sentinel
                             #'ar/counsel-find--async-filter)
     '("" "working..."))))

;; Like `find-dired' but taking a list of directories.
;; Mostly copied from find-dired.el.
(defun ar/counsel-find--dired (dpaths args)
  "Run `find' and go into Dired mode on a buffer of the output.
The command run is essentially: find DPATHS \\( ARGS \\) -ls"
  (cl-assert (> (length dpaths) 0) nil "Needs at least one directory path to find from")
  (let ((dired-buffers dired-buffers)
        (command))
    (pop-to-buffer-same-window (get-buffer-create "*Find*"))
    (setq default-directory (file-name-as-directory
                             (expand-file-name (if (= (length dpaths) 1)
                                                   (nth 0 dpaths)
                                                 (f-common-parent dpaths)))))
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
    (dired-mode default-directory "")
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
    (insert (format "  find in %s:\n" (s-join " "
                                              (mapcar (lambda (path)
                                                        ;; Relative, but expand "."
                                                        (file-name-nondirectory
                                                         (expand-file-name
                                                          (directory-file-name
                                                           (f-relative path default-directory)))))
                                                      dpaths))))

    ;; Dired second line is the `find' command.
    (let ((point (point)))
      (insert "  " command "\n")
      (dired-insert-set-properties point (point)))

    (setq buffer-read-only t)

    (let ((proc (get-buffer-process (current-buffer))))
      (set-process-filter proc #'ar/counsel-find--dired-filter)
      (set-process-sentinel proc #'find-dired-sentinel)
      (move-marker (process-mark proc) (point) (current-buffer)))
    (setq mode-line-process '(":%s"))))

(defun ar/counsel-find--dired-filter (proc string)
  "Apply `find-dired-sentinel' to PROC and STRING.
Also strip all paths of `default-directory' (make them relative)."
  (find-dired-filter proc string)
  (when-let ((buf (process-buffer proc))
             (inhibit-read-only t))
    (with-current-buffer buf
      (save-excursion
	(save-restriction
	  (widen)
          (goto-line 2)
          (goto-char (line-end-position))
          (while (search-forward (file-name-as-directory
                                  default-directory) nil t)
            (replace-match "")))))))

(provide 'ar-counsel-find)

;;; ar-counsel-find.el ends here
