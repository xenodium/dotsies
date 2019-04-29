;;; ar-counsel-find.el --- Counsel find support.

;;; Commentary:
;; Counsel find helpers.


;;; Code:

(require 'counsel)
(require 's)

(defvar ar/counsel-find--paths nil "Find path.")

(defvar ar/counsel-find--history nil "History for `ar/counsel-find'.")

(defvar ar/counsel-find--last-command nil "Last find shell command.")

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
                                         (message "Implement me"))))
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

(defun ar/counsel-find--function (pattern)
  "Find files ivy function matching PATTERN."
  (setq ar/counsel-find--last-command
        (format "find %s -ipath '*%s*'"
                (s-join " " ar/counsel-find--paths)
                (s-replace-regexp "[ ]+" "*" pattern)))
  (message "ar/counsel-find--function: %s"
           ar/counsel-find--last-command)
  (or
   (ivy-more-chars)
   (progn
     (counsel--async-command ar/counsel-find--last-command)
     '("" "working..."))))

(provide 'ar-counsel-find)

;;; ar-counsel-find.el ends here
