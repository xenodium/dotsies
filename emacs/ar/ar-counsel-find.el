;;; ar-counsel-find.el --- Counsel find support.

;;; Commentary:
;; Counsel find helpers.


;;; Code:

(require 'counsel)
(require 's)

(defvar ar/counsel-find--paths nil "Find path.")

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
            :caller 'ar/counsel-find))

(defun ar/counsel-find--function (pattern)
  "Find files ivy function matching PATTERN."
  (let* ((command (format "find %s -ipath '*%s*'"
                          (s-join " " ar/counsel-find--paths)
                          (s-replace-regexp "[ ]+" "*" pattern))))
    (or
     (ivy-more-chars)
     (progn
       (message "ar/counsel-find--function: %s" command)
       (counsel--async-command command)
       '("" "working...")))))


(provide 'ar-counsel-find)

;;; ar-counsel-find.el ends here
