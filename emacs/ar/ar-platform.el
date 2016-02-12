;;; ar-platform.el --- Platform support.

;;; Commentary:
;; Setup helpers based on platform.


;;; Code:

(require 'ar-osx)
(require 'ar-linux)

(defun ar/platform-new-browser-tab ()
  "Open a new browser tab in the default browser."
  (interactive)
  (let ((command (cond
                  ((ar/osx-p)
                   "open http://google.com")
                  ((ar/platform-gnu-linux-p)
                   "google-chrome http://google.com")
                  (nil))))
    (unless command
      (error "Unrecognized platform"))
    (shell-command command)))

(defun ar/platform-open-in-external-app-function ()
  "Return a function to open FPATH externally."
  (cond
   ((ar/osx-p)
    (lambda (fPath)
      (shell-command (format "open \"%s\"" fPath))))
   ((ar/linux-p)
    (lambda (fPath)
      (let ((process-connection-type nil))
        (start-process "" nil "xdg-open" fPath))))))

;; Based on http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html
(defun ar/platform-open-in-external-app ()
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.

Version 2015-01-26
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'"
  (interactive)
  (if (eq major-mode 'eww-mode)
      (eww-browse-with-external-browser eww-current-url)
    (let* ((ξfile-list
            (cond ((eq major-mode 'dired-mode)
                   (dired-get-marked-files))
                  (t
                   (list (buffer-file-name)))))
           (ξdo-it-p (if (<= (length ξfile-list) 5)
                         t
                       (y-or-n-p "Open more than 5 files? "))))
      (when ξdo-it-p
        (mapc (ar/platform-open-in-external-app-function) ξfile-list)))))

(provide 'ar-platform)

;;; ar-platform.el ends here
