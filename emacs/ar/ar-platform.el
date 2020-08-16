;;; ar-platform.el --- Platform support.

;;; Commentary:
;; Setup helpers based on platform.


;;; Code:

(defun ar/platform-new-browser-tab ()
  "Open a new browser tab in the default browser."
  (interactive)
  (let ((command (cond
                  ((string-equal system-type "darwin")
                   "open http://google.com")
                  ((string-equal system-type "gnu/linux")
                   "google-chrome http://google.com")
                  (nil))))
    (unless command
      (error "Unrecognized platform"))
    (shell-command command)))

(defun ar/platform-open-in-external-app-function ()
  "Return a function to open FPATH externally."
  (cond
   ((string-equal system-type "darwin")
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

(defun ar/platform-open-file-at-point ()
  "Open the file path at point.
If there is text selection, uses the text selection for path.
If the path starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
Path may have a trailing “:‹n›” that indicates line number.
If so, jump to that line number.
If path does not have a file extention, automatically try with “.el” for elisp
files.
This command is similar to `find-file-at-point' but without prompting for
confirmation.

URL `http://ergoemacs.org/emacs/emacs_open_file_path_fast.html'"
  (interactive)
  (let ((ξpath (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (let (p0 p1 p2)
                   (validate-setq p0 (point))
                   ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
                   (skip-chars-backward "^  \"\t\n`'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\`")
                   (validate-setq p1 (point))
                   (goto-char p0)
                   (skip-chars-forward "^  \"\t\n`'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\'")
                   (validate-setq p2 (point))
                   (goto-char p0)
                   (buffer-substring-no-properties p1 p2)))))
    (if (string-match-p "\\`https?://" ξpath)
        (browse-url ξpath)
      (progn ; not starting “http://”
        (if (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\'" ξpath)
            (progn
              (let (
                    (ξfpath (match-string 1 ξpath))
                    (ξline-num (string-to-number (match-string 2 ξpath))))
                (if (file-exists-p ξfpath)
                    (progn
                      (find-file ξfpath)
                      (goto-char 1)
                      (forward-line (1- ξline-num)))
                  (progn
                    (when (y-or-n-p (format "File doesn't exist: %s.  Create? " ξfpath))
                      (find-file ξfpath))))))
          (progn
            (if (file-exists-p ξpath)
                (find-file ξpath)
              (if (file-exists-p (concat ξpath ".el"))
                  (find-file (concat ξpath ".el"))
                (when (y-or-n-p (format "File doesn't exist: %s.  Create? " ξpath))
                  (find-file ξpath ))))))))))

(provide 'ar-platform)

;;; ar-platform.el ends here
