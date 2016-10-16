;;; ar-dired.el --- Dired support.

;;; Commentary:
;; Dired helpers.


;;; Code:

(defun ar/dired-split-directories (dir-a dir-b)
  (delete-other-windows)
  (split-window-right)
  (find-file dir-a)
  (find-file-other-window dir-b)
  (other-window 1))

(defun ar/dired-split-downloads-to-active ()
  "Split window with downloads to active directory."
  (interactive)
  (ar/dired-split-directories "~/Downloads"
                              "~/stuff/active"))

(defun ar/dired-split-desktop-to-active ()
  "Split window with downloads to active directory."
  (interactive)
  (ar/dired-split-directories "~/Desktop"
                              "~/stuff/active"))

(defun ar/dired-split-active-to-active ()
  "Split window with active to active directory."
  (interactive)
  (ar/dired-split-directories "~/stuff/active"
                              "~/stuff/active"))

(defun ar/dired-get-size ()
  "Show total for selected files."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-csh" files)
      (message
       "Total size: %s"
       (progn
         (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$")
         (match-string 1))))))

(provide 'ar-dired)

;;; ar-dired.el ends here
