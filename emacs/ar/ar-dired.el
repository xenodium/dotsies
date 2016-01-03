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

(provide 'ar-dired)

;;; ar-dired.el ends here
