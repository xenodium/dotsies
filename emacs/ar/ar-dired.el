;;; ar-dired.el --- Dired support.

;;; Commentary:
;; Dired helpers.


;;; Code:

(require 'f)

(defun ar/dired-split-directories (dir-a dir-b)
  (delete-other-windows)
  (split-window-right)
  (find-file dir-a)
  (find-file-other-window dir-b)
  (other-window 1))

(defun ar/dired-current-directory ()
  "Get the current directory from buffer, including `dired-mode'."
  (if (buffer-file-name)
      (if (eq major-mode 'dired-mode)
          (buffer-file-name)
        (file-name-directory (buffer-file-name)))
    (expand-file-name default-directory)))

(defun ar/dired-split-downloads-to-current ()
  "Split window with downloads to active directory."
  (interactive)
  (ar/dired-split-directories "~/Downloads"
                              (ar/dired-current-directory)))

(defun ar/dired-split-desktop-to-current ()
  "Split window with downloads to active directory."
  (interactive)
  (ar/dired-split-directories "~/Desktop"
                              (ar/dired-current-directory)))

(defun ar/dired-split-active-to-current ()
  "Split window with active to active directory."
  (interactive)
  (ar/dired-split-directories "~/stuff/active"
                              (ar/dired-current-directory)))

(defun ar/dired--choose-from (path)
  "Choose a file from PATH."
  (completing-read "Choose: " (f-files path)))

(defun ar/dired--copy-or-move (fun path)
  "Copy/move using FUN to file PATH."
  (let* ((from-path (ar/dired--choose-from (f-expand path)))
         (to-path (f-join (ar/dired-current-directory)
                          (f-filename from-path))))
    (funcall fun from-path to-path)
    (when (not (eq major-mode 'dired-mode))
      (find-file (ar/dired-current-directory)))
    (dired-goto-file to-path)))

(defun ar/dired--find-file-from (path)
  "Find file choosing from PATH."
  (let ((file-path (ar/dired--choose-from (f-expand path))))
    (find-file file-path)))

(defun ar/dired-find-file-from-downloads ()
  "Move a file from Downloads directory to current."
  (interactive)
  (ar/dired--find-file-from "~/Downloads"))

(defun ar/dired-move-file-from-downloads ()
  "Move a file from Downloads directory to current."
  (interactive)
  (ar/dired--copy-or-move 'f-move "~/Downloads"))

(defun ar/dired-copy-file-from-downloads ()
  "Copy a file from Downloads directory to current."
  (interactive)
  (ar/dired--copy-or-move 'f-copy "~/Downloads"))

(defun ar/dired-find-file-from-desktop ()
  "Move a file from Desktop directory to current."
  (interactive)
  (ar/dired--find-file-from "~/Desktop"))

(defun ar/dired-move-file-from-desktop ()
  "Move a file from Desktop directory to current."
  (interactive)
  (ar/dired--copy-or-move 'f-move "~/Desktop"))

(defun ar/dired-copy-file-from-desktop ()
  "Copy a file from Desktop directory to current."
  (interactive)
  (ar/dired--copy-or-move 'f-copy "~/Desktop"))

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
