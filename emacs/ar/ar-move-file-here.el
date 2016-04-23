;;; ar-move-file-here.el --- Move here support.

;;; Commentary:
;; Move here helpers.


;;; Code:

(require 'dash)

;; start directory
(defvar ar/move-file-here-start-dir (expand-file-name "~/Downloads"))

;; Based on http://pragmaticemacs.com/emacs/quickly-move-a-file-to-the-current-directory
(defun ar/move-file-here ()
  "Move file from somewhere else to here.
The file is taken from a start directory set by `ar/move-file-here-start-dir' and moved to the current directory if invoked in dired, or else the directory containing current buffer. The user is presented with a list of files in the start directory, from which to select the file to move, sorted by most recent first."
  (interactive)
  (let (file-list target-dir file-list-sorted start-file start-file-full)
    ;; clean directories from list but keep times
    (setq file-list
          (-remove (lambda (x) (nth 1 x))
                   (directory-files-and-attributes ar/move-file-here-start-dir)))

    ;; get target directory
    ;; http://ergoemacs.org/emacs/emacs_copy_file_path.html
    (setq target-dir
          (if (equal major-mode 'dired-mode)
              (expand-file-name default-directory)
            (if (null (buffer-file-name))
                (user-error "ERROR: current buffer is not associated with a file.")
              (file-name-directory (buffer-file-name)))))

    ;; sort list by most recent
    ;;http://stackoverflow.com/questions/26514437/emacs-sort-list-of-directories-files-by-modification-date
    (setq file-list-sorted
          (mapcar #'car
                  (sort file-list
                        #'(lambda (x y) (time-less-p (nth 6 y) (nth 6 x))))))

    (setq start-file (completing-read (format "Move selected file to %s: " target-dir)
                                      file-list-sorted))

    ;; add full path to start file and end-file
    (setq start-file-full
          (expand-file-name start-file ar/move-file-here-start-dir))
    (setq end-file
          (expand-file-name (file-name-nondirectory start-file) target-dir))
    (rename-file start-file-full end-file)
    (message "moved %s to %s" start-file-full end-file)))

;;(read-file-name "Move selected file to " "~/Downloads/" nil t)

(provide 'ar-move-file-here)

;;; ar-move-file-here.el ends here
