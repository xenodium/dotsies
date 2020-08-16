;;; -*- lexical-binding: t; -*-

(defun ar/yank-line-below (arg)
  "Yank to line below. With ARG, repeat."
  (interactive "p")
  (let ((lines))
    (dotimes (_i arg)
      (setq lines
            (concat lines
                    (current-kill 0)
                    "\n")))
    (setq lines (string-remove-suffix "\n" lines))
    (save-excursion
      (end-of-line)
      (newline)
      (insert lines))
    (next-line)))

(defun adviced:read-shell-command (orig-fun &rest r)
  "Advice around `read-shell-command' to replace $f with buffer file name."
  (let ((command (apply orig-fun r)))
    (if (string-match-p "\\$f" command)
        (replace-regexp-in-string "\\$f"
                                  (or (buffer-file-name)
                                      (user-error "No file file visited to replace $f"))
                                  command)
      command)))

(advice-add #'read-shell-command
            :around
            #'adviced:read-shell-command)

;; From https://github.com/daschwa/emacs.d
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single
line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-end-position)))))
