;;; ar-git.el --- Git support.

;;; Commentary:
;; Git helpers.


;;; Code:

(defun ar/git-pull-repo-at-path (path)
  "Pull repository at PATH."
  (ar/file-with-current-file path (magit-pull)))

(defun ar/git-unpushed-changes-p ()
  "Check if unpushed changes are present in git master."
  (not (string-empty-p (shell-command-to-string "git log --oneline origin/master..master"))))

(defun ar/git-pending-repo-at-path-p (path)
  "Check if pending changes in repository at PATH."
  (or (ar/file-with-current-file path (magit-anything-modified-p))
      (ar/git-unpushed-changes-p)))

(defun ar/git-check-frequent-repos-pending ()
  "Check for for uncommited changes in frequent repos."
  (interactive)
  (cond
   ((ar/git-pending-repo-at-path-p "~/stuff/active/dots")
    (magit-status "~/stuff/active/dots"))
   ((ar/git-pending-repo-at-path-p "~/stuff/active/blog")
    (magit-status "~/stuff/active/blog"))
   ((ar/git-pending-repo-at-path-p "~/stuff/active/non-public")
    (magit-status "~/stuff/active/non-public"))
   (t (message "Life is good!"))))

(defun ar/git-pull-frequent-repos ()
  "Pull all frequent repositories."
  (interactive)
  (ar/git-pull-repo-at-path "~/stuff/active/dots")
  (ar/git-pull-repo-at-path "~/stuff/active/blog")
  (ar/git-pull-repo-at-path "~/stuff/active/non-public"))

(provide 'ar-git)

;;; ar-git.el ends here
