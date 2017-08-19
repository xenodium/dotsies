;;; company-projectile-cd.el --- Company projectile directory completion.

;;; Commentary:
;; Company projectile directory completion.


;;; Code:

(require 'cl-lib)
(require 'company)
(require 'projectile)

(defvar company-projectile-cd-prefix "cd ")

(defun company-projectile-cd (command &optional arg &rest ignored)
  "Company shell completion for any projectile path."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-projectile-cd))
    (prefix
     (company-grab-symbol-cons company-projectile-cd-prefix
                               (length company-projectile-cd-prefix)))
    (candidates
     (company-projectile-cd--candidates (company-grab-symbol-cons company-projectile-cd-prefix
                                                                  (length company-projectile-cd-prefix))))
    (post-completion
     (company-projectile-cd--expand-inserted-path arg))))

(defun company-projectile--filter (paths needle)
  "Keep PATHS matching NEEDLE."
  (delq nil
        (mapcar (lambda (path)
                  (when (string-match-p needle path)
                    path))
                paths)))

(defun company-projectile-cd--candidates (input)
  "Return candidates for given INPUT."
  (if (consp input)
      (let ((search-term (car input))
            (trigger-found (cdr input)))
        (when trigger-found
          (company-projectile--filter (projectile-current-project-dirs)
                                      (substring-no-properties search-term 0 (length search-term)))))))

(defun company-projectile-cd--expand-inserted-path (path)
  "Delete relative PATH insertion with its absolute equivalent."
  (delete-region (point) (- (point) (length path)))
  (insert (concat (projectile-project-root) path)))

(provide 'company-projectile-cd)

;;; company-projectile-cd.el ends here
