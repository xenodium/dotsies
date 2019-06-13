;;; company-projectile-cd.el --- Company projectile directory completion.

;;; Commentary:
;; Company projectile directory completion.


;;; Code:

(require 'cl-lib)
(require 'company)
(require 'dash)
(require 'f)
(require 's)
(require 'projectile)

(defun company-projectile-cd (command &optional arg &rest ignored)
  "Company shell completion for any projectile path."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-projectile-cd))
    (prefix
     (company-projectile-cd--prefix))
    (candidates
     (company-projectile-cd--candidates
      (company-projectile-cd--prefix)))
    (post-completion
     (company-projectile-cd--expand-inserted-path arg))))

(defun company-projectile-cd--prefix ()
  "Return prefix if in a project.  Nil otherwise to give a chance to other backends."
  (when (projectile-project-p default-directory)
    (-some (lambda (p)
             (let ((prefix (company-grab-symbol-cons p (length p))))
               (when (consp prefix)
                 prefix)))
           '("cd " "ls "))))

(defun company-projectile-cd--candidates (input)
  "Return candidates for given INPUT."
  (when (consp input)
    (let ((search-term (substring-no-properties
                        (car input) 0 (length (car input))))
          (prefix-found (cdr input)))
      (when (and prefix-found (projectile-project-p default-directory))
        (company-projectile-cd--projectile search-term)))))

(defun company-projectile-cd--projectile (search-term)
  (-filter (lambda (path)
             (string-match-p (regexp-quote
                              search-term)
                             path))
           (-snoc
            (projectile-current-project-dirs)
            ;; Throw project root in there also.
            (projectile-project-root))))

(defun company-projectile-cd--expand-inserted-path (path)
  "Replace relative PATH insertion with its relative equivalent."
  (delete-region (point) (- (point) (length path)))
  (insert (file-relative-name (company-projectile-cd--resolve path) default-directory)))

(defun company-projectile-cd--resolve (path)
  "Resolve PATH to either relative or absolute when needed (projectile)."
  (cond  ((f-exists-p (company-projectile-cd--quote path))
          (company-projectile-cd--quote path))
         ((and (projectile-project-p default-directory)
               (f-exists-p (company-projectile-cd--quote (concat (projectile-project-root)
                                                                 path))))
          (company-projectile-cd--quote (concat (projectile-project-root)
                                                path)))
         (t
          (error "could not resolve: %s" path))))

(defun company-projectile-cd--quote (path)
  "Wrap PATH with quotes if needed."
  (if (s-contains-p " " path)
      ;; Quote if spaces found.
      (format "\"%s\"" path)
    path))

(provide 'company-projectile-cd)

;;; company-projectile-cd.el ends here
