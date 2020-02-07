;;; company-cd.el --- Company `cd' completion.  -*- lexical-binding: t; -*-

;;; Commentary:
;; Company `cd' completion.


;;; Code:

(require 'cl-lib)
(require 'company)
(require 'dash)
(require 'f)
(require 's)

(defcustom company-cd-search-timeout 0.5
  "Amount of time in seconds to wait before cancelling the depth search."
  :type 'number)

(defun company-cd (command &optional arg &rest ignored)
  "Company shell completion for `cd' shell or terminal prefixes."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-cd))
    (prefix
     (company-cd--prefix))
    (candidates (cons :async (lambda (callback)
                               (company-cd--candidates callback))))
    (post-completion
     (company-cd--escape-inserted-path arg))))

(defun company-cd--candidates (callback)
  "Get all directories at `default-directory' and invoke CALLBACK."
  (-let ((search-term (substring-no-properties
                       (car (company-cd--prefix)) 0 (length (car (company-cd--prefix)))))
         (dir (f-full default-directory))
         (buffer (get-buffer-create "*company-cd-candidates*")))
    (when (process-live-p (get-buffer-process buffer))
      (kill-process (get-buffer-process buffer)))
    (with-current-buffer buffer
      (erase-buffer))
    (assert (f-exists-p dir) nil "company-cd error: dir not found %s" dir)
    (if (string-prefix-p "/ssh:" dir)
        ;; Use `directory-files' since it's tramp/ssh-aware.
        (funcall callback (seq-filter (lambda (path)
                                        (and
                                         (file-directory-p path)
                                         (not (string-equal ".." path))
                                         (not (string-equal "." path))))
                                      (directory-files dir)))
      (set-process-sentinel (start-process-shell-command
                             "company-cd-candidates"
                             buffer
                             (if (executable-find "fd")
                                 (s-lex-format "fd --color never --max-depth 2  --type directory ${search-term} . ")
                               (s-lex-format "find . \\( -type d -or -type l \\) -maxdepth 1 -not -path . -not -path ./.\\* -iname \\*${search-term}\\*")))
                            (lambda (_ event)
                              (when (string-equal event "finished\n")
                                (funcall callback (company-cd--parse buffer))))))))

(defun company-cd--prefix ()
  (-some (lambda (p)
           (let ((prefix (company-grab-symbol-cons p (length p))))
             (when (consp prefix)
               prefix)))
         '("cd " "ls ")))

(defun company-cd--parse (buffer)
  "Parse results from BUFFER."
  (ignore-errors
    (-map (lambda (path)
            (string-remove-prefix "./" path))
          (s-lines
           (s-trim (with-current-buffer buffer
                     (buffer-string)))))))

(defun company-cd--escape-inserted-path (path)
  "Replace inserted PATH with escaped equivalent."
  (delete-region (point) (- (point) (length path)))
  (insert (shell-quote-argument path)))

(provide 'company-cd)

;;; company-cd.el ends here
