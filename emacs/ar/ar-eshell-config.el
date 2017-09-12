;;; ar-eshell-config.el --- Eshell config support.

;;; Commentary:
;; Eshell config helpers.


;;; Code:

(require 'esh-mode)
(require 'em-alias)
(require 'em-prompt)
(require 'shrink-path)
(require 'validate)

(defun ar/eshell-config--prompt-char ()
  "Return shell config character, based on current OX. For example, an  for MacOS."
  (let ((os-char (cond ((ar/osx-p) "")
                       ((ar/linux-p) "🐧")
                       (t "?"))))
    (format "%s %s" os-char (if (= (user-uid) 0)
                                "#"
                              "$"))))

(defun ar/eshell-config--prompt-function ()
  "Make eshell prompt purrrty."
  (let ((shrinked-dpath (car (shrink-path-prompt (eshell/pwd))))
        (dname (cdr (shrink-path-prompt (eshell/pwd)))))
    (concat "\n┌─ " shrinked-dpath dname "\n"
            "└─>"
            (propertize (ar/eshell-config--git-branch-prompt)
                        'face 'font-lock-function-name-face)
            " "
            (propertize (ar/eshell-config--prompt-char) 'face 'eshell-prompt-face)
            ;; needed for the input text to not have prompt face
            (propertize " " 'face 'default))))

(defun ar/eshell-config--git-branch-prompt ()
  "Git branch prompt."
  (let ((branch (car (loop for match in (split-string (shell-command-to-string "git branch") "\n")
                           when (string-match "^\*" match)
                           collect match))))
    (if (not (eq branch nil))
        (concat " [" (substring branch 2)  "]")
      "")))

(defun eshell/clear ()
  "Alias to clear (destructive) eshell content."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))


(defun eshell/emacs (&rest args)
  "Open a file (ARGS) in Emacs.  Some habits die hard."
  (if (null args)
      ;; If I just ran "emacs", I probably expect to be launching
      ;; Emacs, which is rather silly since I'm already in Emacs.
      ;; So just pretend to do what I ask.
      (bury-buffer)
    ;; We have to expand the file names or else naming a directory in an
    ;; argument causes later arguments to be looked for in that directory,
    ;; not the starting directory
    (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))

(defalias 'eshell/e 'eshell/emacs)

(defun eshell/ec (&rest args)
  "Compile a file (ARGS) in Emacs.  Use `compile' to do background make."
  (if (eshell-interactive-output-p)
      (let ((compilation-process-setup-function
             (list 'lambda nil
                   (list 'setq 'process-environment
                         (list 'quote (eshell-copy-environment))))))
        (compile (eshell-flatten-and-stringify args))
        (pop-to-buffer compilation-last-buffer))
    (throw 'eshell-replace-command
           (let ((l (eshell-stringify-list (eshell-flatten-list args))))
             (eshell-parse-command (car l) (cdr l))))))
(put 'eshell/ec 'eshell-no-numeric-conversions t)

(defun eshell-view-file (file)
  "View FILE.  A version of `view-file' which properly rets the eshell prompt."
  (interactive "fView file: ")
  (unless (file-exists-p file) (error "%s does not exist" file))
  (let ((had-a-buf (get-file-buffer file))
        (buffer (find-file-noselect file)))
    (if (eq (with-current-buffer buffer (get major-mode 'mode-class))
            'special)
        (progn
          (switch-to-buffer buffer)
          (message "Not using View mode because the major mode is special"))
      (let ((undo-window (list (window-buffer) (window-start)
                               (+ (window-point)
                                  (length (funcall eshell-prompt-function))))))
        (switch-to-buffer buffer)
        (view-mode-enter (cons (selected-window) (cons nil undo-window))
                         'kill-buffer)))))

(defun eshell/less (&rest args)
  "Invoke `view-file' on a file (ARGS).  \"less +42 foo\" will go to line 42 in the buffer for foo."
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (eshell-view-file file)
          (forward-line line))
      (eshell-view-file (pop args)))))

(defalias 'eshell/more 'eshell/less)

;; https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org
;; (defun eshell/find (&rest args)
;;   "Wrapper around the ‘find’ executable and ARGS."
;;   (let ((cmd (concat "find " (string-join args))))
;;     (shell-command-to-string cmd)))

(provide 'ar-eshell-config)

;;; ar-eshell-config.el ends here
