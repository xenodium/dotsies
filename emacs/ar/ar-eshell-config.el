;;; ar-eshell-config.el --- Eshell config support.

;;; Commentary:
;; Eshell config helpers.


;;; Code:

(require 'esh-mode)
(require 'em-alias)
(require 'em-prompt)
(require 'f)
(require 'shrink-path)
(require 'validate)
(require 'iimage)
(require 'cl-lib)

(defun ar/eshell-config--prompt-char ()
  "Return shell config character, based on current OX. For example, an  for MacOS."
  (let ((os-char (cond ((string-equal system-type "darwin") "")
                       ((string-equal system-type "gnu/linux") "🐧")
                       (t "?"))))
    (format "%s %s" os-char (if (= (user-uid) 0)
                                "#"
                              "$"))))

(defun ar/eshell-config--shrinked-dpath ()
  "Shrinked current directory path."
  (car (shrink-path-prompt (eshell/pwd))))

(defun ar/eshell-config--dname ()
  "Current directory name (no path)."
  (f-filename (eshell/pwd)))

(defun ar/eshell-config--prompt-function ()
  "Make eshell prompt purrrty."
  (concat "\n┌─ "
          (abbreviate-file-name (if (f-root-p (eshell/pwd))
                                    (eshell/pwd)
                                  (f-parent (eshell/pwd))))
          "/"
          (propertize (ar/eshell-config--dname)
                      'face 'eshell-ls-directory)
          "\n"
          "└─>"
          (propertize (ar/eshell-config--git-branch-prompt)
                      'face 'font-lock-function-name-face)
          " "
          (propertize (ar/eshell-config--prompt-char) 'face 'eshell-prompt)
          ;; needed for the input text to not have prompt face
          (propertize " " 'face 'default)))

(defun ar/eshell-config--git-branch-prompt ()
  "Git branch prompt."
  (let ((branch (car (cl-loop for match in (split-string (shell-command-to-string "git branch") "\n")
                           when (string-match "^\*" match)
                           collect match))))
    (if (not (eq branch nil))
        (concat " [" (substring branch 2)  "]")
      "")))

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
(defalias 'eshell/ec 'eshell/emacs)

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

(validate-setq eshell-prompt-function #'ar/eshell-config--prompt-function)

;; https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org
;; (defun eshell/find (&rest args)
;;   "Wrapper around the ‘find’ executable and ARGS."
;;   (let ((cmd (concat "find " (string-join args))))
;;     (shell-command-to-string cmd)))

(defun ar/image-p (fpath)
  "Return non-nil if FPATH is an image file."
  (seq-some (lambda (line)
              (string-match-p "image" line))
            (process-lines "file" "-b" "--mime-type"
                           (expand-file-name fpath))))

;; Inspired by https://emacs.stackexchange.com/a/9737
(defun adviced:eshell/cat (orig-fun &rest args)
  "Display image when using cat on it."
  (let ((image-fpath (seq-some (lambda (arg)
                                 (when (ar/image-p (expand-file-name arg))
                                   (expand-file-name arg)))
                               args)))
    (if (not image-fpath)
        (apply orig-fun args)
      (add-text-properties 0 (length image-fpath)
                           `(display ,(create-image image-fpath)
                                     modification-hooks
                                     (iimage-modification-hook))
                           image-fpath)
      (eshell-buffered-print image-fpath))
    (eshell-flush)))

(advice-add #'eshell/cat :around #'adviced:eshell/cat)

(provide 'ar-eshell-config)

;;; ar-eshell-config.el ends here
