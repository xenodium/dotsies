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
          (propertize (if (= (user-uid) 0)
                          "#"
                        "$") 'face 'eshell-prompt)
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
