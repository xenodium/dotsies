;;; ar-eshell-config.el --- Eshell config support.

;;; Commentary:
;; Eshell config helpers.


;;; Code:

(require 'esh-mode)
(require 'shrink-path)

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

(defun  ar/eshell-config-setup-aliases ()
  (eshell/alias "ec" "find-file $1"))

(provide 'ar-eshell-config)

;;; ar-eshell-config.el ends here
