;;; ar-eshell-config.el --- Eshell config support.

;;; Commentary:
;; Eshell config helpers.


;;; Code:

(defun ar/eshell-config--prompt-char ()
  "Return shell config character, based on current OX. For example, an  for MacOS."
  (let ((os-char (cond ((ar/osx-p) "")
                       ((ar/linux-p) "🐧")
                       (t "?"))))
    (format "%s %s" os-char (if (= (user-uid) 0)
                                "#"
                              "$"))))

(ar/eshell-config--prompt-char)

(defun ar/eshell-config--prompt-function ()
  (concat
   (propertize "┌─ " 'face `(:foreground "green"))
   (propertize (concat (eshell/pwd)) 'face 'comint-highlight-input)
   (propertize "\n" 'face `(:foreground "green"))
   (propertize "└─>" 'face `(:foreground "green"))
   (propertize (format " %s " (ar/eshell-config--prompt-char))
               'face `(:foreground "green"))
   ))

(setq eshell-prompt-function #'ar/eshell-config--prompt-function)

(provide 'ar-eshell-config)

;;; ar-eshell-config.el ends here
