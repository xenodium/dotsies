;;; ar-helm-projectile.el --- Helm projectile support.

;;; Commentary:
;; Helm projectile helpers.

;;; Code:

(require 'ar-shell)
(require 'cl)
(require 'helm-projectile)
(require 'esh-mode)

(defun ar/helm-projectile-shell-cd ()
  "Change shell current working directory using helm projectile."
  (interactive)
  (assert (or (equal major-mode 'eshell-mode)
              (equal major-mode 'shell-mode)) nil "Not in (E)Shell mode")
  (let ((helm-dir-source (copy-tree  helm-source-projectile-directories-list)))
    (add-to-list 'helm-dir-source (cons 'action (if (string-equal mode-name "EShell")
                                                    (lambda (path)
                                                      (insert (format "cd %s" path))
                                                      (eshell-send-input))
                                                  'ar/shell-cd)))
    (add-to-list 'helm-dir-source '(keymap . nil))
    (add-to-list 'helm-dir-source '(header-line . "cd to directory..."))
    (helm :sources helm-dir-source
          :buffer "*helm-dirs*"
          :candidate-number-limit 10000)))

(provide 'ar-helm-projectile)

;;; ar-helm-projectile.el ends here
