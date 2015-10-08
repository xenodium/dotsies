;;; ar-helm-projectile.el --- Helm projectile support.

;;; Commentary:
;; Helm projectile helpers.

;;; Code:

(require 'ar-shell)
(require 'helm-projectile)

(defun ar/helm-projectile-shell-cd ()
  "Change shell shell current working directory using helm projectile."
  (interactive)
  (unless (string-equal mode-name "Shell")
    (error "Not in Shell mode"))
  (let ((helm-dir-source (copy-tree  helm-source-projectile-directories-list)))
    (add-to-list 'helm-dir-source '(action . ar/shell-cd))
    (helm :sources helm-dir-source
          :buffer "*helm-dirs*"
          :candidate-number-limit 10000)))

(provide 'ar-helm-projectile)

;;; ar-helm-projectile.el ends here
