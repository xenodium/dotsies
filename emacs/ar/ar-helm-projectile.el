;;; ar-helm-projectile.el --- Helm projectile support.

;;; Commentary:
;; Helm projectile helpers.

;;; Code:

(require 'helm-projectile)

(defun ar/shell-cd (dir-path)
  "Change shell current working directory to DIR-PATH."
  (unless (string-equal mode-name "Shell")
    (error "Not in Shell mode"))
  (message mode-name)
  (goto-char (point-max))
  (comint-kill-input)
  (insert (concat "cd " (shell-quote-argument dir-path)))
  (let ((comint-process-echoes t))
    (comint-send-input)))

(defun ar/helm-shell-projectile-cd ()
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
