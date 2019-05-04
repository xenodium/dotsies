(require 'ar-vsetq)

(use-package cl
  :config
  ;; Ignore running processes when closing Emacs
  ;; From http://oremacs.com/2015/01/04/dired-nohup
  (defadvice save-buffers-kill-emacs
      (around no-query-kill-emacs activate)
    "Prevent \"Active processes exist\" query on exit."
    (cl-letf (((symbol-function #'process-list) (lambda ())))
      ad-do-it)))

;; Partially use path in buffer name.
(use-package uniquify
  :config
  (ar/vsetq uniquify-buffer-name-style
            'forward))

(use-package discover-my-major
  :ensure t)
