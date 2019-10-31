;;; -*- lexical-binding: t; -*-
(use-package use-package-ensure-system-package
  :ensure t)

(use-package use-package-chords
  :ensure t
  :config
  (run-with-idle-timer 2 nil
                       (lambda ()
                         (key-chord-mode 1))))

;; Ask shell for PATH, MANPATH, and exec-path and update Emacs environment.
;; We do this early on as we assert binaries are installed throughout
;; init.
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package auto-compile
  :ensure t
  :config
  (auto-compile-on-load-mode +1)
  (auto-compile-on-save-mode +1))

(use-package validate
  :ensure t
  :config
  (defmacro validate-customize-set-variable (variable value &optional comment)
    "Like `customize-set-variable', but throw an error if validation fails.
VALUE is validated against SYMBOL's custom type.

\(fn [SYM VAL] ...)"
    `(if (boundp ,variable)
         ;; (customize-set-variable ,variable (validate-value ,value (custom-variable-type ,variable)) ,comment)
         (customize-set-variable ,variable ,value ,comment)
       ;; (user-error "Trying to validate a variable that's not defined yet: `%s'.\nYou need to require the package before validating" ,variable)
       ))

  (add-to-list 'use-package-keywords :custom-validated)

  (defun use-package-normalize/:custom-validated (_name keyword args)
    "Normalize use-package custom-validated keyword."
    (use-package-as-one (symbol-name keyword) args
      #'(lambda (label arg)
          (unless (listp arg)
            (use-package-error
             (concat label " a (<symbol> <value> [comment])"
                     " or list of these")))
          (if (use-package-non-nil-symbolp (car arg))
              (list arg)
            arg))))

  (defun use-package-handler/:custom-validated (name _keyword args rest state)
    "Generate use-package custom-validated keyword code."
    (use-package-concat
     (mapcar
      #'(lambda (def)
          (let ((variable (nth 0 def))
                (value (nth 1 def))
                (comment (nth 2 def)))
            (unless (and comment (stringp comment))
              (setq comment (format "Customized with use-package %s" name)))
            `(validate-customize-set-variable (quote ,variable) ,value ,comment)))
      args)
     (use-package-process-keywords name rest state))))
