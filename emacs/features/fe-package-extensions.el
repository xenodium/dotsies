;;; -*- lexical-binding: t; -*-

(use-package gcmh
  :ensure t
  :config
  (gcmh-mode +1))

(use-package use-package-ensure-system-package
  :ensure t)

(use-package use-package-chords
  :ensure t
  :config
  (run-with-idle-timer 60 nil
                       (lambda ()
                         (key-chord-mode 1)))
  (add-hook 'minibuffer-setup-hook
            (defun ar/disable-key-chord-mode ()
              (set (make-local-variable 'input-method-function) nil))))

;; Ask shell for PATH, MANPATH, and exec-path and update Emacs environment.
;; We do this early on as we assert binaries are installed throughout
;; init.
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (if (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
      (progn
	(message "Native comp is available")
        (when (eq system-type 'darwin)
          (customize-set-variable 'native-comp-driver-options '("-Wl,-w")))
        (setq native-comp-async-report-warnings-errors 'silent)
        ;; Using Emacs.app/Contents/MacOS/bin since it was compiled with
        ;; ./configure --prefix="$PWD/nextstep/Emacs.app/Contents/MacOS"
        ;; Append to path to give priority to values from exec-path-from-shell-initialize.
        (add-to-list 'exec-path (concat invocation-directory (file-name-as-directory "bin")) t)
	(setenv "LIBRARY_PATH" (concat (getenv "LIBRARY_PATH")
                                       (when (getenv "LIBRARY_PATH")
                                         ":")
				       ;; This is where Homebrew puts libgccjit libraries.
                                       (car (file-expand-wildcards
                                             (expand-file-name "/opt/homebrew/opt/libgccjit/lib/gcc/*")))))
	;; Only set after LIBRARY_PATH can find gcc libraries.
	(setq comp-deferred-compilation t)
        (setq comp-speed 3))
    (message "Native comp is *not* available")))

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
         (customize-set-variable ,variable (validate-value ,value (custom-variable-type ,variable)) ,comment)
       (user-error "Trying to validate a variable that's not defined yet: `%s'.\nYou need to require the package before validating" ,variable)))

  (setq use-package-keywords
        (use-package-list-insert :validate-custom
                                 use-package-keywords
                                 :load t))

  (defun use-package-normalize/:validate-custom (_name keyword args)
    "Normalize use-package validate-custom keyword."
    (use-package-as-one (symbol-name keyword) args
      #'(lambda (label arg)
          (unless (listp arg)
            (use-package-error
             (concat label " a (<symbol> <value> [comment])"
                     " or list of these")))
          (if (use-package-non-nil-symbolp (car arg))
              (list arg)
            arg))))

  (defun use-package-handler/:validate-custom (name _keyword args rest state)
    "Generate use-package validate-custom keyword code."
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
