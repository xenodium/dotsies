(use-package s
  :ensure t)

(use-package async
  :ensure t
  :config
  (async-bytecomp-package-mode +1))

;; https://oremacs.com/2015/01/17/setting-up-ediff
;; Macro for setting custom variables.
;; Similar to custom-set-variables, but more like setq.
(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))


(use-package validate
  :ensure t
  :config
  (defalias 'vsetq 'validate-setq))
