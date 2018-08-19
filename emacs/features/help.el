(require 'ar-vsetq)

(use-package help
  :config
  ;; Select help window by default.
  (ar/vsetq help-window-select t))

(use-package helpful
  :ensure t
  :bind  (("C-h f" . helpful-callable)
          ("C-h v" . helpful-variable)
          ("C-h k" . helpful-key)))
