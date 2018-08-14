(require 'ar-csetq)

;; Growl-workalike for Emacs.
(use-package alert
  :ensure t
  :commands alert
  :config
  (cond ((string-equal system-type "darwin")
         (ar/csetq alert-default-style 'osx-notifier))
        ((string-equal system-type "gnu/linux")
         (ar/csetq alert-default-style 'notifications))
        (t
         (message "Unrecognized system for alert"))))
