;;; use-host-package.el --- Like use-package but for host dependencies support.

;;; Commentary:
;; Like use-package but for host dependencies.

;;; Code:

(require 'files)
(require 'cl)

(defvar use-host-package-install-cmd nil "Install command (ie. \"brew install\" or \"apt-get install\")")

(defun use-host-package (&rest plist-args)
  (unless (use-host-package--installed-p (plist-get plist-args :name)
                                         (plist-get plist-args :bin))
    (use-host-package--install (plist-get plist-args :name)
                               (plist-get plist-args :install-name)
                               (plist-get plist-args :install-command))
    (assert (use-host-package--installed-p (plist-get plist-args :name)
                                           (plist-get plist-args :bin))
            nil "Unfulfilled check after installation"))
  (plist-get plist-args :name))

(defun use-host-package--install (host-package-name
                                  install-name-override
                                  install-command-override)
  (let ((install-command (cond (install-command-override
                                install-command-override)
                               (install-name-override
                                (assert use-host-package-install-cmd
                                        nil
                                        "use-host-package-install-cmd must be set")
                                (format "%s %s"
                                        use-host-package-install-cmd
                                        (shell-quote-argument install-name-override)))
                               (host-package-name
                                (assert use-host-package-install-cmd
                                        nil
                                        "use-host-package-install-cmd must be set")
                                (format "%s %s"
                                        use-host-package-install-cmd
                                        (shell-quote-argument host-package-name))))))
    (message "Installing %s..." host-package-name)
    (assert (= 0 (call-process-shell-command (format "%s %s"
                                                     use-host-package-install-cmd
                                                     (shell-quote-argument host-package-name))
                                             nil (use-host-package--install-buffer)))
            nil (format "Unable to install %s" host-package-name))
    (message "Installed %s" host-package-name)))

(defun use-host-package--installed-p (host-package-name &optional bin-name)
  (cond (bin-name
         (executable-find bin-name))
        (host-package-name
         (executable-find host-package-name))))

(defun use-host-package--install-buffer ()
  (with-current-buffer (get-buffer-create "*use-host-package install*")
    (erase-buffer)
    (current-buffer)))

(ert-deftest use-host-package--installed-p-test ()
  (should (use-host-package--installed-p "ls"))
  (should-not (use-host-package--installed-p "bin-not-found"))
  (should (use-host-package--installed-p "bin-not-found"
                                         "ls"))
  (should-not (use-host-package--installed-p "bin-not-found"
                                             "bin-not-found")))

(provide 'use-host-package)

;;; use-host-package.el ends here
