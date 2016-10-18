;;; use-host-package.el --- Like use-package but for host dependencies support.

;;; Commentary:
;; Like use-package but for host dependencies.

;; (use-host-package
;;  :bin "figlet"
;;  :name "figlet")

;;; Code:

(require 'files)
(require 'cl)

(defvar use-host-package-install-cmd nil "Install command (ie. \"brew install\" or \"apt-get install\")")

(defun use-host-package (&rest plist-args)
  (unless (apply #'use-host-package--installed-p plist-args)
    (apply #'use-host-package--install plist-args)
    (assert (apply #'use-host-package--installed-p plist-args)
            nil "Unfulfilled check after installation")))

(defun use-host-package--assert-variables ()
  (assert use-host-package-install-cmd nil "use-host-package-install-cmd must be set"))

(defun use-host-package--install (&rest plist-args)
  (assert use-host-package-install-cmd nil "use-host-package-install-cmd must be set")
  (let ((host-package-name (plist-get plist-args :name)))
    (message "Installing %s..." host-package-name)
    (assert (= 0 (call-process-shell-command (format "%s %s"
                                                     use-host-package-install-cmd
                                                     (shell-quote-argument host-package-name))
                                             nil (use-host-package--install-buffer)))
            nil (format "Unable to install %s" host-package-name))
    (message "Installed %s" host-package-name)))

(defun use-host-package--installed-p (&rest plist-args)
  (cond ((apply #'use-host-package--bin-installed-p plist-args))))

(defun use-host-package--bin-installed-p (&rest plist-args)
  (let ((bin-name (or (plist-get plist-args :bin)
                      (plist-get plist-args :name))))
    (when bin-name
      (executable-find bin-name))))

(defun use-host-package--install-buffer ()
  (let ((install-buffer-name "*use-host-package install*"))
    (with-current-buffer install-buffer-name
      (erase-buffer)
      (get-buffer install-buffer-name))))

(ert-deftest use-host-package--bin-installed-p-test ()
  (should (use-host-package--bin-installed-p :bin "ls"))
  (should-not (use-host-package--bin-installed-p :bin "bin-not-found"))
  (should (use-host-package--bin-installed-p :name "ls"))
  (should-not (use-host-package--bin-installed-p :name "bin-not-found"))
  (should-not (use-host-package--bin-installed-p :other-args "other-values")))

(ert-deftest use-host-package--installed-p-test ()
  (should (use-host-package--installed-p :bin "ls"))
  (should-not (use-host-package--installed-p :bin "bin-not-found"))
  (should (use-host-package--installed-p :name "ls"))
  (should-not (use-host-package--installed-p :name "bin-not-found"))
  (should-not (use-host-package--installed-p :other-args "other-values")))

(provide 'use-host-package)

;;; use-host-package.el ends here
