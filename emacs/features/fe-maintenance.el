;;; -*- lexical-binding: t; -*-

;; Find errors in init.el by bisecting the file.
(use-package bug-hunter
  :ensure t
  :commands bug-hunter-init-file)

;; Restart Emacs from Emacs.
(when (< emacs-major-version 29)
  (use-package restart-emacs
    :ensure t
    :commands restart-emacs))

;; Safely delete packages without breaking depending packages.
(use-package package-safe-delete
  :ensure t
  :commands (package-safe-delete
             ar/reinstall-package)
  :config
  ;; https://emacsredux.com/blog/2020/09/12/reinstalling-emacs-packages/
  (defun ar/reinstall-package (package)
    (interactive (list (intern (completing-read "reinstall: "
                                                (mapcar #'car package-alist)))))
    (unload-feature package)
    (package-reinstall package)))

(use-package esup
  :ensure t
  :commands esup)

;; Peak into macros by expanding them inline.
(use-package macrostep
  :ensure t
  :commands macrostep-expand)

(use-package profiler
  :commands ar/profiler-start-cpu
  :init
  (defun ar/profiler-start-cpu ()
    "Start cpu profiler."
    (interactive)
    (profiler-start 'cpu)))

;; From https://github.com/hlissner/doom-emacs/blob/5dacbb7cb1c6ac246a9ccd15e6c4290def67757c/core/autoload/debug.el#L57
(defun ar/am-i-secure ()
  "Test to see if your root certificates are securely configured in emacs."
  (declare (interactive-only t))
  (interactive)
  (unless (string-match-p "\\_<GNUTLS\\_>" system-configuration-features)
    (warn "gnutls support isn't built into Emacs, there may be problems"))
  (if-let* ((bad-hosts
             (cl-loop for bad
                      in '("https://wrong.host.badssl.com/"
                           "https://self-signed.badssl.com/")
                      if (condition-case _e
                             (url-retrieve-synchronously bad)
                           (error nil))
                      collect bad)))
      (error (format "tls seems to be misconfigured (it got %s)."
                     bad-hosts))
    (url-retrieve "https://badssl.com"
                  (lambda (status)
                    (if (or (not status) (plist-member status :error))
                        (warn "Something went wrong.\n\n%s" (pp-to-string status))
                      (message "Your trust roots are set up properly.\n\n%s" (pp-to-string status))
                      t)))))
