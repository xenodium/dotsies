(require 'ar-csetq)

(use-package compile
  :hook ((compilation-mode . goto-address-mode))
  :commands compile
  :bind (:map
         compilation-mode-map
         ("p" . previous-error-no-select)
         ("n" . next-error-no-select)
         ("{" . compilation-previous-file)
         ("}" . compilation-next-file))
  :config

  ;; http://ivanmalison.github.io/dotfiles/#colorizecompliationbuffers
  (defun ar/colorize-compilation-buffer ()
    (let ((was-read-only buffer-read-only))
      (unwind-protect
          (progn
            (when was-read-only
              (read-only-mode -1))
            (ansi-color-apply-on-region (point-min) (point-max)))
        (when was-read-only
          (read-only-mode +1)))))

  (add-hook 'compilation-filter-hook 'ar/colorize-compilation-buffer)

  (defun ar/compile-autoclose (buffer string)
    "Hide successful builds window with BUFFER and STRING."
    (cond ((string-match "finished" string)
           (message "Build finished")
           (when (> (count-windows) 1)
             (run-with-timer 2 nil
                             #'delete-window
                             (get-buffer-window buffer t))))
          (t
           (next-error)
           (when (equal major-mode 'objc-mode)
             (next-error))
           (message "Compilation exited abnormally: %s" string))))

  ;; Automatically hide successful builds window.
  (setq compilation-finish-functions #'ar/compile-autoclose)

  ;; Automatically scroll build output.
  (ar/csetq compilation-scroll-output t))
