(use-package compile
  :commands compile
  :config
  (defun ar/compile-autoclose (buffer string)
    "Hide successful builds window with BUFFER and STRING."
    (cond ((string-match "finished" string)
           (message "Build finished")
           (run-with-timer 2 nil
                           #'delete-window
                           (get-buffer-window buffer t)))
          (t
           (next-error)
           (when (equal major-mode 'objc-mode)
             (next-error))
           (message "Compilation exited abnormally: %s" string))))
  ;; Automatically hide successful builds window.
  (setq compilation-finish-functions #'ar/compile-autoclose))
