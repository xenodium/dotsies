;;; -*- lexical-binding: t; -*-

(use-package compile
  :hook ((compilation-mode . goto-address-mode))
  :commands (compile ar/compile)
  :functions
  (previous-error-no-select
   next-error-no-select
   kill-buffer-and-window)
  :validate-custom
  ;; Automatically scroll build output.
  (compilation-scroll-output t)
  (compilation-skip-threshold 2)
  (compilation-auto-jump-to-first-error t)
  :bind (:map
         compilation-mode-map
         ("p" . previous-error-no-select)
         ("n" . next-error-no-select)
         ("{" . compilation-previous-file)
         ("}" . compilation-next-file)
         ("q" . kill-buffer-and-window)
         :map
         prog-mode-map
         ("C-c C-c" . ar/compile))
  :config

  (defun ar/compile (pfx)
    """Run the same compile as the last time.

If there was no last time, or there is a prefix argument, this acts like
M-x compile.
"""
    (interactive "p")
    (if (and (eq pfx 1)
	     compilation-last-buffer)
        (progn
          (set-buffer compilation-last-buffer)
          (revert-buffer t t))
      (call-interactively 'compile)))

  (defun ar/compile--history-path ()
    (concat (file-name-as-directory (expand-file-name "~/.emacs.d/")) ".comphist.el"))

  (defun ar/compile--history-read ()
    (if (not (file-exists-p (ar/compile--history-path)))
        (make-hash-table :test 'equal)
      (with-temp-buffer
        (insert-file-contents (ar/compile--history-path))
        (read (current-buffer)))))

  (defun ar/compile--history-write (hashtable)
    (with-temp-buffer
      (prin1 hashtable (current-buffer))
      (write-file (ar/compile--history-path) nil)))

  (defun ar/compile--history-add (command project-root directory)
    (let* ((history (ar/compile--history-read)))
      (map-put history command (list project-root directory))
      (ar/compile--history-write history)))

  (defun ar/compile--history-get (command)
    (let* ((history (ar/compile--history-read)))
      (map-elt history command)))

  (defun ar/compile (prefix)
    (interactive "p")
    (if (and (eq prefix 1) compilation-last-buffer)
        ;; Retry using last compile command.
        (progn
          (set-buffer compilation-last-buffer)
          (revert-buffer t t))
      ;; Compile using environment caching.
      (let* ((command (compilation-read-command compile-command))
             (project-root (projectile-project-root))
             (cache (ar/compile--history-get command))
             (cached-root (nth 0 cache))
             (cached-directory (nth 1 cache))
             (potential-directory (when cached-directory
                                    (concat project-root cached-directory)))
             (current-directory default-directory)
             ;; Overriding default-directory for compile command.
             (default-directory (if (and potential-directory (file-exists-p potential-directory))
                                    potential-directory
                                  default-directory)))
        (setq ar/compile--command command)
        (setq ar/compile--project-root project-root)
        (setq ar/compile--directory (if project-root
                                        (file-relative-name current-directory project-root)
                                      current-directory))
        (compile command))))

  (defun ar/compile-cache-env (buffer string)
    (when (and (string-match "finished" string)
               (boundp 'ar/compile--command)
               (boundp 'ar/compile--directory)
               (boundp 'ar/compile--project-root))
      (ar/compile--history-add ar/compile--command
                               ar/compile--project-root
                               ar/compile--directory)))

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
           (when (and (> (count-windows) 1)
                      (get-buffer-window buffer t))
             (run-with-timer 2 nil
                             #'delete-window
                             (get-buffer-window buffer t))))
          (t
           (message "Compilation exited abnormally: %s" string))))

  ;; Automatically hide successful builds window.
  ;; Trying out without for a little while.
  (setq compilation-finish-functions (list #'ar/compile-autoclose #'ar/compile-cache-env)))
