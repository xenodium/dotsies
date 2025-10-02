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
  (compilation-max-output-line-length nil)
  (compilation-ask-about-save nil)
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
  (defun ar/compile (prefix)
    (interactive "p")
    (if (and (eq prefix 1)
             ;; Check if command invoked via binding.
             (eq (key-binding (this-command-keys)) this-command)
             (buffer-live-p compilation-last-buffer))
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
             (potential-directory (when (and cached-directory
                                             (file-exists-p (concat project-root cached-directory)))
                                    (concat project-root cached-directory)))
             ;; Overriding default-directory for compile command.
             (default-directory (or potential-directory default-directory)))
        (setq ar/compile--command command)
        (setq ar/compile--project-root project-root)
        (setq ar/compile--directory (if project-root
                                        (file-relative-name default-directory project-root)
                                      default-directory))
        (compile command))))

  (defun ar/compile-forget-command ()
    (interactive)
    (let* ((history (ar/compile--history-read))
           (command (completing-read "Forget compile command: " (map-keys history))))
      (ar/compile--history-write (map-delete history command))))

  (defun ar/compile--history-path ()
    (concat user-emacs-directory ".comphist.el"))

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
      (map-put history (string-trim command) (list project-root directory))
      (ar/compile--history-write history)))

  (defun ar/compile--history-get (command)
    (let* ((history (ar/compile--history-read)))
      (map-elt history (string-trim command))))

  (defun ar/compile-cache-env (buffer string)
    (when (and (string-match "finished" string)
               (boundp 'ar/compile--command)
               (boundp 'ar/compile--directory)
               (boundp 'ar/compile--project-root))
      (ar/compile--history-add ar/compile--command
                               ar/compile--project-root
                               ar/compile--directory)
      (makunbound 'ar/compile--command)
      (makunbound 'ar/compile--directory)
      (makunbound 'ar/compile--project-root)))

  (defun ar/compile-autoclose (buffer string)
    "Hide successful builds window with BUFFER and STRING."
    (if (string-match "finished" string)
        (progn
          (message "Build finished :)")
          (run-with-timer 3 nil
                          (lambda ()
                            (when-let* ((multi-window (> (count-windows) 1))
                                        (live (buffer-live-p buffer))
                                        (window (get-buffer-window buffer t)))
                              (delete-window window)))))
      (message "Compilation %s" string)))

  ;; Automatically hide successful builds window.
  ;; Trying out without for a little while.
  (setq compilation-finish-functions (list #'ar/compile-cache-env #'ar/compile-autoclose))

  (defun ar/colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))

  (add-hook 'compilation-filter-hook 'ar/colorize-compilation-buffer))
