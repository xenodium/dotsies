;;; -*- lexical-binding: t; -*-

(use-package proced
  :functions ar/proced--hook-fun
  :commands (proced ar/quick-kill-process)
  :hook (proced-mode . ar/proced--hook-fun)
  :config
  (defun ar/proced--hook-fun ()
    (setq proced-auto-update-flag t))

  (require 'map)
  (require 'proced)
  (require 'seq)

  (defun ar/quick-kill-process ()
    (interactive)
    (let* ((pid-width 5)
           (comm-width 25)
           (user-width 10)
           (processes (proced-process-attributes))
           (candidates
            (mapcar (lambda (attributes)
                      (let* ((process (cdr attributes))
                             (pid (format (format "%%%ds" pid-width) (map-elt process 'pid)))
                             (user (format (format "%%-%ds" user-width)
                                           (truncate-string-to-width
                                            (map-elt process 'user) user-width nil nil t)))
                             (comm (format (format "%%-%ds" comm-width)
                                           (truncate-string-to-width
                                            (map-elt process 'comm) comm-width nil nil t)))
                             (args-width (- (window-width) (+ pid-width user-width comm-width 3)))
                             (args (map-elt process 'args)))
                        (cons (if args
                                  (format "%s %s %s %s" pid user comm (truncate-string-to-width args args-width nil nil t))
                                (format "%s %s %s" pid user comm))
                              process)))
                    processes))
           (selection (map-elt candidates
                               (completing-read "kill process: "
                                                (seq-sort
                                                 (lambda (p1 p2)
                                                   (string-lessp (nth 2 (split-string (string-trim (car p1))))
                                                                 (nth 2 (split-string (string-trim (car p2))))))
                                                 candidates) nil t)))
           (prompt-title (format "%s %s %s"
                                 (map-elt selection 'pid)
                                 (map-elt selection 'user)
                                 (map-elt selection 'comm))))
      (when (y-or-n-p (format "Kill? %s" prompt-title))
        (if (eq (signal-process (map-elt selection 'pid) 9) 0)
            (message "killed: %s" prompt-title)
          (message "error: could not kill %s" prompt-title))))))
