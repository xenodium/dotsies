;;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'comint)
(require 'dired)
(require 'dired-aux)
(require 'seq)

(defvar dired-script--execs nil "All execs in progress")

(cl-defstruct
    dired-script--exec
  "Describes an exec in progress."
  script
  process
  name
  calling-buffer
  reporter)

(defun dired-script-convert-to-gif ()
  (interactive)
  (dired-script--dired-execute-script-on-marked-files
   "ffmpeg -loglevel quiet -stats -y -i ${f} -pix_fmt rgb24 -r 15 ${fne}.gif"
   "Convert to gif" '("ffmpeg")))

(defun dired-script-drop-audio ()
  (interactive)
  (dired-script--dired-execute-script-on-marked-files
   "ffmpeg -i ${f} -c copy -an ${fne}_no_audio.${e}"
   "Remove audio" '("ffmpeg")))

(defun dired-script--dired-execute-script-on-marked-files (script name utils)
  "Execute SCRIPT, using buffer NAME, FILES, and bin UTILS."
  (cl-assert (equal major-mode 'dired-mode) nil "Not in dired-mode")
  (dired-script-execute-script script name (dired-get-marked-files) utils))

(defun dired-script-execute-script (script name files utils)
  "Execute SCRIPT, using buffer NAME, FILES, and bin UTILS."
  (cl-assert (not (string-empty-p script)) nil "Script must not be empty")
  (cl-assert name nil "Script must have a name")
  (let* ((proc-buffer (generate-new-buffer name))
         (template script)
         (script "")
         (proc))
    (if (seq-empty-p files)
        (setq script template)
      (seq-do (lambda (file)
                (setq script (concat script "\n" (dired-script--expand template file))))
              files))
    (setq script (string-trim script))
    (seq-do (lambda (util)
              (cl-assert (executable-find util) nil
                         (format "%s not installed" util)))
            utils)
    (setq proc (start-process (buffer-name proc-buffer) proc-buffer "zsh" "-x" "-c" script))
    (with-current-buffer proc-buffer
      (setq default-directory default-directory)
      (shell-command-save-pos-or-erase)
      (require 'shell)
      (shell-mode)
      (view-mode +1)
      (setq view-exit-action 'kill-buffer))
    (if (equal (process-status proc) 'exit)
        (progn
          (if (= (process-exit-status proc) 0)
              (progn
                ;; (message "Finished %s" (buffer-name (process-buffer proc)))
                (with-current-buffer (current-buffer)
                  (when revert-buffer-function)
                  (funcall revert-buffer-function))
                (unless (equal (process-buffer proc)
                               (window-buffer (selected-window)))
                  (kill-buffer (process-buffer proc))))
            (if (y-or-n-p (format "Couldn't run %s, see output? " (buffer-name (process-buffer proc))))
                (switch-to-buffer (process-buffer proc))
              (unless (equal (process-buffer proc)
                             (window-buffer (selected-window)))
                (kill-buffer (process-buffer proc))))))
      (setq dired-script--execs
            (push (cons (process-name proc)
                        (make-dired-script--exec :script script
                                        :process proc
                                        :name (process-name proc)
                                        :calling-buffer (current-buffer)
                                        :reporter (make-progress-reporter (process-name proc))))
                  dired-script--execs))
      (set-process-sentinel proc #'dired-script--sentinel)
      (set-process-filter proc #'dired-script--filter))))

(defun dired-script--expand (template file)
  "Expand TEMPLATE. FIXME."
  (setq file (expand-file-name file))
  ;; "${f}" with "/path/file.jpg" -> "'/path/file.jpg'"
  (when (string-match "[[:blank:]]\\($\{f\}\\)\\([[:blank:]]\\|$\\)" template)
    (setq template (replace-match (format "'%s'" file) nil nil template 1)))
  ;; "${fne}_other_${e}" with "/path/file.jpg" -> "'/path/file_other.jpg'"
  (when (string-match "[[:blank:]]\\(\\($\{fne\}\\)\\([^ ]+\\)\\($\{e\}\\)\\)" template)
    (setq template (replace-match (format "'%s\\3%s'"
                                          (file-name-sans-extension file)
                                          (file-name-extension file)) nil nil template 1)))
  ;; "${fne}.gif" with "/path/tmp.txt" -> "'/path/tmp.gif'"
  (when (string-match "[[:blank:]]\\(\\($\{fne\}\\)\\([^ ]+\\)\\([[:blank:]]\\|$\\)\\)" template)
    (setq template (replace-match (format "'%s\\3'\\4"
                                          (file-name-sans-extension file)) nil nil template 1)))
  template)

;; (dired-script--expand "someutil ${f} -flag" "/path/to/tmp.txt")
;; (dired-script--expand "someutil ${fne}_another.${e} -flag" "/hom/tmp.txt")
;; (dired-script--expand "someutil ${fne}.gif -flag" "/hom/tmp.txt")

(defun dired-script--filter (process string)
  (when-let* ((exec (map-elt dired-script--execs (process-name process)))
              (reporter (dired-script--exec-reporter exec)))
    (progress-reporter-update reporter))
  (comint-output-filter process string))

(defun dired-script--sentinel (process state)
  (let ((exec (map-elt dired-script--execs (process-name process))))
    (when exec
      (progress-reporter-done (dired-script--exec-reporter exec)))
    (if (= (process-exit-status process) 0)
        (progn
          (with-current-buffer (dired-script--exec-calling-buffer exec)
            (when revert-buffer-function)
            (funcall revert-buffer-function))
          (unless (equal (process-buffer process)
                         (window-buffer (selected-window)))
            (kill-buffer (process-buffer process))))
      (if (y-or-n-p (format "Couldn't run %s, see output? " (buffer-name (process-buffer process))))
          (switch-to-buffer (process-buffer process))
        (kill-buffer (process-buffer process)))))
  (setq dired-script--execs
        (map-delete dired-script--execs (process-name process))))

(provide 'dired-script)
