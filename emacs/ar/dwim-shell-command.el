;;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'comint)
(require 'dired)
(require 'dired-aux)
(require 'seq)

(defvar dwim-shell-command--commands nil "All commands in progress")

(cl-defstruct
    dwim-shell-command--command
  "Describes a command in progress."
  script
  process
  name
  calling-buffer
  reporter
  on-completion
  files-before)

(defun dwim-shell-command-convert-audio-to-mp3 ()
  "Convert all marked audio to mp3(s)."
  (interactive)
  (dwim-shell-command--on-marked-files
   "ffmpeg -stats -n -i <<f>> -acodec libmp3lame <<fne>>.mp3"
   "Convert to mp3" '("ffmpeg")))

(defun dwim-shell-command-convert-image-to-jpg ()
  "Convert all marked images to jpg(s)."
  (interactive)
  (dwim-shell-command--on-marked-files
   "convert -verbose <<f>> <<fne>>.jpg"
   "Convert to jpg" '("convert")))

(defun dwim-shell-command-convert-image-to-png ()
  "Convert all marked images to png(s)."
  (interactive)
  (dwim-shell-command--on-marked-files
   "convert -verbose <<f>> <<fne>>.png"
   "Convert to png" '("convert")))

(defun dwim-shell-command-convert-to-gif ()
  "Convert all marked videos to optimized gif(s)."
  (interactive)
  (dwim-shell-command--on-marked-files
   "ffmpeg -loglevel quiet -stats -y -i <<f>> -pix_fmt rgb24 -r 15 <<fne>>.gif"
   "Convert to gif" '("ffmpeg")))

(defun dwim-shell-command-convert-to-optimized-gif ()
  "Convert all marked videos to optimized gif(s)."
  (interactive)
  (dwim-shell-command--on-marked-files
   "ffmpeg -loglevel quiet -stats -y -i <<f>> -pix_fmt rgb24 -r 15 <<fne>>.gif
    gifsicle -O3 <<fne>>.gif --lossy=80 -o <<fne>>.gif"
   "Convert to optimized gif" '("ffmpeg" "gifsicle")))

(defun dwim-shell-command-unzip ()
  "Unzip all marked archives (of any kind) using `atool'."
  (interactive)
  (dwim-shell-command--on-marked-files
   "atool --extract --explain <<f>>" "Unzip" '("atool")))

(defun dwim-shell-command-speed-up-gif ()
  "Speeds up gif(s)."
  (interactive)
  (let ((factor (string-to-number
                 (completing-read "Speed up x times: " '("1" "1.5" "2" "2.5" "3" "4")))))
    (dwim-shell-command--on-marked-files
     (format "gifsicle -U <<f>> <<frames>> -O2 -o <<fne>>_x%s.<<e>>" factor)
     "Speed up gif" '("gifsicle" "identify")
     (lambda (script file)
       (string-replace "<<frames>>" (dwim-shell-command--gifsicle-frames-every factor file) script)))))

(defun dwim-shell-command--gifsicle-frames-every (skipping-every file)
  (string-join
   (seq-map (lambda (n) (format "'#%d'" n))
            (number-sequence 0 (string-to-number
                                ;; Get total frames count.
                                (seq-first (process-lines "identify" "-format" "%n\n" file)))
                             skipping-every)) " "))

(defun dwim-shell-command-drop-video-audio ()
  "Drop audio from all marked videos."
  (interactive)
  (dwim-shell-command--on-marked-files
   "ffmpeg -i <<f>> -c copy -an <<fne>>_no_audio.<<e>>"
   "Drop audio" '("ffmpeg")))

(defun dwim-shell-command--on-marked-files (script name utils &optional post-process-template on-completion)
  "Execute SCRIPT, using buffer NAME, FILES, and bin UTILS."
  (dwim-shell-command-execute-script script name (dwim-shell-command--marked-files) utils
                             post-process-template on-completion))

(defun dwim-shell-command--marked-files ()
  "Return buffer file (if available) or marked files for a `dired' buffer."
  (if (buffer-file-name)
      (list (buffer-file-name))
    (dired-get-marked-files)))

(defun dwim-shell-command-execute-script (script name files utils &optional post-process-template on-completion)
  "Execute SCRIPT, using buffer NAME, FILES, and bin UTILS."
  (cl-assert (not (string-empty-p script)) nil "Script must not be empty")
  (cl-assert name nil "Script must have a name")
  (let* ((proc-buffer (generate-new-buffer name))
         (template script)
         (script "")
         (files-before)
         (proc)
         (progress-reporter))
    (if (seq-empty-p files)
        (setq script template)
      (seq-do (lambda (file)
                (setq script
                      (concat script "\n"
                              (dwim-shell-command--expand template file post-process-template))))
              files))
    (setq script (string-trim script))
    (seq-do (lambda (util)
              (cl-assert (executable-find util) nil
                         (format "%s not installed" util)))
            utils)
    (with-current-buffer proc-buffer
      (require 'shell)
      (shell-mode))
    (with-current-buffer proc-buffer
      (setq default-directory default-directory)
      (shell-command-save-pos-or-erase)
      (view-mode +1)
      (setq view-exit-action 'kill-buffer))
    (setq files-before (dwim-shell-command--default-directory-files))
    (setq proc (start-process (buffer-name proc-buffer) proc-buffer "zsh" "-x" "-c" script))
    (setq progress-reporter (make-progress-reporter (process-name proc)))
    (progress-reporter-update progress-reporter)
    (if (equal (process-status proc) 'exit)
        (progn
          (dwim-shell-command--finalize (current-buffer)
                                files-before
                                proc
                                progress-reporter
                                on-completion))
      (setq dwim-shell-command--commands
            (push (cons (process-name proc)
                        (make-dwim-shell-command--command :script script
                                               :process proc
                                               :name (process-name proc)
                                               :calling-buffer (current-buffer)
                                               :files-before files-before
                                               :reporter progress-reporter))
                  dwim-shell-command--commands))
      (set-process-sentinel proc #'dwim-shell-command--sentinel)
      (set-process-filter proc #'dwim-shell-command--filter))))

(defun dwim-shell-command--expand (template file &optional post-process-template)
  "Expand TEMPLATE, using <<f>> for FILE, <<fne>> for FILE without
 extension, and <<e>> for FILE extension."
  (setq file (expand-file-name file))
  ;; "<<fne>>_other_<<e>>" with "/path/file.jpg" -> "'/path/file_other.jpg'"
  (setq template (replace-regexp-in-string "[[:blank:]]\\(\\(\<\<fne\>\>\\)\\([^ \n]+\\)\\(\<\<e\>\>\\)\\)"
                                           (format "'%s\\3%s'"
                                                   (file-name-sans-extension file)
                                                   (file-name-extension file))
                                           template nil nil 1))
  ;; "<<fne>>.gif" with "/path/tmp.txt" -> "'/path/tmp.gif'"
  (setq template (replace-regexp-in-string "[[:blank:]]\\(\\(\<\<fne\>\>\\)\\([^ \n]+\\)\\([[:blank:]]\\|$\\)\\)"
                                           (format "'%s\\3'\\4" (file-name-sans-extension file)) template nil nil 1))
  ;; "<<fne>>" with "/path/tmp.txt" -> "'/path/tmp'"
  (setq template (replace-regexp-in-string "\\(\<\<fne\>\>\\)"
                                           (format "'%s'" (file-name-sans-extension file)) template nil nil 1))
  ;; "<<f>>" with "/path/file.jpg" -> "'/path/file.jpg'"
  (setq template (replace-regexp-in-string "[[:blank:]]\\(\<\<f\>\>\\)\\([[:blank:]]\\|$\\)"
                                           (format "'%s'" file)
                                           template nil nil 1))
  (when post-process-template
    (setq template (funcall post-process-template template file)))
  template)

(defun dwim-shell-command--default-directory-files ()
  "List of files in current buffer's `default-directory'."
  (cond ((equal major-mode 'dired-mode)
         (dwim-shell-command--dired-files))
        (default-directory
          (with-temp-buffer
            (let ((default-directory default-directory))
              (dired-mode default-directory)
              (when revert-buffer-function
                (funcall revert-buffer-function nil t))
              (dwim-shell-command--dired-files))))))

(defun dwim-shell-command--dired-files ()
  "List of files in current dired buffer."
  (cl-assert (equal major-mode 'dired-mode) nil "Not in dired-mode")
  (save-excursion
    (goto-char (point-min))
    (let (r)
      (while (= 0 (forward-line))
        (when-let (filename (dired-get-filename nil t))
          (push filename r)))
      (nreverse r))))

(defun dwim-shell-command--finalize (calling-buffer files-before process progress-reporter on-completion)
  (when progress-reporter
    (progress-reporter-done progress-reporter))
  (if (= (process-exit-status process) 0)
      (progn
        (with-current-buffer calling-buffer
          (when (and (equal major-mode 'dired-mode)
                     revert-buffer-function)
            (funcall revert-buffer-function nil t))
          (when-let* ((oldest-new-file (car (last (seq-sort #'file-newer-than-file-p
                                                            (seq-difference (dwim-shell-command--default-directory-files)
                                                                            files-before))))))
            (dired-jump nil oldest-new-file)))
        (when on-completion
          (funcall on-completion))
        (unless (equal (process-buffer process)
                       (window-buffer (selected-window)))
          (kill-buffer (process-buffer process))))
    (if (y-or-n-p (format "Couldn't run %s, see output? " (buffer-name (process-buffer process))))
        (switch-to-buffer (process-buffer process))
      (kill-buffer (process-buffer process))))
  (setq dwim-shell-command--commands
        (map-delete dwim-shell-command--commands (process-name process))))

(defun dwim-shell-command--sentinel (process state)
  (let ((exec (map-elt dwim-shell-command--commands (process-name process))))
    (dwim-shell-command--finalize (dwim-shell-command--command-calling-buffer exec)
                          (dwim-shell-command--command-files-before exec)
                          process
                          (dwim-shell-command--command-reporter exec)
                          (dwim-shell-command--command-on-completion exec))))

(defun dwim-shell-command--filter (process string)
  (when-let* ((exec (map-elt dwim-shell-command--commands (process-name process)))
              (reporter (dwim-shell-command--command-reporter exec)))
    (progress-reporter-update reporter))
  (comint-output-filter process string))

(provide 'dwim-shell-command)
