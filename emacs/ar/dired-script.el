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
  reporter
  on-completion
  files-before)

(defun dired-script-convert-audio-to-mp3 ()
  "Convert all marked audio to mp3(s)."
  (interactive)
  (dired-script--dired-execute-script-on-marked-files
   "ffmpeg -stats -n -i <<f>> -acodec libmp3lame <<fne>>.mp3"
   "Convert to mp3" '("ffmpeg")))

(defun dired-script-convert-image-to-jpg ()
  "Convert all marked images to jpg(s)."
  (interactive)
  (dired-script--dired-execute-script-on-marked-files
   "convert -verbose <<f>> <<fne>>.jpg"
   "Convert to jpg" '("convert")))

(defun dired-script-convert-image-to-png ()
  "Convert all marked images to png(s)."
  (interactive)
  (dired-script--dired-execute-script-on-marked-files
   "convert -verbose <<f>> <<fne>>.png"
   "Convert to png" '("convert")))

(defun dired-script-convert-to-gif ()
  "Convert all marked videos to optimized gif(s)."
  (interactive)
  (dired-script--dired-execute-script-on-marked-files
   "ffmpeg -loglevel quiet -stats -y -i <<f>> -pix_fmt rgb24 -r 15 <<fne>>.gif"
   "Convert to gif" '("ffmpeg")))

(defun dired-script-convert-to-optimized-gif ()
  "Convert all marked videos to optimized gif(s)."
  (interactive)
  (dired-script--dired-execute-script-on-marked-files
   "ffmpeg -loglevel quiet -stats -y -i <<f>> -pix_fmt rgb24 -r 15 <<fne>>.gif
    gifsicle -O3 <<fne>>.gif --lossy=80 -o <<fne>>.gif"
   "Convert to optimized gif" '("ffmpeg" "gifsicle")))

(defun dired-script-unzip ()
  "Unzip all marked archives (of any kind) using `atool'."
  (interactive)
  (dired-script--dired-execute-script-on-marked-files
   "atool --extract --explain <<f>>" "Unzip" '("atool")))

(defun dired-script-speed-up-gif ()
  "Speeds up gif(s)."
  (interactive)
  (let ((factor (string-to-number
                 (completing-read "Speed up x times: " '("1" "1.5" "2" "2.5" "3" "4")))))
    (dired-script--dired-execute-script-on-marked-files
     (format "gifsicle -U <<f>> <<frames>> -O2 -o <<fne>>_x%s.<<e>>" factor)
     "Speed up gif" '("gifsicle" "identify")
     (lambda (script file)
       (string-replace "<<frames>>" (dired-script--gifsicle-frames-every factor file) script)))))

(defun dired-script--gifsicle-frames-every (skipping-every file)
  (string-join
   (seq-map (lambda (n) (format "'#%d'" n))
            (number-sequence 0 (string-to-number
                                ;; Get total frames count.
                                (seq-first (process-lines "identify" "-format" "%n\n" file)))
                             skipping-every)) " "))

(defun dired-script-drop-video-audio ()
  "Drop audio from all marked videos."
  (interactive)
  (dired-script--dired-execute-script-on-marked-files
   "ffmpeg -i <<f>> -c copy -an <<fne>>_no_audio.<<e>>"
   "Drop audio" '("ffmpeg")))

(defun dired-script--dired-execute-script-on-marked-files (script name utils &optional post-process-template on-completion)
  "Execute SCRIPT, using buffer NAME, FILES, and bin UTILS."
  (cl-assert (equal major-mode 'dired-mode) nil "Not in dired-mode")
  (dired-script-execute-script script name (dired-get-marked-files) utils
                               post-process-template on-completion))

(defun dired-script--dired-files ()
  "List of files in current dired buffer."
  (cl-assert (equal major-mode 'dired-mode) nil "Not in dired-mode")
  (save-excursion
    (goto-char (point-min))
    (let (r)
      (while (= 0 (forward-line))
        (when-let (filename (dired-get-filename nil t))
          (push filename r)))
      (nreverse r))))

(defun dired-script-execute-script (script name files utils &optional post-process-template on-completion)
  "Execute SCRIPT, using buffer NAME, FILES, and bin UTILS."
  (cl-assert (not (string-empty-p script)) nil "Script must not be empty")
  (cl-assert name nil "Script must have a name")
  (let* ((proc-buffer (generate-new-buffer name))
         (template script)
         (script "")
         (files-before)
         (proc))
    (if (seq-empty-p files)
        (setq script template)
      (seq-do (lambda (file)
                (setq script
                      (concat script "\n"
                              (dired-script--expand template file post-process-template))))
              files))
    (setq script (string-trim script))
    (seq-do (lambda (util)
              (cl-assert (executable-find util) nil
                         (format "%s not installed" util)))
            utils)
    (with-current-buffer proc-buffer
      (setq default-directory default-directory)
      (shell-command-save-pos-or-erase)
      (view-mode +1)
      (setq view-exit-action 'kill-buffer))
    (setq files-before (dired-script--dired-files))
    (setq proc (start-process (buffer-name proc-buffer) proc-buffer "zsh" "-x" "-c" script))
    (if (equal (process-status proc) 'exit)
        (progn
          (if (= (process-exit-status proc) 0)
              (progn
                (when on-completion
                  (funcall on-completion))
                (with-current-buffer (current-buffer)
                  (when revert-buffer-function
                    (funcall revert-buffer-function))
                  (when-let* ((files-after (dired-script--dired-files))
                              (oldest-new-file (car (last (seq-sort #'file-newer-than-file-p
                                                                    (seq-difference files-after
                                                                                    files-before))))))
                    (dired-goto-file oldest-new-file)))
                (unless (equal (process-buffer proc)
                               (window-buffer (selected-window)))
                  (kill-buffer (process-buffer proc))))
            (if (y-or-n-p (format "Couldn't run %s, see output? " (buffer-name (process-buffer proc))))
                (switch-to-buffer (process-buffer proc))
              (unless (equal (process-buffer proc)
                             (window-buffer (selected-window)))
                (kill-buffer (process-buffer proc))))))
      (with-current-buffer proc-buffer
        (require 'shell)
        (shell-mode))
      (setq dired-script--execs
            (push (cons (process-name proc)
                        (make-dired-script--exec :script script
                                                 :process proc
                                                 :name (process-name proc)
                                                 :calling-buffer (current-buffer)
                                                 :files-before (dired-script--dired-files)
                                                 :reporter (make-progress-reporter (process-name proc))))
                  dired-script--execs))
      (set-process-sentinel proc #'dired-script--sentinel)
      (set-process-filter proc #'dired-script--filter))))

(defun dired-script--expand (template file &optional post-process-template)
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
            (when revert-buffer-function
              (funcall revert-buffer-function))
            (when-let* ((before (dired-script--exec-files-before exec))
                        (after (dired-script--dired-files))
                        (oldest-new-file (car (last (seq-sort #'file-newer-than-file-p
                                                              (seq-difference after before))))))
              (dired-goto-file oldest-new-file)))
          (when (dired-script--exec-on-completion exec)
            (funcall (dired-script--exec-on-completion exec)))
          (unless (equal (process-buffer process)
                         (window-buffer (selected-window)))
            (kill-buffer (process-buffer process))))
      (if (y-or-n-p (format "Couldn't run %s, see output? " (buffer-name (process-buffer process))))
          (switch-to-buffer (process-buffer process))
        (kill-buffer (process-buffer process)))))
  (setq dired-script--execs
        (map-delete dired-script--execs (process-name process))))

(provide 'dired-script)
