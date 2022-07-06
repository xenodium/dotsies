;;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'comint)
(require 'dired)
(require 'dired-aux)
(require 'seq)

(defvar dwim-shell--execs nil "All execs in progress")

(cl-defstruct
    dwim-shell--exec
  "Describes an exec in progress."
  script
  process
  name
  calling-buffer
  reporter
  on-completion
  files-before)

(defun dwim-shell-convert-audio-to-mp3 ()
  "Convert all marked audio to mp3(s)."
  (interactive)
  (dwim-shell--dired-execute-script-on-marked-files
   "ffmpeg -stats -n -i <<f>> -acodec libmp3lame <<fne>>.mp3"
   "Convert to mp3" '("ffmpeg")))

(defun dwim-shell-convert-image-to-jpg ()
  "Convert all marked images to jpg(s)."
  (interactive)
  (dwim-shell--dired-execute-script-on-marked-files
   "convert -verbose <<f>> <<fne>>.jpg"
   "Convert to jpg" '("convert")))

(defun dwim-shell-convert-image-to-png ()
  "Convert all marked images to png(s)."
  (interactive)
  (dwim-shell--dired-execute-script-on-marked-files
   "convert -verbose <<f>> <<fne>>.png"
   "Convert to png" '("convert")))

(defun dwim-shell-convert-to-gif ()
  "Convert all marked videos to optimized gif(s)."
  (interactive)
  (dwim-shell--dired-execute-script-on-marked-files
   "ffmpeg -loglevel quiet -stats -y -i <<f>> -pix_fmt rgb24 -r 15 <<fne>>.gif"
   "Convert to gif" '("ffmpeg")))

(defun dwim-shell-convert-to-optimized-gif ()
  "Convert all marked videos to optimized gif(s)."
  (interactive)
  (dwim-shell--dired-execute-script-on-marked-files
   "ffmpeg -loglevel quiet -stats -y -i <<f>> -pix_fmt rgb24 -r 15 <<fne>>.gif
    gifsicle -O3 <<fne>>.gif --lossy=80 -o <<fne>>.gif"
   "Convert to optimized gif" '("ffmpeg" "gifsicle")))

(defun dwim-shell-unzip ()
  "Unzip all marked archives (of any kind) using `atool'."
  (interactive)
  (dwim-shell--dired-execute-script-on-marked-files
   "atool --extract --explain <<f>>" "Unzip" '("atool")))

(defun dwim-shell-speed-up-gif ()
  "Speeds up gif(s)."
  (interactive)
  (let ((factor (string-to-number
                 (completing-read "Speed up x times: " '("1" "1.5" "2" "2.5" "3" "4")))))
    (dwim-shell--dired-execute-script-on-marked-files
     (format "gifsicle -U <<f>> <<frames>> -O2 -o <<fne>>_x%s.<<e>>" factor)
     "Speed up gif" '("gifsicle" "identify")
     (lambda (script file)
       (string-replace "<<frames>>" (dwim-shell--gifsicle-frames-every factor file) script)))))

(defun dwim-shell--gifsicle-frames-every (skipping-every file)
  (string-join
   (seq-map (lambda (n) (format "'#%d'" n))
            (number-sequence 0 (string-to-number
                                ;; Get total frames count.
                                (seq-first (process-lines "identify" "-format" "%n\n" file)))
                             skipping-every)) " "))

(defun dwim-shell-drop-video-audio ()
  "Drop audio from all marked videos."
  (interactive)
  (dwim-shell--dired-execute-script-on-marked-files
   "ffmpeg -i <<f>> -c copy -an <<fne>>_no_audio.<<e>>"
   "Drop audio" '("ffmpeg")))

(defun dwim-shell--dired-execute-script-on-marked-files (script name utils &optional post-process-template on-completion)
  "Execute SCRIPT, using buffer NAME, FILES, and bin UTILS."
  (cl-assert (equal major-mode 'dired-mode) nil "Not in dired-mode")
  (dwim-shell-execute-script script name (dired-get-marked-files) utils
                             post-process-template on-completion))

(defun dwim-shell-execute-script (script name files utils &optional post-process-template on-completion)
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
                              (dwim-shell--expand template file post-process-template))))
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
    (setq files-before (dwim-shell--dired-files))
    (setq proc (start-process (buffer-name proc-buffer) proc-buffer "zsh" "-x" "-c" script))
    (setq progress-reporter (make-progress-reporter (process-name proc)))
    (progress-reporter-update progress-reporter)
    (if (equal (process-status proc) 'exit)
        (progn
          (dwim-shell--finalize (current-buffer)
                                files-before
                                proc
                                progress-reporter
                                on-completion))
      (setq dwim-shell--execs
            (push (cons (process-name proc)
                        (make-dwim-shell--exec :script script
                                               :process proc
                                               :name (process-name proc)
                                               :calling-buffer (current-buffer)
                                               :files-before (dwim-shell--dired-files)
                                               :reporter progress-reporter))
                  dwim-shell--execs))
      (set-process-sentinel proc #'dwim-shell--sentinel)
      (set-process-filter proc #'dwim-shell--filter))))

(defun dwim-shell--expand (template file &optional post-process-template)
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

(defun dwim-shell--default-directory ()
  "List of files in current buffer's `default-directory'."
  (cond ((equal major-mode 'dired-mode)
         (dwim-shell--dired-files))
        (default-directory
          (with-temp-buffer
            (let ((default-directory default-directory))
              (dired-mode default-directory)
              (when revert-buffer-function
                (funcall revert-buffer-function))
              (dwim-shell--dired-files))))))

(defun dwim-shell--dired-files ()
  "List of files in current dired buffer."
  (cl-assert (equal major-mode 'dired-mode) nil "Not in dired-mode")
  (save-excursion
    (goto-char (point-min))
    (let (r)
      (while (= 0 (forward-line))
        (when-let (filename (dired-get-filename nil t))
          (push filename r)))
      (nreverse r))))

(defun dwim-shell--finalize (calling-buffer files-before process progress-reporter on-completion)
  (when progress-reporter
    (progress-reporter-done progress-reporter))
  (if (= (process-exit-status process) 0)
      (progn
        (with-current-buffer calling-buffer
          (when revert-buffer-function
            (funcall revert-buffer-function))
          (when-let* ((oldest-new-file (car (last (seq-sort #'file-newer-than-file-p
                                                            (seq-difference (dwim-shell--default-directory)
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
  (setq dwim-shell--execs
        (map-delete dwim-shell--execs (process-name process))))

(defun dwim-shell--sentinel (process state)
  (let ((exec (map-elt dwim-shell--execs (process-name process))))
    (dwim-shell--finalize (dwim-shell--exec-calling-buffer exec)
                          (dwim-shell--exec-files-before exec)
                          process
                          (dwim-shell--exec-reporter exec)
                          (dwim-shell--exec-on-completion exec))))

(defun dwim-shell--filter (process string)
  (when-let* ((exec (map-elt dwim-shell--execs (process-name process)))
              (reporter (dwim-shell--exec-reporter exec)))
    (progress-reporter-update reporter))
  (comint-output-filter process string))

(provide 'dwim-shell)
