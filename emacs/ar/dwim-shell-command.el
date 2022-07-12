;;; dwim-shell-command.el --- Execute shell commands with DWIM behaviour
;;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'comint)
(require 'dired)
(require 'dired-aux)
(require 'seq)

;;; Commentary:

;; Provides `dwim-shell-command' as a DWIM alternative to
;; `shell-command'.
;;
;; Use `dwim-shell-command-on-marked-files' to create your own command
;; line utilities, invoked via M-x.
;;
;; See `dwim-shell-command-convert-audio-to-mp3' or
;; `dwim-shell-command-pdf-password-protect' as examples.

;;; Code:

(defvar dwim-shell-command--commands nil "All commands in progress.")

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
  (dwim-shell-command-on-marked-files
   "Convert to mp3"
   "ffmpeg -stats -n -i '<<f>>' -acodec libmp3lame '<<fne>>.mp3'"
   :utils "ffmpeg"))

(defun dwim-shell-command-convert-image-to-jpg ()
  "Convert all marked images to jpg(s)."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Convert to jpg"
   "convert -verbose '<<f>>' '<<fne>>.jpg'"
   :utils "convert"))

(defun dwim-shell-command-convert-image-to-png ()
  "Convert all marked images to png(s)."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Convert to png"
   "convert -verbose '<<f>>' '<<fne>>.png'"
   :utils "convert"))

(defun dwim-shell-command-convert-image-grayscale ()
  "Convert all marked images to grayscale."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Convert image to grayscale"
   "convert -verbose -type Grayscale '<<f>>' '<<fne>>_grayscale.<<e>>'"
   :utils "convert"))

(defun dwim-shell-command-reorient-image ()
  "Reorient images."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Reorient image"
   "convert -verbose -auto-orient '<<f>>' '<<fne>>_reoriented.<<e>>'"
   :utils "convert"))

(defun dwim-shell-command-convert-to-gif ()
  "Convert all marked videos to optimized gif(s)."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Convert to gif"
   "ffmpeg -loglevel quiet -stats -y -i '<<f>>' -pix_fmt rgb24 -r 15 '<<fne>>.gif'"
   :utils "ffmpeg"))

(defun dwim-shell-command-convert-to-optimized-gif ()
  "Convert all marked videos to optimized gif(s)."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Convert to optimized gif"
   "ffmpeg -loglevel quiet -stats -y -i '<<f>>' -pix_fmt rgb24 -r 15 '<<fne>>.gif'
    gifsicle -O3 '<<fne>>.gif' --lossy=80 -o '<<fne>>.gif'"
   :utils '("ffmpeg" "gifsicle")))

(defun dwim-shell-command-unzip ()
  "Unzip all marked archives (of any kind) using `atool'."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Unzip" "atool --extract --explain '<<f>>'"
   :utils "atool"))

(defun dwim-shell-command-speed-up-gif ()
  "Speeds up gif(s)."
  (interactive)
  (let ((factor (string-to-number
                 (completing-read "Speed up x times: " '("1" "1.5" "2" "2.5" "3" "4")))))
    (dwim-shell-command-on-marked-files
     "Speed up gif"
     (format "gifsicle -U '<<f>>' <<frames>> -O2 -o '<<fne>>_x%s.<<e>>'" factor)
     :extensions "gif" :utils '("gifsicle" "identify")
     :post-process-template (lambda (script file)
       (string-replace "<<frames>>" (dwim-shell-command--gifsicle-frames-every factor file) script)))))

(defun dwim-shell-command-pdf-password-protect ()
  "Speeds up gif(s)."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Password protect pdf"
   (format "qpdf --verbose --encrypt '%s' '%s' 256 -- '<<f>>' '<<fne>>_enc.<<e>>'"
           (read-passwd "user-password: ")
           (read-passwd "owner-password: "))
   :utils "qpdf"))

(defun dwim-shell-command--gifsicle-frames-every (skipping-every file)
  "Generate frames SKIPPING-EVERY count for video FILE."
  (string-join
   (seq-map (lambda (n) (format "'#%d'" n))
            (number-sequence 0 (string-to-number
                                ;; Get total frames count.
                                (seq-first (process-lines "identify" "-format" "%n\n" file)))
                             skipping-every)) " "))

(defun dwim-shell-command-drop-video-audio ()
  "Drop audio from all marked videos."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Drop audio"
   "ffmpeg -i '<<f>>' -c copy -an '<<fne>>_no_audio.<<e>>'"
   :utils "ffmpeg"))

(defun dwim-shell-command-bin-plist-to-xml ()
  "Convert binary plist to xml."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Convert binary plist to xml."
   "plutil -convert xml1 -o '<<fne>>.xml' '<<f>>'"
   :utils "plutil"))

(defun dwim-shell-command-clipboard-to-qr ()
  "Generate a QR code from clipboard."
  (interactive)
  (let ((temp-file (concat (temporary-file-directory) "qr-code")))
    (dwim-shell-command-on-marked-files
     "Generate a QR code from clipboard"
     (format "qrencode -s10 -o %s %s" temp-file (shell-quote-argument (current-kill 0)))
     :utils "qrencode"
     :on-completion (lambda ()
                      (switch-to-buffer (find-file-noselect temp-file t))))))

(defun dwim-shell-command-convert-image-to-icns ()
  "Convert png to icns icon."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Convert png to icns icon."
   "
    # Based on http://stackoverflow.com/questions/12306223/how-to-manually-create-icns-files-using-iconutil
    # Note: png must be 1024x1024
    mkdir <<fne>>.iconset
    sips -z 16 16 '<<f>>' --out '<<fne>>.iconset/icon_16x16.png'
    sips -z 32 32 '<<f>>' --out '<<fne>>.iconset/icon_16x16@2x.png'
    sips -z 32 32 '<<f>>' --out '<<fne>>.iconset/icon_32x32.png'
    sips -z 64 64 '<<f>>' --out '<<fne>>.iconset/icon_32x32@2x.png'
    sips -z 128 128 '<<f>>' --out '<<fne>>.iconset/icon_128x128.png'
    sips -z 256 256 '<<f>>' --out '<<fne>>.iconset/icon_128x128@2x.png'
    sips -z 256 256 '<<f>>' --out '<<fne>>.iconset/icon_256x256@2x.png'
    sips -z 512 512 '<<f>>' --out '<<fne>>.iconset/icon_512x512.png'
    sips -z 512 512 '<<f>>' --out '<<fne>>.iconset/icon_256x256@2x.png'
    sips -z 1024 1024 '<<f>>' --out '<<fne>>.iconset/icon_512x512@2x.png'
    iconutil -c icns `<<fne>>.iconset'"
   :utils '("sips" "iconutil")
   :extensions "png"))

(defun dwim-git-clone-clipboard-url ()
  "Clone git URL in clipboard asynchronously and open in Dired when finished."
  (interactive)
  (cl-assert (string-match-p "^\\(http\\|https\\|ssh\\)://" (current-kill 0)) nil "No URL in clipboard")
  (let* ((url (current-kill 0))
         (download-dir (expand-file-name "~/Downloads/"))
         (project-dir (concat download-dir (file-name-base url)))
         (default-directory download-dir))
    (when (or (not (file-exists-p project-dir))
              (when (y-or-n-p (format "%s exists.  delete?" (file-name-base url)))
                (delete-directory project-dir t)
                t))
      (dwim-shell-command-on-marked-files
       (format "Clone %s" (file-name-base url))
       (format "git clone %s" url)
       :utils "git"
       :on-completion (lambda()
                        (dired project-dir))))))

(defun dwim-shell-command ()
  "Execute DWIM shell command."
  (interactive)
  (dwim-shell-command-on-marked-files
   "DWIM shell command" (read-shell-command "DWIM shell command: ")))

(cl-defun dwim-shell-command-execute-script (buffer-name script &key files extensions shell-util shell-args shell-pipe utils post-process-template on-completion)
  "Execute a script asynchronously, DWIM style with SCRIPT and BUFFER-NAME.

:FILES are used to instantiate SCRIPT as a  noweb template.

  The following are supported:

    <<f>> (file path)
    <<fne>> (file path without extension)
    <<e>> (extension)

  For example:

    Given :FILES '(\"path/to/image.png\")

    \"convert <<f>> <<fne>>.jpg\"

    yields

    \"convert 'path/to/image.png' `path/to/image.jpg'\"

:EXTENSIONS ensures that all files in :FILES have the given
extensions.  Can be either single string \"png\" or a list '(\"png\" \"jpg\").

:SHELL-UTIL and :SHELL-ARGS can be used to specify SCRIPT interpreter.

  For python, use:

    (dwim-shell-command-execute-script
       \"Print Pi\"
       \"import math
         print math.pi\"
       :shell-util \"python\"
       :shell-args \"-c\")

:SHELL-PIPE can be used to pipe SCRIPT to it

  For swift, use:

    (dwim-shell-command-on-marked-files
       \"Print Pi\"
       \"print(Double.pi)\"
       :shell-pipe \"swift -\")

:UTILS ensures that all needed command line utilities are installed.
Can be either a single string \"ffmpeg\" or a list '(\"ffmpet\" \"convert\").

:POST-PROCESS-TEMPLATE enables processing template further after noweb
instantiation.

:ON-COMPLETION is invoked after SCRIPT executes (disabling DWIM
internal behavior)."
  (cl-assert buffer-name nil "Script must have a buffer name")
  (cl-assert (not (string-empty-p script)) nil "Script must not be empty")
  (when (stringp extensions)
    (setq extensions (list extensions)))
  (when (and shell-util (stringp shell-util))
    (setq shell-util (list shell-util)))
  (when (and shell-args (stringp shell-args))
    (setq shell-args (list shell-args)))
  (when (stringp utils)
    (setq utils (list utils)))
  (let* ((proc-buffer (generate-new-buffer buffer-name))
         (template script)
         (script "")
         (files-before)
         (proc)
         (progress-reporter))
    (if (seq-empty-p files)
        (setq script template)
      (seq-do (lambda (file)
                (when extensions
                  (cl-assert (seq-contains-p extensions (downcase (file-name-extension file)))
                             nil "Not a .%s file" (string-join extensions " .")))
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
    (setq proc (apply 'start-process (seq-concatenate 'list
                                                      (list (buffer-name proc-buffer) proc-buffer)
                                                      (or shell-util '("zsh"))
                                                      (or shell-args '("-x" "-c"))
                                                      (if shell-pipe
                                                          (list (format "echo '%s' | %s" script shell-pipe))
                                                        (list script)))))
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
                                                          :reporter progress-reporter
                                                          :on-completion on-completion))
                  dwim-shell-command--commands))
      (set-process-sentinel proc #'dwim-shell-command--sentinel)
      (set-process-filter proc #'dwim-shell-command--filter))))

(defun dwim-shell-command--expand (template file &optional post-process-template)
  "Expand TEMPLATE using FILE.

Expand using <<f>> for FILE, <<fne>> for FILE without extension, and
 <<e>> for FILE extension.

  For example:

    Given FILE \"path/to/image.png\"

    \"convert '<<f>>' '<<fne>>.jpg'\"

    yields

    \"convert 'path/to/image.png' 'path/to/image.jpg'\"

Use POST-PROCESS-TEMPLATE to further expand template given own logic."
  (setq file (expand-file-name file))
  ;; "<<fne>>" with "/path/tmp.txt" -> "/path/tmp"
  (setq template (replace-regexp-in-string "\\(\<\<fne\>\>\\)" (file-name-sans-extension file) template nil nil 1))

  ;; "<<e>>" with "/path/tmp.txt" -> "txt"
  (setq template (replace-regexp-in-string "\\(\<\<e\>\>\\)" (file-name-extension file) template nil nil 1))

  ;; "<<f>>" with "/path/file.jpg" -> "/path/file.jpg"
  (setq template (replace-regexp-in-string "\\(\<\<f\>\>\\)" file template nil nil 1))
  (when post-process-template
    (setq template (funcall post-process-template template file)))
  template)

(defun dwim-shell-command--default-directory-files ()
  "List of files in current buffer's `default-directory'."
  (when default-directory
    (seq-map (lambda (filename)
               (concat default-directory filename))
             (process-lines "ls" "-1"))))

(defun dwim-shell-command--last-modified-between (before after)
  "Compare files in BEFORE and AFTER and return oldest file in diff."
  (car (last (seq-sort #'file-newer-than-file-p
                       (seq-difference after before)))))

(defun dwim-shell-command--finalize (calling-buffer files-before process progress-reporter on-completion)
  "Finalize script execution.

 CALLING-BUFFER, FILES-BEFORE, PROCESS, PROGRESS-REPORTER, and
ON-COMPLETION are all needed to finalize processing."
  (let ((oldest-new-file))
    (when progress-reporter
      (progress-reporter-done progress-reporter))
    (if (= (process-exit-status process) 0)
        (if on-completion
            (progn (funcall on-completion)
                   (kill-buffer (process-buffer process)))
          (progn
            (with-current-buffer calling-buffer
              (when (and (equal major-mode 'dired-mode)
                         revert-buffer-function)
                (funcall revert-buffer-function nil t))
              (setq oldest-new-file
                    (dwim-shell-command--last-modified-between
                     files-before
                     (dwim-shell-command--default-directory-files)))
              (when oldest-new-file
                (dired-jump nil oldest-new-file)))
            (unless (equal (process-buffer process)
                           (window-buffer (selected-window)))
              (if oldest-new-file
                  (kill-buffer (process-buffer process))
                (switch-to-buffer (process-buffer process))))))
      (if (and (buffer-name (process-buffer process))
               (y-or-n-p (format "Couldn't run %s, see output? " (buffer-name (process-buffer process)))))
          (switch-to-buffer (process-buffer process))
        (kill-buffer (process-buffer process))))
    (setq dwim-shell-command--commands
          (map-delete dwim-shell-command--commands (process-name process)))))

(defun dwim-shell-command--sentinel (process state)
  "Handles PROCESS sentinel and STATE."
  (let ((exec (map-elt dwim-shell-command--commands (process-name process))))
    (dwim-shell-command--finalize (dwim-shell-command--command-calling-buffer exec)
                                  (dwim-shell-command--command-files-before exec)
                                  process
                                  (dwim-shell-command--command-reporter exec)
                                  (dwim-shell-command--command-on-completion exec))))

(defun dwim-shell-command--filter (process output)
  "Handles PROCESS filtering and STATE and OUTPUT."
  (when-let* ((exec (map-elt dwim-shell-command--commands (process-name process)))
              (reporter (dwim-shell-command--command-reporter exec)))
    (progress-reporter-update reporter))
  (comint-output-filter process output))

(cl-defun dwim-shell-command-on-marked-files (buffer-name script &key utils extensions shell-util shell-args shell-pipe post-process-template on-completion)
  "Execute SCRIPT, using BUFFER-NAME.
See `dwim-shell-command-execute-script' for all other params."
  (dwim-shell-command-execute-script buffer-name script
                                     :files (dwim-shell-command--marked-files)
                                     :utils utils
                                     :extensions extensions
                                     :shell-util shell-util
                                     :shell-args shell-args
                                     :shell-pipe shell-pipe
                                     :post-process-template post-process-template
                                     :on-completion on-completion))

(defun dwim-shell-command--marked-files ()
  "Return buffer file (if available) or marked files for a `dired' buffer."
  (if (buffer-file-name)
      (list (buffer-file-name))
    (dired-get-marked-files)))

(provide 'dwim-shell-command)

;;; dwim-shell-command.el ends here
