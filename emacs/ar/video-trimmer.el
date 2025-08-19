;;; video-trimmer.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'dired)
(require 'transient)
(require 'seq)

(defcustom video-trimmer-move-by-increment 1.0
  "Supported video media."
  :group 'video-trimmer
  :type 'number)

(defcustom video-trimmer-auto-show-transient-menu t
  "Supported video media."
  :group 'video-trimmer
  :type 'boolean)

(defcustom video-trimmer-supported-video
  '("3g2" "3gp" "asf" "asx" "avi" "divx" "drc" "dvb" "evo" "f4p"
    "f4v" "flv" "h264" "h265" "hevc" "m2ts" "m2v" "mkv" "mov" "mp4"
    "mpg" "mpeg" "mts" "mxf" "ogm" "ogv" "qt" "rm" "rmvb" "vob"
    "webm" "wmv")
  "Supported video media."
  :group 'video-trimmer
  :type '(repeat string))

(defvar-local video-trimmer--total nil)
(defvar-local video-trimmer--trim-front 0.0)
(defvar-local video-trimmer--trim-back 0.0)
(defvar-local video-trimmer--filename nil)
(defvar-local video-trimmer--frame-file nil)

(define-derived-mode video-trimmer-mode special-mode "Video trimmer"
  "Mode for trimming videos."
  (setq-local cursor-type nil)
  (setq-local video-trimmer--total nil)
  (setq-local video-trimmer--trim-front nil)
  (setq-local video-trimmer--trim-back nil))

(define-key video-trimmer-mode-map (kbd "}") #'video-trimmer-move-front-forward)
(define-key video-trimmer-mode-map (kbd "{") #'video-trimmer-move-front-backward)
(define-key video-trimmer-mode-map (kbd "]") #'video-trimmer-move-back-forward)
(define-key video-trimmer-mode-map (kbd "[") #'video-trimmer-move-back-backward)
(define-key video-trimmer-mode-map (kbd "+") #'video-trimmer-increment-increment)
(define-key video-trimmer-mode-map (kbd "-") #'video-trimmer-decrement-increment)
(define-key video-trimmer-mode-map (kbd "RET") #'video-trimmer-save-trimmed-copy)
(define-key video-trimmer-mode-map (kbd "?") #'video-trimmer-menu)

(defun video-trimmer-trim()
  "Create or switch to *clipper* buffer with timeline controls."
  (interactive)
  (let* ((filename (if (= 1 (length (dired-get-marked-files nil nil nil t)))
                       (nth 0 (dired-get-marked-files nil nil nil t))
                     (read-file-name "Select video file: ")))
         (buffer-name (format "*%s*" (file-name-nondirectory filename))))
    (unless (video-trimmer--is-video-p filename)
      (user-error "Not a recognized video file.  See `video-trimmer-supported-video'"))
    (switch-to-buffer (get-buffer-create buffer-name))
    (unless (eq major-mode 'video-trimmer-mode)
      (video-trimmer-mode))
    (setq-local video-trimmer--filename filename)
    (when-let ((duration (video-trimmer--get-video-duration filename)))
      (setq-local video-trimmer--total duration)
      (setq-local video-trimmer--trim-front 0.0)
      (setq-local video-trimmer--trim-back 0.0))
    (when video-trimmer-auto-show-transient-menu
      (call-interactively #'video-trimmer-menu))
    (video-trimmer--extract-frame 0.0)  ; Show initial frame
    (video-trimmer--update-timeline)))

(defun video-trimmer-increment-increment ()
  "Increment the trimming increment."
  (interactive)
  (setq video-trimmer-move-by-increment
        (cond ((< video-trimmer-move-by-increment 0.1) 0.1)
              ((< video-trimmer-move-by-increment 1.0) 1.0)
              ((< video-trimmer-move-by-increment 10.0) 10.0)
              ((< video-trimmer-move-by-increment 60.0) 60.0)
              (t video-trimmer-move-by-increment))))

(defun video-trimmer-decrement-increment ()
  "Decrement the trimming increment."
  (interactive)
  (setq video-trimmer-move-by-increment
        (cond ((> video-trimmer-move-by-increment 10.0) 10.0)
              ((> video-trimmer-move-by-increment 1.0) 1.0)
              ((> video-trimmer-move-by-increment 0.1) 0.1)
              (t video-trimmer-move-by-increment))))

(defun video-trimmer-move-front-forward ()
  "Move video front forward."
  (interactive)
  (setq video-trimmer--trim-front (min (- video-trimmer--total video-trimmer--trim-back)
                                       (+ video-trimmer--trim-front video-trimmer-move-by-increment)))
  (video-trimmer--extract-frame video-trimmer--trim-front)
  (video-trimmer--update-timeline))

(defun video-trimmer-move-front-backward ()
  "Move video front backward."
  (interactive)
  (setq video-trimmer--trim-front (max 0 (- video-trimmer--trim-front video-trimmer-move-by-increment)))
  (video-trimmer--extract-frame video-trimmer--trim-front)
  (video-trimmer--update-timeline))

(defun video-trimmer-move-back-forward ()
  "Move video back forward."
  (interactive)
  (setq video-trimmer--trim-back (max 0 (- video-trimmer--trim-back video-trimmer-move-by-increment)))
  (video-trimmer--extract-frame (- video-trimmer--total video-trimmer--trim-back))
  (video-trimmer--update-timeline))

(defun video-trimmer-move-back-backward ()
  "Move video back backward."
  (interactive)
  (setq video-trimmer--trim-back (min (- video-trimmer--total video-trimmer--trim-front)
                                      (+ video-trimmer--trim-back video-trimmer-move-by-increment)))
  (video-trimmer--extract-frame (- video-trimmer--total video-trimmer--trim-back))
  (video-trimmer--update-timeline))

(defun video-trimmer-save-trimmed-copy ()
  "Save trimmed video to a file copy."
  (interactive)
  (unless (executable-find "ffmpeg")
    (user-error "Ffmpeg not found"))
  (unless video-trimmer--filename
    (user-error "File no found"))
  (let* ((start-time video-trimmer--trim-front)
         (duration (- video-trimmer--total video-trimmer--trim-front video-trimmer--trim-back))
         (output-file (video-trimmer--unique-new-file-path video-trimmer--filename)))
    (message "Trimming video...")
    (let ((result (call-process "ffmpeg" nil nil nil
                                "-ss" (format "%.2f" start-time)
                                "-t" (format "%.2f" duration)
                                "-i" video-trimmer--filename
                                "-c" "copy"
                                "-avoid_negative_ts" "make_zero"
                                "-y" output-file)))
      (kill-buffer)
      (if (= result 0)
          (progn
            (dired-jump nil output-file)
            (message "Saved to: %s" (file-name-nondirectory output-file)))
        (message "Error trimming video")))))

(defun video-trimmer-quit ()
  "Quit video trimming tool."
  (interactive)
  (quit-restore-window (get-buffer-window (current-buffer)) 'kill))

(transient-define-prefix video-trimmer-menu ()
  "Video trimmer control menu."
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-warn
  [[:description
    (lambda () (format "Front (%s)"
                       (video-trimmer--format-time video-trimmer--trim-front)))
                 ("}" "Move Forward" video-trimmer-move-front-forward)
                 ("{" "Move Backward" video-trimmer-move-front-backward)]
   [:description
    (lambda () (format "Back (%s)"
                       (video-trimmer--format-time
                        (- video-trimmer--total video-trimmer--trim-back))))
                 ("]" "Move Forward" video-trimmer-move-back-forward)
                 ("[" "Move Backward" video-trimmer-move-back-backward)]
   [:description
    (lambda () (format "Increment (%.1fs)" video-trimmer-move-by-increment))
    ("+" "Increase" video-trimmer-increment-increment)
    ("-" "Decrease" video-trimmer-decrement-increment)]
   ["Custom"
    ("f" "Front..." video-trimmer-set-front-time)
    ("b" "Back..." video-trimmer-set-back-time)
    ]
   [""
    ("l" "Length..." video-trimmer-set-time-length)
    ("c" "Increment..." video-trimmer-set-custom-increment)
    ]
   [""
    ("RET" "Save copy" video-trimmer-save-trimmed-copy :transient nil)
    ("t" "Tutorial" video-trimmer-open-tutorial)]
   [""
    ("q" "Quit" video-trimmer-quit :transient nil)]])

(defun video-trimmer-open-tutorial ()
  "Open ffmpeg tutorial."
  (interactive)
  (browse-url "https://www.youtube.com/watch?v=9kaIXkImCAM"))

(defun video-trimmer--update-timeline ()
  "Update and display the current timeline."
  (let ((message-log-max nil))
    (message "%s" (video-trimmer--make-timeline video-trimmer--total video-trimmer--trim-front video-trimmer--trim-back))))

(defun video-trimmer--format-time (seconds)
  "Format SECONDS as MM:SS.SS or HH:MM:SS.SS if hours > 0."
  (let* ((hours (/ (floor seconds) 3600))
         (minutes (/ (mod (floor seconds) 3600) 60))
         (secs (mod (floor seconds) 60))
         (centis (floor (* 100 (- seconds (floor seconds))))))
    (if (> hours 0)
        (format "%02d:%02d:%02d.%02d" hours minutes secs centis)
      (format "%02d:%02d.%02d" minutes secs centis))))

(defun video-trimmer--make-timeline (total &optional trim-front trim-back)
  "Make a timeline bar showing TOTAL secs with TRIM-FRONT and TRIM-BACK."
  (setq trim-front (or trim-front 0))
  (setq trim-back (or trim-back 0))
  (let* ((start-time (video-trimmer--format-time trim-front))
         (end-time (video-trimmer--format-time (- total trim-back)))
         (label (format "%s -- %s" start-time end-time))
         (total-width (frame-width))
         (label-width (length label))
         (available-width (- total-width label-width))
         (front-ratio (/ (float trim-front) total))
         (back-ratio (/ (float trim-back) total))
         (front-spaces (round (* front-ratio available-width)))
         (back-spaces (round (* back-ratio available-width)))
         (middle-spaces (- available-width front-spaces back-spaces)))
    (if (and (= trim-front 0) (= trim-back 0))
        (concat start-time " "
                (make-string (- total-width (length start-time) (length end-time) 2) ?┄)
                " " end-time)
      (concat (make-string front-spaces ?\s)
              (substring label 0 (/ label-width 2))
              (make-string middle-spaces ?┄)
              (substring label (/ label-width 2))
              (make-string back-spaces ?\s)))))

(defun video-trimmer--get-video-duration (filename)
  "Get duration of video FILENAME in seconds using ffprobe."
  (unless (executable-find "ffprobe")
    (user-error "Ffprobe not found.  Do you have ffmpeg installed?"))
  (string-to-number
   (shell-command-to-string
    (format "ffprobe -v error -show_entries format=duration -of default=noprint_wrappers=1:nokey=1 %s"
            (shell-quote-argument filename)))))

(defun video-trimmer--extract-frame (timestamp)
  "Extract frame at TIMESTAMP and display in buffer."
  (unless (executable-find "ffmpeg")
    (user-error "Ffmpeg not found"))
  (let ((temp-file (expand-file-name "video-trimmer-frame.jpg" temporary-file-directory)))
    (when (and video-trimmer--frame-file
               (file-exists-p video-trimmer--frame-file))
      (delete-file video-trimmer--frame-file)
      (image-flush (create-image temp-file 'jpeg nil :max-height (frame-pixel-height))))
    (setq video-trimmer--frame-file temp-file)
    (let ((result (call-process "ffmpeg" nil nil nil
                               "-ss" (format "%.2f" timestamp)
                               "-i" video-trimmer--filename
                               "-vframes" "1"
                               "-vcodec" "mjpeg"
                               "-f" "image2"
                               "-y" temp-file)))
      (when (= result 0)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (when (file-exists-p temp-file)
            (insert-image (create-image temp-file 'jpeg nil :max-height (frame-pixel-height)))
            (insert "\n\n"))
          (goto-char (point-min)))))))

(defun video-trimmer--unique-new-file-path (file-path)
  "Return a unique FILE-PATH.

If FILE-PATH already contains a number in the format (n), set counter to n.

\"/tmp/blah.txt\" -> \"/tmp/blah(1).txt\"
\"/tmp/blah(2).txt\" -> \"/tmp/blah(3).txt\""
  (let* ((name (file-name-sans-extension file-path))
         (extension (file-name-extension file-path))
         (counter (if (string-match "(\\([0-9]+\\))$" name)
                      (string-to-number (match-string 1 name))
                    0)))
    (when (string-match "(\\([0-9]+\\))$" name)
      (setq name (replace-match "" t t name)))
    (while (file-exists-p file-path)
      (setq counter (1+ counter))
      (setq file-path (if extension
                          (format "%s(%d).%s" name counter extension)
                        (format "%s(%d)" name counter))))
    file-path))

(defun video-trimmer-set-front-time ()
  "Set trimming front to."
  (interactive)
  (setq video-trimmer--trim-front
        (video-trimmer--read-time-value "From HH:MM:SS.CC or seconds: "))
  (video-trimmer--update-timeline))

(defun video-trimmer-set-back-time ()
  "Set trimming back to."
  (interactive)
  (setq video-trimmer--trim-back
        (- video-trimmer--total
           (video-trimmer--read-time-value "From HH:MM:SS.CC or seconds: ")))
  (video-trimmer--update-timeline))

(defun video-trimmer-set-time-length ()
  "Set trimming front to."
  (interactive)
  (setq video-trimmer--trim-back
        (- video-trimmer--total
           video-trimmer--trim-front
           (video-trimmer--read-time-value "Length HH:MM:SS.CC or seconds: ")))
  (video-trimmer--update-timeline))

(defun video-trimmer-set-custom-increment ()
  "Set a custom increment."
  (interactive)
  (let ((val (read-number "Set custom increment (seconds): " video-trimmer-move-by-increment)))
    (setq video-trimmer-move-by-increment val)))

(defun video-trimmer--is-video-p (file)
  "Return non-nil if FILE extension is found in `video-trimmer-supported-video'."
  (if (file-name-extension file)
      (seq-contains-p ready-player-supported-video
                      (file-name-extension file)
                      (lambda (a b)
                        (string-equal (downcase a) (downcase b))))))

(defun video-trimmer--read-time-value (prompt)
  "PROMPT for time string and return value in seconds, or nil if invalid."
  (let ((input (read-string prompt)))
    (or (video-trimmer--parse-time-string input)
        (and (not (equal (string-to-number input) 0))
             (string-to-number input))
        (user-error "Invalid input: %s" input))))

(defun video-trimmer--parse-time-string (time-str)
  "Parse TIME-STR (MM:SS[.CC] or HH:MM:SS[.CC]) into seconds."
  (when (string-match
         "\\`\\(?1:[0-9]+:\\)?\\(?2:[0-9]+\\):\\(?3:[0-9]+\\)\\(?:\\.\\(?4:[0-9][0-9]?\\)\\)?\\'"
         time-str)
    (let ((hrs (if (match-string 1 time-str)
                   (string-to-number (substring (match-string 1 time-str) 0 -1))
                 0))
          (min (string-to-number (match-string 2 time-str)))
          (sec (string-to-number (match-string 3 time-str)))
          (centis-str (match-string 4 time-str)))
      (+ (* hrs 3600)
         (* min 60)
         sec
         (if centis-str (/ (string-to-number centis-str) 100.0) 0)))))

(provide 'video-trimmer)

;;; video-trimmer.el ends here
