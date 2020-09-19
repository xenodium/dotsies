;;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)

(defun musica-info ()
  (interactive)
  (let ((raw (process-lines "pytunes" "info")))
    (message "%s [%s] %s"
             (string-trim (string-remove-prefix "Title" (nth 3 raw)))
             (string-trim (string-remove-prefix "Artist" (nth 1 raw)))
             (string-trim (string-remove-prefix "Album" (nth 2 raw))))))

(defun musica-play-pause ()
  (interactive)
  (cl-assert (executable-find "pytunes") nil "pytunes not installed")
  (process-lines "pytunes" "play")
  (musica-info))

(defun musica-play-next ()
  (interactive)
  (cl-assert (executable-find "pytunes") nil "pytunes not installed")
  (process-lines "pytunes" "next"))

(defun musica-play-next-random ()
  (interactive)
  (cl-assert (executable-find "pytunes") nil "pytunes not installed")
  (process-lines "pytunes" "shuffle" "enable")
  (let-alist (seq-random-elt (musica--index))
    (process-lines "pytunes" "play" .filename))
  (musica-info))

(defun musica-play-previous ()
  (interactive)
  (cl-assert (executable-find "pytunes") nil "pytunes not installed")
  (process-lines "pytunes" "previous"))

(defun musica-index ()
  "Indexes Music's tracks in two stages:
1. Generates \"Tracks.sqlite\" using pytunes (needs https://github.com/hile/pytunes installed).
2. Caches an index at ~/.emacs.d/.musica.el."
  (interactive)
  (message "Indexing music... started")
  (let* ((now (current-time))
         (name "Music indexing")
         (buffer (get-buffer-create (format "*%s*" name))))
    (with-current-buffer buffer
      (delete-region (point-min)
                     (point-max)))
    (set-process-sentinel
     (start-process name
                    buffer
                    (file-truename (expand-file-name invocation-name
                                                     invocation-directory))
                    "--quick" "--batch" "--eval"
                    (prin1-to-string
                     `(progn
                        (interactive)
                        (require 'cl-lib)
                        (require 'seq)
                        (require 'map)

                        (message "Generating Tracks.sqlite...")
                        (process-lines "pytunes" "update-index") ;; Generates Tracks.sqlite
                        (message "Generating Tracks.sqlite... done")

                        (defun parse-tags (path)
                          (with-temp-buffer
                            (if (eq 0 (call-process "ffprobe" nil t nil "-v" "quiet"
                                                    "-print_format" "json" "-show_format" path))
                                (map-elt (json-parse-string (buffer-string)
                                                            :object-type 'alist)
                                         'format)
                              (message "Warning: Couldn't read track metadata for %s" path)
                              (message "%s" (buffer-string))
                              (list (cons 'filename path)))))

                        (let* ((paths (process-lines "sqlite3"
                                                     (concat (expand-file-name "~/")
                                                             "Music/Music/Music Library.musiclibrary/Tracks.sqlite")
                                                     "select path from tracks"))
                               (total (length paths))
                               (n 0)
                               (records (seq-map (lambda (path)
                                                   (let ((tags (parse-tags path)))
                                                     (message "%d/%d %s" (setq n (1+ n))
                                                              total (or (map-elt (map-elt tags 'tags) 'title) "No title"))
                                                     tags))
                                                 paths)))
                          (with-temp-buffer
                            (prin1 records (current-buffer))
                            (write-file "~/.emacs.d/.musica.el" nil))))))
     (lambda (process state)
       (if (= (process-exit-status process) 0)
           (message "Indexing music... finished (%.3fs)"
                    (float-time (time-subtract (current-time) now)))
         (message "Indexing music... failed, see %s" buffer))))))

(defun musica-search ()
  (interactive)
  (cl-assert (executable-find "pytunes") nil "pytunes not installed")
  (let* ((c1-width (round (* (- (window-width) 9) 0.4)))
         (c2-width (round (* (- (window-width) 9) 0.3)))
         (c3-width (- (window-width) 9 c1-width c2-width)))
    (ivy-read "Play: " (mapcar
                        (lambda (track)
                          (let-alist track
                            (cons (format "%s   %s   %s"
                                          (truncate-string-to-width
                                           (or .tags.title
                                               (file-name-base .filename)
                                               "No title") c1-width nil ?\s "…")
                                          (truncate-string-to-width (propertize (or .tags.artist "")
                                                                                'face '(:foreground "yellow")) c2-width nil ?\s "…")
                                          (truncate-string-to-width
                                           (propertize (or .tags.album "")
                                                       'face '(:foreground "cyan1")) c3-width nil ?\s "…"))
                                  track)))
                        (musica--index))
              :action (lambda (selection)
                        (let-alist (cdr selection)
                          (process-lines "pytunes" "play" .filename)
                          (message "Playing: %s [%s] %s"
                                   (or .tags.title
                                       (file-name-base .filename)
                                       "No title")
                                   (or .tags.artist
                                       "No artist")
                                   (or .tags.album
                                       "No album")))))))

(defun musica--index ()
  (with-temp-buffer
    (insert-file-contents "~/.emacs.d/.musica.el")
    (read (current-buffer))))

(provide 'musica)
