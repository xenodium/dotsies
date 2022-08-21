;;; danny.el --- Danny (download nanny) package. -*- lexical-binding: t; -*-

;;; Commentary:
;; Danny monitors downloads and offers quick actions.


;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'f)
(require 'filenotify)
(require 'ht)
(require 'ivy)
(require 'subr-x)

(defvar danny-monitor-dir-path "~/Downloads" "Directory path to monitor.")

(cl-defstruct
    danny-destination-root
  name
  dpath
  recursive)

(defvar danny-ignored-regexps (list "crdownload$"
                                    "part$"
                                    "m4a$"
                                    "ytdl$"
                                    "temp\\.webm$"
                                    "temp\\.mkv$"
                                    "\\.f[0-9]+\\."
                                    "com\\.google\\.Chrome\\.")
  "List of ignored regular expressions matching file names.
These are handy for matching temp files created during downloads.

For example \"crdownload$\" and \"part$\".")

(defvar danny-destination-roots
  (list (make-danny-destination-root :name "Documents"
                                     :dpath "~/Documents")
        (make-danny-destination-root :name "Temp"
                                     :dpath "/tmp"))
  "Root destination directories.  For example:

  (list (make-danny-destination-root :name \"Documents\"
                                     :dpath \"~/Documents\")
        (make-danny-destination-root :name \"Stuff\"
                                     :dpath \"~/Stuff\"))")

(defvar danny--notify-descriptor nil "File notify descriptor.")

(defvar danny--history-hash (ht) "History hashtable.")

(defvar danny--history-path "~/.danny")

(defvar danny--base-frame-params
  (list
   (cons 'auto-raise t)
   (cons 'font "Menlo 15")
   (cons 'internal-border-width 10)
   (cons 'left 0.33)
   (cons 'left-fringe 0)
   (cons 'line-spacing 3)
   (cons 'menu-bar-lines 0)
   (cons 'minibuffer 'only)
   (cons 'right-fringe 0)
   (cons 'tool-bar-lines 0)
   (cons 'top 200)
   (cons 'undecorated t)
   (cons 'unsplittable t)
   (cons 'vertical-scroll-bars nil))
  "Base frame parameters.")

(defvar danny--active-frame nil "Active danny frame")

(defun danny-stop-monitoring ()
  (interactive)
  "Stop monitoring for new files."
  (when (file-notify-valid-p danny--notify-descriptor)
    (file-notify-rm-watch danny--notify-descriptor)
    (setq danny--notify-descriptor nil))
  (danny--log "Stopped monitoring \"%s\"" danny-monitor-dir-path))

(defun danny-start-listening ()
  "Start monitoring for new files."
  (interactive)
  (danny-stop-monitoring)
  (setq danny--history-hash (danny--read-history-hashtable danny--history-path))
  (cl-assert danny-monitor-dir-path nil "`danny-monitor-dir-path' must be set")
  (setq danny--notify-descriptor (file-notify-add-watch danny-monitor-dir-path
                                                        '(change attribute-change)
                                                        'danny--notify-callback))
  (danny--log "Started monitoring \"%s\"" danny-monitor-dir-path))

(defun danny--notify-callback (event)
  "Handle EVENT for file-notify events."
  (when-let ((new-fpath (or (danny--new-file-for-event event)
                            (danny--new-downloaded-file-for-event event))))
    (when (and (not (eq (frame-focus-state) t))
               (> (f-size new-fpath) 0))
      (danny--choose-action new-fpath))))

(defun danny--log (&rest args)
  (let* ((log-buffer-name  "*danny log*")
         (log-buffer (get-buffer-create log-buffer-name))
         (log-file-name "~/.danny.log")
         (log-str (concat
                   (format-time-string "%Y-%m-%d-%H:%M:%S: ")
                   (apply 'format args)
                   "\n")))
    (save-excursion
      (with-current-buffer log-buffer
        (setq buffer-read-only nil)
        (goto-char (point-max))
        (insert log-str)
        (setq buffer-read-only t))
      (with-temp-buffer
        (insert log-str)
        (write-region (point-min) (point-max) log-file-name t
                      'nomessage)))))

(defun danny--new-file-for-event (event)
  "Handle EVENT for file-notify events."
  (let ((event-type (nth 1 event))
        (fpath (nth 2 event)))
    (if (and (eq event-type 'created)
             (f-file-p fpath)
             (not (-find-index (lambda (ignored-regexp)
                                 (s-matches-p ignored-regexp fpath))
                               danny-ignored-regexps)))
        (progn
          (danny--log "Is new file? yes %s %s" event-type fpath)
          fpath)
      (danny--log "Is new file? no %s %s" event-type fpath)
      nil)))

(defun danny--new-downloaded-file-for-event (event)
  "Handle EVENT for file-notify events for downloads."
  (let* ((event-type (nth 1 event))
         (src-fpath (nth 2 event))
         (dst-fpath (when (eq event-type 'renamed)
                      (nth 3 event))))
    (if (and (eq event-type 'renamed)
             (f-file-p dst-fpath)
             ;; Check some.part was renamed to some.txt.
             ;; part is one of the ignored extensions.
             (and
              (-find-index (lambda (ignored-regexp)
                             (s-matches-p ignored-regexp src-fpath))
                           danny-ignored-regexps)
              (not (-find-index (lambda (ignored-regexp)
                                  (s-matches-p ignored-regexp dst-fpath))
                                danny-ignored-regexps))))
        (progn
          (danny--log "Is download? yes %s %s %s"
                      event-type src-fpath (or dst-fpath "nil"))
          dst-fpath)
      (danny--log "Is download? no %s %s %s"
                  event-type src-fpath (or dst-fpath "nil"))
      nil)))

(defun danny-process-last-download ()
  "Process last downloaded file."
  (interactive)
  (let* ((fpaths (f-files danny-monitor-dir-path))
         (newest-fpath (-first-item fpaths)))
    (-each fpaths
      (lambda (fpath)
        (when (file-newer-than-file-p fpath newest-fpath)
          (setq newest-fpath fpath))))
    (cl-assert newest-fpath nil "No files available")
    (danny--choose-action newest-fpath)))

(defun danny--move-file (src-fpath dst-dpath)
  (unless (f-exists-p dst-dpath)
    (error "Directory not found: %s" dst-dpath))
  (let ((dst-fpath (f-join dst-dpath
                           (danny--read-string (format "Save as (%s): " (f-filename src-fpath))
                                               (f-filename src-fpath)
                                               20))))
    (when (equal src-fpath dst-fpath)
      (error "Moving to same location"))
    (if (f-exists-p dst-fpath)
        (progn
          (unless (danny--y-or-n (format "Override? %s\n" dst-fpath))
            (error "Aborted"))
          (rename-file src-fpath dst-fpath t)
          (danny--add-destination-dir dst-dpath)
          (danny--log "%s -> %s" src-fpath dst-fpath)
          (kill-new dst-fpath))
      (rename-file src-fpath dst-fpath t)
      (when (fboundp  'dired-jump)
        (dired-jump nil dst-fpath))
      (danny--add-destination-dir dst-dpath)
      (danny--log "%s -> %s" src-fpath dst-fpath)
      (kill-new dst-fpath))))

(defun danny--create-move-action-fun (src-fpath)
  (lambda (dst-dpath)
    (danny--move-file src-fpath dst-dpath)))

(defun danny--create-open-action-fun (file-path)
  (lambda (ignored)
    (x-focus-frame nil)
    (find-file file-path)))

(defun danny--choose-action (fpath)
  (if danny--active-frame
      (danny--log "Ignoring download (%s), there's an existing session." fpath)
    (danny--framed-ivy-read
     (list
      (make-danny--framed-ivy-source
       :prompt (format "Action on (%s): " (f-filename fpath))
       :collection (lambda ()
                     (-concat
                      (list
                       (cons "Open" (lambda ()
                                      (unless (f-exists-p fpath)
                                        (error "File not found: %s" fpath))
                                      (if ivy-current-prefix-arg
                                          (shell-command (format "open \"%s\"" fpath))
                                        (x-focus-frame nil)
                                        (find-file fpath))))
                       (cons "Move" (lambda ()
                                      (danny--handle-file-created fpath)))
                       (cons "Reveal" (lambda ()
                                        (x-focus-frame nil)
                                        (if (fboundp  'dired-jump)
                                            (dired-jump nil fpath)
                                          (find-file (f-dirname fpath)))))
                       (cons "Delete" (lambda ()
                                        (when (danny--y-or-n (format "Delete? %s\n" fpath))
                                          (delete-file fpath))))
                       (cons "Open in /tmp" (lambda ()
                                              (let ((dst-fpath (f-join "/tmp"
                                                                       (f-filename fpath))))
                                                (rename-file fpath
                                                             dst-fpath t)
                                                (if ivy-current-prefix-arg
                                                    (shell-command (format "open \"%s\"" dst-fpath))
                                                  (x-focus-frame nil)
                                                  (find-file dst-fpath))))))
                      (-map
                       (lambda (destination)
                         (cons (format "Move to %s" destination)
                               (lambda ()
                                 (danny--move-file fpath destination))))
                       (danny--last-destinations))))
       :action (lambda (item)
                 (funcall (cdr item)))
       :unwind (lambda ()
                 (danny--kill-active-frame)))))))

(defun danny--handle-file-created (file-path)
  "Handle new file created at FILE-PATH."
  (let ((unwind (lambda ()
                  (danny--kill-active-frame))))
    (danny--framed-ivy-read
     (-concat (list
               (make-danny--framed-ivy-source
                :prompt (format "Save in Recent (%s): " (f-filename file-path))
                :action (danny--create-move-action-fun file-path)
                :collection (lambda ()
                              (danny--last-destinations))
                :unwind unwind))
              (-map
               (lambda (root)
                 (make-danny--framed-ivy-source
                  :prompt (format "Save in \"%s\" (%s): "
                                  (danny-destination-root-name root)
                                  (f-filename file-path))
                  :action (danny--create-move-action-fun file-path)
                  :collection (lambda ()
                                (-concat
                                 (list (danny-destination-root-dpath root))
                                 (f-directories (danny-destination-root-dpath root)
                                                nil
                                                (danny-destination-root-recursive root))))
                  :unwind unwind))
               danny-destination-roots)))))

(cl-defstruct
    danny--framed-ivy-source
  prompt
  collection
  action
  unwind)

(cl-defun danny--framed-ivy-read (sources &key index initial-input)
  (let ((kmap (make-sparse-keymap))
        (source))
    (cl-assert (> (length sources) 0))
    (when (null index)
      (setq index 0))
    (setq source (nth index sources))
    (define-key kmap (kbd "<right>") (lambda ()
                                       (interactive)
                                       (ivy-quit-and-run (danny--framed-ivy-read sources
                                                                                 :index (if (>= (1+ index)
                                                                                                (length sources))
                                                                                            0
                                                                                          (1+ index))
                                                                                 :initial-input ivy-text))))
    (define-key kmap (kbd "<left>") (lambda ()
                                      (interactive)
                                      (ivy-quit-and-run (danny--framed-ivy-read sources
                                                                                :index (if (< (1- index)
                                                                                              0)
                                                                                           (1- (length sources))
                                                                                         (1- index))
                                                                                :initial-input ivy-text))))
    (let* ((collection (funcall (danny--framed-ivy-source-collection source)))
           (lines-count (+ (length (s-split "\n" (danny--framed-ivy-source-prompt source)))
                           (length collection))))
      (danny--kill-active-frame)

      (setq danny--active-frame
            (make-frame
             (-concat danny--base-frame-params
                      (list (cons 'height (min (+ 2 lines-count)
                                               25))
                            ;; Calculate a sensible width, based on longest path or prompt.
                            (cons 'width (max (+ 10 (length (danny--framed-ivy-source-prompt source)))
                                              (+ 10 (danny--longest-line-length collection))))))))
      (x-focus-frame danny--active-frame)
      (ivy-read (danny--framed-ivy-source-prompt source)
                collection
                :require-match t
                :update-fn (lambda ()
                             ;; Forcing redisplay works around "Open" source not shown
                             ;; after having visited other sources (left/right keys).
                             (redisplay))
                :action (danny--framed-ivy-source-action source)
                :initial-input initial-input
                :unwind (danny--framed-ivy-source-unwind source)
                :keymap kmap))))

(defun danny--read-string (prompt default width)
  (let* ((input)
         (lines (s-split "\n" prompt))
         (frame (make-frame
                 (-concat danny--base-frame-params
                          (list (cons 'height (length lines))
                                (cons 'width (+ 1 width (danny--longest-line-length lines))))))))
    (setq input (read-string prompt default))
    (delete-frame frame)
    (other-window 1)
    (if (> (length input) 0)
        input
      default)))

(defun danny--y-or-n (prompt)
  "Frame-based yes or no dialog with PROMPT."
  (danny--kill-active-frame)
  (let* ((input)
         (lines (s-split "\n" prompt))
         (height (1+ (length lines)))
         (frame (make-frame
                 (-concat danny--base-frame-params
                          (list (cons 'height height)
                                (cons 'width (+ 20 (danny--longest-line-length lines))))))))
    (setq input (yes-or-no-p prompt))
    (delete-frame frame)
    (other-window 1)
    input))

(defun danny--kill-active-frame ()
  (when danny--active-frame
    (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
      (abort-recursive-edit))
    (delete-frame danny--active-frame))
  (setq danny--active-frame nil)
  (when (minibufferp)
    (other-window 1)))

(defun danny--longest-line-length (lines)
  "Return the longest length in LINES."
  (let ((longest 0))
    (mapc (lambda (item)
            ;; Collection items may be text or cons of text and function.
            (let ((text (if (consp item)
                            (car item)
                          item)))
              (when (> (length text) longest)
                (setq longest (length text)))))
          lines)
    longest))

(defun danny--write-history-hashtable (hash-fpath hashtable)
  "Write history HASHTABLE in HASH-FPATH."
  (with-temp-buffer
    (prin1 hashtable (current-buffer))
    (write-file hash-fpath nil)))

(defun danny--read-history-hashtable (hash-fpath)
  "Read history hash in HASH-FPATH."
  (if (not (f-exists? hash-fpath))
      (ht-create)
    (with-temp-buffer
      (insert-file-contents hash-fpath)
      (read (current-buffer)))))

(defun danny--last-destinations ()
  "Return a list of recent destinations."
  (ht-get danny--history-hash "last-destinations" '()))

(defun danny--add-destination-dir (dpath)
  "Return a recent DIR to history.."
  (ht-set danny--history-hash
          "last-destinations"
          (-distinct (-concat (ht-get danny--history-hash
                                      "last-destinations" '()) (list dpath))))
  (danny--write-history-hashtable danny--history-path danny--history-hash)
  danny--history-hash)

(provide 'danny)

;;; danny.el ends here
