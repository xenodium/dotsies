;;; danny.el --- Danny (download nanny) package. -*- lexical-binding: t; -*-

;;; Commentary:
;; Danny monitors downloads and offers quick actions.


;;; Code:

(require 'cl)
(require 'dash)
(require 'f)
(require 'filenotify)
(require 'ht)
(require 'ivy)

(defvar danny--notify-descriptor nil "File notify descriptor.")

(defvar danny-monitor-path nil "Path to monitor.")

(defvar danny--history-hash (ht) "History.")

(defvar danny--destination-root "~/Downloads/Targets")

(defvar danny--history-path "~/.danny")

(cl-defstruct
    danny-destination-root
  name
  path)

(defun danny-stop-monitoring ()
  (interactive)
  "Stop monitoring for new files."
  (when (file-notify-valid-p danny--notify-descriptor)
    (file-notify-rm-watch danny--notify-descriptor)
    (setq danny--notify-descriptor nil)))

(defun danny-start-listening ()
  "Start monitoring for new files."
  (interactive)
  (danny-stop-monitoring)
  (setq danny--history-hash (danny--read-history-hashtable danny--history-path))
  (cl-assert danny-monitor-path nil "`danny-monitor-path' must be set")
  (setq danny--notify-descriptor (file-notify-add-watch danny-monitor-path
                                                        '(change attribute-change)
                                                        'danny--notify-callback)))

(defun danny--notify-callback (event)
  "Handle EVENT for file-notify events."
  (let ((event-type (nth 1 event))
        (file-path (nth 2 event)))
    (when (and (eq event-type 'created)
               (f-file-p file-path))
      (danny--handle-file-created file-path))))

(defun danny--create-move-action-fun (file-path)
  (lambda (destination-dir)
    (let ((destination-file (f-join destination-dir
                                    (danny--read-string (format "Save as (%s): " (f-filename file-path))
                                                        (f-filename file-path)
                                                        20))))
      (if (f-exists-p destination-file)
          (progn
            (unless (danny--y-or-n (format "Override? %s\n" destination-file))
              (error "Aborted"))
            (rename-file file-path destination-file t)
            (danny--add-destination-dir destination-dir))
        (rename-file file-path destination-file t)
        (danny--add-destination-dir destination-dir)))))

(defun danny--create-open-action-fun (file-path)
  (lambda (ignored)
    (find-file file-path)))

(defun danny--handle-file-created (file-path)
  "Handle new file created at FILE-PATH."
  (with-current-buffer (get-buffer-create "*danny*")
    (let* ((unwind (lambda ()
                     (delete-frame)
                     (other-window 1))))
      (danny--framed-ivy-read (list
                               (make-danny--framed-ivy-source :prompt (format "Open? (%s) " (f-filename file-path))
                                                              :action (danny--create-open-action-fun file-path)
                                                              :collection (lambda (str pred v)
                                                                            '())
                                                              :unwind unwind)
                               (make-danny--framed-ivy-source :prompt (format "Save in Recent (%s): " (f-filename file-path))
                                                              :action (danny--create-move-action-fun file-path)
                                                              :collection (lambda (str pred v)
                                                                            (danny--last-destinations))
                                                              :unwind unwind)
                               (make-danny--framed-ivy-source :prompt (format "Save in Downloads (%s): " (f-filename file-path))
                                                              :action (danny--create-move-action-fun file-path)
                                                              :collection (lambda (str pred v)
                                                                            (f-directories danny--destination-root))
                                                              :unwind unwind))))))

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
    (with-current-buffer (get-buffer-create "*danny*")
      (let* ((lines-count (length (funcall (danny--framed-ivy-source-collection source)
                                           nil nil nil)))
             (frame (make-frame `((auto-raise . t)
                                  (font . "Menlo 15")
                                  (height . ,(if (> lines-count 0) 10
                                               1))
                                  (internal-border-width . 20)
                                  (left . 0.33)
                                  (left-fringe . 0)
                                  (line-spacing . 3)
                                  (menu-bar-lines . 0)
                                  (minibuffer . only)
                                  (right-fringe . 0)
                                  (tool-bar-lines . 0)
                                  (top . 200)
                                  (undecorated . t)
                                  (unsplittable . t)
                                  (vertical-scroll-bars . nil)
                                  ;; Calculate a sensible width, based on longest path (in source).
                                  (width . ,(if (> lines-count 0)
                                                (+ 1
                                                   lines-count
                                                   (danny--longest-line-length (funcall (danny--framed-ivy-source-collection source)
                                                                                        nil nil nil)))
                                              (1+ (length (danny--framed-ivy-source-prompt source)))))))))
        (ivy-read (danny--framed-ivy-source-prompt source)
                  (danny--framed-ivy-source-collection source)
                  :update-fn (lambda ()
                               ;; Forcing redisplay works around "Open" source not shown
                               ;; after having visited other sources (left/right keys).
                               (redisplay))
                  :action (danny--framed-ivy-source-action source)
                  :initial-input initial-input
                  :unwind (danny--framed-ivy-source-unwind source)
                  :keymap kmap)))))

(defun danny--read-string (prompt default width)
  (with-current-buffer (get-buffer-create "*danny*")
    (let* ((input)
           (lines (s-split "\n" prompt))
           (frame (make-frame `((auto-raise . t)
                                (font . "Menlo 15")
                                (height . ,(length lines))
                                (internal-border-width . 10)
                                (left . 0.33)
                                (left-fringe . 0)
                                (line-spacing . 3)
                                (menu-bar-lines . 0)
                                (minibuffer . only)
                                (right-fringe . 0)
                                (tool-bar-lines . 0)
                                (top . 200)
                                (undecorated . t)
                                (unsplittable . t)
                                (vertical-scroll-bars . nil)
                                (width . ,(+ 1 width (danny--longest-line-length lines)))))))
      (setq input (read-string prompt))
      (delete-frame)
      (other-window 1)
      (if (> (length input) 0)
          input
        default))))

(defun danny--y-or-n (prompt)
  "Frame-based yes or no dialog with PROMPT."
  (with-current-buffer (get-buffer-create "*danny*")
    (let* ((input)
           (lines (s-split "\n" prompt))
           (height (length lines))
           (width 0)
           (frame (make-frame `((auto-raise . t)
                                (font . "Menlo 15")
                                (height . ,height)
                                (internal-border-width . 10)
                                (left . 0.33)
                                (left-fringe . 0)
                                (line-spacing . 3)
                                (menu-bar-lines . 0)
                                (minibuffer . only)
                                (right-fringe . 0)
                                (tool-bar-lines . 0)
                                (top . 200)
                                (undecorated . t)
                                (unsplittable . t)
                                (vertical-scroll-bars . nil)
                                (width . ,(+ 1 (danny--longest-line-length lines)))))))
      (setq input (yes-or-no-p prompt))
      (delete-frame)
      (other-window 1)
      input)))

(defun danny--longest-line-length (lines)
  "Return the longest length in LINES."
  (let ((longest 0))
    (mapc (lambda (line)
            (when (> (length line) longest)
              (setq longest (length line))))
          lines)
    longest))

(defun danny--write-history-hashtable (path hashtable)
  "Write history HASHTABLE in PATH."
  (with-temp-buffer
    (prin1 hashtable (current-buffer))
    (write-file path nil)))

(defun danny--read-history-hashtable (path)
  "Read history hash in PATH."
  (if (not (f-exists? path))
      (ht-create)
    (with-temp-buffer
      (insert-file-contents path)
      (read (current-buffer)))))

(defun danny--last-destinations ()
  "Return a list of recent destinations."
  (ht-get danny--history-hash "last-destinations" '()))

(defun danny--add-destination-dir (dir)
  "Return a recent DIR to history.."
  (ht-set danny--history-hash
          "last-destinations"
          (-distinct (-concat (ht-get danny--history-hash
                                      "last-destinations" '()) (list dir))))
  (danny--write-history-hashtable danny--history-path danny--history-hash)
  danny--history-hash)

(provide 'danny)

;;; danny.el ends here
