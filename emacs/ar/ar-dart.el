(require 'cl)
(require 'json)
(require 's)

(defvar ar/dart-executable-path nil)
(defvar ar/dart-analysis-server-snapshot-path nil)
(defvar ar/dart-sdk-path nil)

(defvar ar/dart--request-id 0)
(defvar ar/dart--buffered-json nil)
(defvar ar/dart--debug nil)

(defun ar/dart--get-next-request-id ()
  (setq ar/dart--request-id (1+ ar/dart--request-id))
  (format "request-%s" ar/dart--request-id))

(defun ar/dart--assert-path-var (path var-name)
  (assert (stringp path) nil (format "Path unset. Set %s" var-name))
  (assert (file-exists-p path) nil (format "%s not found. Set %s." path var-name)))

(defun ar/dart--handle-response (response)
  (let-alist response
    (when .id
      ;; TODO: remove message.
      (message "response %s" .id)
      (when .result
        ;; TODO: remove message and print.
        (message "with result:")
        (print .result))
      t)))

(defun ar/dart--handle-event-as-fallback (event)
  (let-alist event
    ;; TODO: remove message.
    (message "ignoring: %s" .event)))

(defun ar/dart--handle-event (event)
  (let-alist event
    (when .event
      (cond ((ar/dart--handle-error-event event))
            ((ar/dart--handle-completion-results-event event))
            ((ar/dart--handle-event-as-fallback event)))
      t)))

(defun ar/dart--handle-error (error)
  ;; TODO: validate error object.
  (let-alist error
    ;; TODO: remove message.
    (message "%s:%s %s" .location.file .location.offset .message)))

(defun ar/dart--handle-completion-results-event (completion-results-event)
  (let-alist completion-results-event
    ;; TODO: validate results event object.
    (when (string-equal .event "completion.results")
      (print completion-results-event)
      t)))

(defun ar/dart--handle-error-event (error-event)
  (let-alist error-event
    ;; TODO: validate error event object.
    (when (string-equal .event "analysis.errors")
      (if (ar/dart--list-empty-p .params.errors)
          ;; TODO: remove message.
          (message "life is good :)")
        (mapc (lambda (error)
                (ar/dart--handle-error error))
              .params.errors))
      t)))

(defun ar/dart--list-empty-p (list)
  ;; TODO: Assert is list.
  (= (length .params.errors) 0))

(defun ar/dart--log-object-as-json (object)
  (when ar/dart--debug (let ((json-encoding-pretty-print t))
                         (message "===")
                         (message (json-encode object)))))

(defun ar/dart--buffered-filter (process chunk)
  (let ((end-of-buffer (string-match-p "}\n$" chunk))
        (server-message))
    (setq ar/dart--buffered-json (concat ar/dart--buffered-json chunk))
    (when end-of-buffer
      (setq server-message (json-read-from-string ar/dart--buffered-json))
      ;; (ar/dart--log-object-as-json response)
      (cond ((ar/dart--handle-response server-message))
            ((ar/dart--handle-event server-message)))
      (setq ar/dart--buffered-json nil))))

(defun ar/dart--start-dart-analysis-server-process ()
  (ar/dart--assert-path-var ar/dart-executable-path
                            'ar/dart-executable-path)
  (ar/dart--assert-path-var ar/dart-analysis-server-snapshot-path
                            'ar/dart-analysis-server-snapshot-path)
  (ar/dart--assert-path-var ar/dart-sdk-path
                            'ar/dart-sdk-path)
  (let ((process (get-process "analysis_server.dart.snapshot")))
    (unless process
      (setq process
            (start-process "analysis_server.dart.snapshot"
                           "*dart analysis*"
                           ar/dart-executable-path
                           ar/dart-analysis-server-snapshot-path
                           (concat "--sdk=" ar/dart-sdk-path)))
      (buffer-disable-undo (get-buffer "*dart analysis*"))
      (set-process-filter process #'ar/dart--buffered-filter)
      (set-process-query-on-exit-flag process nil))
    (assert process nil (format "Unable to start analysis_server.dart.snapshot at: %s"
                                ar/dart-analysis-server-snapshot-path))
    process))

(defun ar/dart--to-compact-one-liner (string)
  (assert (string-or-null-p string) nil "Cannot compact non string")
  (s-replace-all '(("\n" . "")) string))

(defun ar/dart--set-analysis-root-path (root-path)
  (let ((set-analysis-root-path-request
         (format "
{
    \"id\": \"%s\",
    \"method\": \"analysis.setAnalysisRoots\",
    \"params\": {
        \"included\": [\"%s\"],
        \"excluded\": []
     }
}"
                 (ar/dart--get-next-request-id) root-path)))
    (ar/dart--send-request set-analysis-root-path-request)))

(defun ar/dart--get-server-version ()
  (let ((get-server-version-request
         (format "
{
    \"id\": \"%s\",
    \"method\": \"server.getVersion\"
}"
                 (ar/dart--get-next-request-id))))
    (ar/dart--send-request get-server-version-request)))

(defun ar/dart--create-add-content-overlay (content)
  (format "
{
    \"type\": \"add\",
    \"content\": %s
}"
          (json-encode-string content)))

(defun ar/dart--create-change-content-overlay (offset length replacement)
  (format "
{
    \"type\": \"change\",
    \"edits\": [
        {
            \"offset\": %s,
            \"length\": %s,
            \"replacement\": %s
        }
     ]
}"
          offset length (json-encode-string replacement)))

(defun ar/dart--on-buffer-first-change ()
  (ar/dart--add-content-overlay))

(defun ar/dart--on-buffer-change (change-begin
                                  change-end
                                  replaced-region-length)
  (ar/dart--change-content-overlay change-begin
                                   change-end
                                   replaced-region-length)
  (when (string-equal (buffer-substring change-begin
                                        change-end)
                      ".")
    (ar/dart--complete-file-at-path (buffer-file-name)
                                    change-begin)))

(defun ar/dart-monitor-buffer-changes ()
  (interactive)
  ;; TODO: Is it safe to assume all source is under a lib directory?
  (ar/dart--set-analysis-root-path (file-name-directory (buffer-file-name)))
  (add-hook 'first-change-hook #'ar/dart--on-buffer-first-change t t)
  (add-hook 'after-change-functions #'ar/dart--on-buffer-change t t))

(defun ar/dart--update-content-overlay (file-path
                                        content-overlay-update-type)
  (let ((update-content-request
         (format "
{
    \"id\": \"%s\",
    \"method\": \"analysis.updateContent\",
    \"params\": {
        \"files\": {
            \"%s\": %s
         }
     }
}"
                 (ar/dart--get-next-request-id) file-path content-overlay-update-type)))
    (ar/dart--send-request update-content-request)))

(defun ar/dart--change-content-overlay (change-begin change-end replaced-region-length)
  (with-current-buffer (current-buffer)
    (assert (buffer-file-name) nil "Buffer must have a file bound.")
    (ar/dart--update-content-overlay
     (buffer-file-name)
     (ar/dart--create-change-content-overlay (- change-begin 1)
                                             replaced-region-length
                                             (buffer-substring change-begin
                                                               change-end)))))

(defun ar/dart--add-content-overlay (&optional buffer)
  ;; TODO: Remove interactive?
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (assert (buffer-file-name) nil "Buffer must have a file bound.")
    (ar/dart--update-content-overlay
     (buffer-file-name)
     (ar/dart--create-add-content-overlay (buffer-string)))))

(defun ar/dart--complete-file-at-path (file-path offset)
  (let ((get-suggestions-request
         (format "
{
    \"id\": \"%s\",
    \"method\": \"completion.getSuggestions\",
    \"params\": {
        \"file\": \"%s\",
        \"offset\": %s
     }
}"
                 (ar/dart--get-next-request-id) file-path offset)))
    (ar/dart--send-request get-suggestions-request)))

(defun ar/dart--send-request (request)
  (message (ar/dart--log-object-as-json (json-read-from-string request)))
  (ar/dart--start-dart-analysis-server-process)
  (process-send-string  (get-process "analysis_server.dart.snapshot")
                        (concat (ar/dart--to-compact-one-liner request) "\n")))

;; (ar/dart--start-dart-analysis-server-process)
;; (ar/dart--set-analysis-root-path "~/stuff/active/hello-dart")
;; (ar/dart--complete-file-at-path "~/stuff/active/hello-dart/bin/main.dart" 92)

;; (setq ar/dart-executable-path "~/homebrew/bin/dart")
;; (setq ar/dart-sdk-path "~/homebrew/Cellar/dart/1.13.0/libexec/")
;; (setq ar/dart-analysis-server-snapshot-path "~/homebrew/Cellar/dart/1.13.0/libexec/bin/snapshots/analysis_server.dart.snapshot")
