;;; os-present.el --- Presents non-decorated frames.
;;; -*- lexical-binding: t; -*-


;;; Commentary:
;; Use `os-present' to present a non-decorated frame.

(require 'org-element)

;;; Code:

(defmacro os-present (buffer-name minibuffer-only sync-body &rest body)
  "Create a buffer with BUFFER-NAME and eval BODY in a basic frame."
  (declare (indent 1) (debug t))
  ;; TODO: Consider using (get-buffer-create "*present*") as default.
  `(let* ((buffer (get-buffer-create ,buffer-name))
          (options (append '((auto-raise . t)
                             (font . "Menlo 15")
                             (top . 200)
                             (height . 20)
                             (width . 110)
                             (internal-border-width . 20)
                             (left . 0.33)
                             (left-fringe . 0)
                             (line-spacing . 3)
                             (menu-bar-lines . 0)
                             (right-fringe . 0)
                             (tool-bar-lines . 0)
                             (undecorated . t)
                             (unsplittable . t)
                             (vertical-scroll-bars . nil))
                           (when ,minibuffer-only
                             '((minibuffer . only)))))
          (frame (make-frame options)))
     (set-face-attribute 'ivy-current-match frame
                         :background "#2a2a2a"
                         :foreground 'unspecified)
     (select-frame frame)
     (select-frame-set-input-focus frame)
     (switch-to-buffer buffer)
     (with-selected-frame frame
       (switch-to-buffer buffer))
     (if ,sync-body
         (progn
           (when (null ',body)
             (user-error "Must have a body to execute as sync."))
           (with-current-buffer buffer
             (condition-case nil
                 (unwind-protect
                     ,@body
                   (delete-frame frame)
                   (kill-buffer buffer))
               (quit (delete-frame frame)
                     (kill-buffer buffer)))
             ))
       ,@body)))

(defun os-present-clipboard-manager ()
  "Search all my org links."
  (os-present "*OS Copy*" t t
              (let ((selection (completing-read "Copy: " kill-ring nil t))
                    (select-enable-clipboard t))
                (call-process "/Applications/Hammerspoon.app/Contents/Frameworks/hs/hs"
                              nil 0 nil
                              "-c"
                              (format "backFromEmacs();
                             hs.pasteboard.setContents(\"%s\");" selection)))))

(defun os-present-my-org-links ()
  "Search all my org links."
  (os-present "*OS bookmarks*" t t
              (let ((selection (completing-read "Open bookmark: "
                                                (if ar/modal-ivy--bookmarks-source
                                                    ar/modal-ivy--bookmarks-source
                                                  (setq ar/modal-ivy--bookmarks-source
                                                        (ar/modal-ivy--load-bookmarks-source))
                                                  ar/modal-ivy--bookmarks-source)
                                                nil t))
                    (select-enable-clipboard t))
                (browse-url (get-text-property 0 'url selection)))))

(defun os-present-org-links (file-path)
  "Load org from FILE-PATH and return all links."
  (with-temp-buffer
    (insert-file-contents file-path)
    (org-mode)
    (let (links)
      (org-element-map (org-element-parse-buffer) 'link
        (lambda (link)
          (let* ((link-type (org-element-property :type link))
                 (raw-link (org-element-property :raw-link link))
                 (content (org-element-contents link))
                 ;; TODO: Text like "*scratch*" breaks title. Handle it.
                 (title (substring-no-properties (or (seq-first content) raw-link))))
            (when (or (string-equal link-type "https") (string-equal link-type "http"))
              (push (concat (propertize title 'url raw-link)
                            (propertize (format "\n%s\n" raw-link)
                                        'face 'whitespace-space))
                    links))))
        nil nil 'link)
      (prin1 (seq-sort 'string-greaterp links))
      (seq-sort 'string-greaterp links)
      )))

(defun os-present-chatgpt-compose (&optional content-path)
  "Search all my org links."
  (let* ((display-buffer-alist)
         (buffer (chatgpt-shell-prompt-compose-show-buffer
                  (when content-path
                    (with-temp-buffer
                      (insert-file-contents content-path)
                      (delete-file content-path)
                      (buffer-string)))
                  t t)))
    (with-current-buffer buffer
      (setq mode-line-format '("%b")))
    (os-present buffer nil nil (progn))))

(provide 'os-present)

;;; os-present.el ends here
