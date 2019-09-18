;; Remember scratch content across sessions.
(use-package persistent-scratch
  :ensure t
  :config
  (persistent-scratch-setup-default))

;; Re-open scratch if killed.
(use-package immortal-scratch
  :ensure t
  :after persistent-scratch
  :custom (immortal-scratch-switch-to-respawned-scratch t)
  :config
  (defun ar/immortal-scratch-respawn-advice (orig-fun &rest r)
    "Advice function around `immortal-scratch-respawn' (ORIG-FUN and R)."
    (let ((initial-scratch-message
           ;; `persistent-scratch-save-file' content looks as follows:
           ;; (["*scratch*" "This is the actual content" (24) fundamental-mode nil nil])
           ;; Need to unpack actual content  with nth and aref.
           (aref
            (nth 0
                 (read
                  (with-temp-buffer
                    (let ((coding-system-for-read 'utf-8-unix))
                      (insert-file-contents persistent-scratch-save-file))
                    (buffer-string)))) 1)))
      ;; Now that `initial-scratch-message' is momentarily set,
      ;; invoke `immortal-scratch-respawn'
      (apply orig-fun r)
      ;; Scroll to top.
      (with-current-buffer (get-buffer-create "*scratch*")
        (goto-char (point-min)))))

  ;; If `initial-scratch-message' is nil, immortal-scratch-respawn breaks.
  ;; Temporarily let-set `initial-scratch-message' in adviced function
  ;; using persistent-scratch's value.
  (advice-add #'immortal-scratch-respawn
              :around
              'ar/immortal-scratch-respawn-advice)
  (immortal-scratch-mode))
