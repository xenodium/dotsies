;;; ar-hammerspoon-org-modal.el --- Hammerspoon org modal editing support.

;;; Commentary:
;; Hammerspoon org modal editing mode.


;;; Code:

(require 'org)
(require 'ar-org)

(defvar ar/hammerspoon-org-modal--marker)

(defun ar/hammerspoon-org-modal-save-and-exit ()
  "Save the current org buffer and return focus to the calling macOS app."
  (interactive)
  (save-current-buffer)
  (ar/hammerspoon-org-modal-mode -1)
  ;; goto-char didn't work, but switching buffer is enough to return to maker.
  (switch-to-buffer (marker-buffer ar/hammerspoon-org-modal--marker))
  (shell-command "/Applications/Hammerspoon.app/Contents/Resources/extensions/hs/ipc/bin/hs -c 'backFromEmacsOrgEdit()'"))

(defun ar/hammerspoon-org-modal-add-todo ()
  "Go to tasks INBOX and add empty TODO."
  ;; Method called using "emacsclient -ne" so need to use (window-buffer (selected-window))
  ;; so we don't get *server* buffer.
  (with-current-buffer (window-buffer (selected-window))
    (let ((marker (copy-marker (point))))
      (find-file (ar/org-get-daily-file-path))
      (ar/org-goto-file (ar/org-get-daily-file-path) "inbox")
      (ar/hammerspoon-org-modal-mode +1)
      (setq ar/hammerspoon-org-modal--marker marker)
      (org-narrow-to-subtree)
      (org-show-subtree)
      (org-end-of-meta-data t)
      (org-insert-heading)
      (insert "TODO ")
      (widen))))

(define-minor-mode ar/hammerspoon-org-modal-mode
  "When enabled `ar/hammerspoon-org-modal-save-and-exit' is bound to C-c C-c."
  :init-value nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") #'ar/hammerspoon-org-modal-save-and-exit)
            map)
  :lighter " hammerspon org modal edit"
  (assert (eq major-mode 'org-mode) nil "Not in org-mode")
  (make-local-variable 'ar/hammerspoon-org-modal--marker))

(provide 'ar-hammerspoon-org-modal)

;;; ar-hammerspoon-org-modal.el ends here
