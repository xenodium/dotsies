;;; ar-hammerspoon-org-modal.el --- Hammerspoon org modal editing support.

;;; Commentary:
;; Hammerspoon org modal editing mode.


;;; Code:

(require 'org)
(require 'ar-org)

(defvar ar/hammerspoon-org-modal--modeline-format nil)
(put 'ar/hammerspoon-org-modal--modeline-format 'permanent-local t)

(defun ar/hammerspoon-org-modal-save-and-exit ()
  "Save the current org buffer and return focus to the calling macOS app."
  (interactive)
  (setq mode-line-format ar/hammerspoon-org-modal--modeline-format)
  (call-interactively #'save-buffer)
  (ar/hammerspoon-org-modal-mode -1)
  ;; Contrary to other macOS apps, Emacs shifts focus to next frame, after deleting current frame.
  ;; Work around by moving focus to other macOS app first and then deleting frame.
  (shell-command "/Applications/Hammerspoon.app/Contents/Frameworks/hs/hs -c 'backFromEmacs()'")
  (delete-frame)
  (message "Saved %s" (ar/org-get-daily-file-path)))

(defun ar/hammerspoon-org-modal-add-todo ()
  "Go to tasks INBOX and add empty TODO."
  ;; Method called using "emacsclient -ne" so need to use (window-buffer (selected-window))
  ;; so we don't get *server* buffer.
  (with-current-buffer (window-buffer (selected-window))
    (new-frame '((left-fringe . 0)
                 (right-fringe . 0)
                 (name . "New TODO")
                 (undecorated . t)
                 (vertical-scroll-bars . nil)
                 (minibuffer . nil)
                 (unsplittable . t)
                 (user-position . t)
                 (top . 75)
                 (height . 20)
                 (left . 0.33)
                 (tool-bar-lines . 0)
                 (menu-bar-lines . 0)))
    (find-file (ar/org-get-daily-file-path))
    (ar/org-goto-file (ar/org-get-daily-file-path) "inbox")
    (when mode-line-format
      (setq ar/hammerspoon-org-modal--modeline-format mode-line-format)
      (setq mode-line-format nil))
    (ar/hammerspoon-org-modal-mode +1)
    (org-narrow-to-subtree)
    (org-show-subtree)
    (org-end-of-meta-data t)
    (org-insert-heading)
    (insert "TODO ")
    (widen)))

(define-minor-mode ar/hammerspoon-org-modal-mode
  "When enabled `ar/hammerspoon-org-modal-save-and-exit' is bound to C-c C-c."
  :init-value nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") #'ar/hammerspoon-org-modal-save-and-exit)
            map)
  :lighter " hammerspon org modal edit"
  (assert (eq major-mode 'org-mode) nil "Not in org-mode"))

(provide 'ar-hammerspoon-org-modal)

;;; ar-hammerspoon-org-modal.el ends here
