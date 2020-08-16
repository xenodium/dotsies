;;; -*- lexical-binding: t; -*-

(use-package org-super-agenda
  :ensure t
  :validate-custom
  (org-super-agenda-groups
   '((:name "Category: habit"
            :category "habit")
     (:name "Category: inbox"
            :category "inbox")
     (:name ""
            :auto-category t)))
  :config
  (org-super-agenda-mode +1))

(with-eval-after-load 'fullframe
  (fullframe org-agenda-mode
             org-agenda-quit))

;; A little formatting of agenda view.
;;   Tuesday     1 October 2019
;; ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░ DONE High Pass the salt.
;; (let ((spaces (make-string 32 (string-to-char "░"))))
;;   (map-put org-agenda-prefix-format 'agenda (concat spaces " ")))
;; (map-put org-agenda-prefix-format 'todo " %i %-31:c")
(map-put org-agenda-prefix-format 'agenda "    % s")
(map-put org-agenda-prefix-format 'todo "    ")

;; https://pages.sachachua.com/.emacs.d/Sacha.html#org32b4908
(defun ar/org-agenda-done (&optional arg)
  "Mark current TODO as done.
This changes the line at point, all other lines in the agenda referring to
the same tree node, and the headline of the tree node in the Org-mode file."
  (interactive "P")
  (org-agenda-todo "DONE"))

;; https://pages.sachachua.com/.emacs.d/Sacha.html#orgade1aa9
(defun ar/org-agenda-mark-done-and-add-followup ()
  "Mark the current TODO as done and add another task after it.
Creates it at the same level as the previous task, so it's better to use
this with to-do items than with projects or headings."
  (interactive)
  (org-agenda-todo "DONE")
  (org-agenda-switch-to)
  (org-capture 0 "t"))

(defun ar/org-agenda-todo-previous-keyword ()
  "Transition current task to previous state in `org-todo-keywords'"
  (interactive)
  (let ((org-inhibit-logging t)) ;; Do not ask for notes when transitioning states.
    (org-agenda-todo 'left)))

(defun ar/org-agenda-todo-next-keyword ()
  "Transition current task to next state in `org-todo-keywords'"
  (interactive)
  (let ((org-inhibit-logging t)) ;; Do not ask for notes when transitioning states.
    (org-agenda-todo 'right)))

(defun ar/org-agenda-item-move-up ()
  "Move the current agenda item up."
  (interactive)
  (unless (ignore-errors
            (org-save-all-org-buffers)
            (org-agenda-switch-to)
            (org-metaup)
            (switch-to-buffer (other-buffer (current-buffer) 1))
            (org-agenda-redo)
            (org-agenda-previous-line))
    ;; Something err'd. Switch back to agenda anyway.
    (switch-to-buffer (other-buffer (current-buffer) 1))))

(defun ar/org-agenda-item-move-down ()
  "Move the current agenda item down."
  (interactive)
  (unless (ignore-errors
            (org-save-all-org-buffers)
            (org-agenda-switch-to)
            (org-metadown)
            (switch-to-buffer (other-buffer (current-buffer) 1))
            (org-agenda-redo)
            (org-agenda-next-line))
    ;; Something err'd. Switch back to agenda anyway.
    (switch-to-buffer (other-buffer (current-buffer) 1))))

;; From http://pragmaticemacs.com/emacs/reorder-todo-items-in-your-org-mode-agenda/
(defun ar/org-headline-to-top ()
  "Move the current org headline to the top of its section"
  (interactive)
  ;; check if we are at the top level
  (let ((lvl (org-current-level)))
    (cond
     ;; above all headlines so nothing to do
     ((not lvl)
      (message "No headline to move"))
     ((= lvl 1)
      ;; if at top level move current tree to go above first headline
      (org-cut-subtree)
      (beginning-of-buffer)
      ;; test if point is now at the first headline and if not then
      ;; move to the first headline
      (unless (looking-at-p "*")
        (org-next-visible-heading 1))
      (org-paste-subtree))
     ((> lvl 1)
      ;; if not at top level then get position of headline level above
      ;; current section and refile to that position. Inspired by
      ;; https://gist.github.com/alphapapa/2cd1f1fc6accff01fec06946844ef5a5
      (let* ((org-reverse-note-order t)
             (pos (save-excursion
                    (outline-up-heading 1)
                    (point)))
             (filename (buffer-file-name))
             (rfloc (list nil filename nil pos)))
        (org-refile nil nil rfloc))))))

(defun ar/org-agenda-item-to-top ()
  "Move the current agenda item to the top of the subtree in its file"
  (interactive)
  (org-save-all-org-buffers)
  (org-agenda-switch-to)
  (ar/org-headline-to-top)
  (switch-to-buffer (other-buffer (current-buffer) 1))
  (org-agenda-redo))

;; From https://blog.aaronbieber.com/2016/09/25/agenda-interactions-primer.html
(defun ar/org-agenda-capture (&optional vanilla)
  "Capture a task in agenda mode, using the date at point.

If VANILLA is non-nil, run the standard `org-capture'."
  (interactive "P")
  (if vanilla
      (org-capture)
    (let ((org-overriding-default-time (org-get-cursor-date)))
      (org-capture nil "t"))))

(defun ar/org-agenda-next-header ()
  "Jump to the next header in an agenda series."
  (interactive)
  (ar/org-agenda--goto-header))

(defun ar/org-agenda-previous-header ()
  "Jump to the previous header in an agenda series."
  (interactive)
  (ar/org-agenda--goto-header t))

(defun ar/org-agenda--goto-header (&optional backwards)
  "Find the next agenda series header forwards or BACKWARDS."
  (let ((pos (save-excursion
               (goto-char (if backwards
                              (line-beginning-position)
                            (line-end-position)))
               (let* ((find-func (if backwards
                                     'previous-single-property-change
                                   'next-single-property-change))
                      (end-func (if backwards
                                    'max
                                  'min))
                      (all-pos-raw (list (funcall find-func (point) 'org-agenda-structural-header)
                                         (funcall find-func (point) 'org-agenda-date-header)))
                      (all-pos (cl-remove-if-not 'numberp all-pos-raw))
                      (prop-pos (if all-pos (apply end-func all-pos) nil)))
                 prop-pos))))
    (if pos (goto-char pos))
    (if backwards (goto-char (line-beginning-position)))))

(defun ar/org-agenda-toggle (&optional arg)
  "Toggles between agenda using my custom command and org file."
  (interactive "P")
  (if (eq major-mode 'org-agenda-mode)
      (find-file "~/stuff/active/agenda.org")
    (let ((agenda-buffer (get-buffer "*Org Agenda*")))
      (if agenda-buffer
          ;; Switching buffer doesn't reset point location.
          (switch-to-buffer agenda-buffer)
        (org-agenda arg "c")))))

;; `ar/org-agenda-schedule-dwim' `ar/org-agenda-validate-marked-entries' and
;; `ar/org-agenda-bulk-action' are mostly lifted from org-agenda.el.
;; The dwim methods behave more like dired bulk commands: if there's a selection
;; operate on all items, otherwise operate on current point.
(defun ar/org-agenda-schedule-dwim (&optional arg)
  (interactive "P")
  (ar/org-agenda-validate-marked-entries)
  (let ((time (and (not arg)
                   (let ((new (org-read-date
                               nil nil nil "(Re)Schedule to" org-overriding-default-time)))
                     ;; A "double plus" answer applies to every
                     ;; scheduled time.  Do not turn it into
                     ;; a fixed date yet.
                     (if (string-match-p "\\`[ \t]*\\+\\+"
                                         org-read-date-final-answer)
                         org-read-date-final-answer
                       new)))))
    (ar/org-agenda-bulk-action (lambda ()
                                 (org-agenda-schedule arg time)))))

;; (defun ar/org-agenda-todo-dwim (&optional arg)
;;   (interactive "P")
;;   (org-fast-todo-selection) (completing-read
;;                                           "Todo state: "
;;                                           org-todo-keywords-1)
;;   (let ((state (org-fast-todo-selection))
;;         (org-inhibit-blocking t)
;;         (org-inhibit-logging 'note))
;;     (ar/org-agenda-bulk-action (lambda ()
;;                                  (org-agenda-todo state)))))

(defun ar/org-agenda-validate-marked-entries ()
  "Ensure all marked org agenda entries in selection are valid."
  (dolist (m org-agenda-bulk-marked-entries)
    (unless (and (markerp m)
                 (marker-buffer m)
                 (buffer-live-p (marker-buffer m))
                 (marker-position m))
      (user-error "Marker %s for bulk command is invalid" m))))

(defun ar/org-agenda-bulk-action (cmd)
  "Apply CMD to `org-agenda-bulk-marked-entries'."
  (if org-agenda-bulk-marked-entries
      (progn
        ;; Loop over all markers and apply bulk command.
        (let ((processed 0)
              (skipped 0)
              ;; Sort the markers, to make sure that parents are handled
              ;; before children.
              (entries (sort org-agenda-bulk-marked-entries
                             (lambda (a b)
                               (cond
                                ((eq (marker-buffer a) (marker-buffer b))
                                 (< (marker-position a) (marker-position b)))
                                (t
                                 (string< (buffer-name (marker-buffer a))
                                          (buffer-name (marker-buffer b))))))))
              redo-at-end)
          (dolist (e entries)
            (let ((pos (text-property-any (point-min) (point-max) 'org-hd-marker e)))
              (if (not pos)
                  (progn (message "Skipping removed entry at %s" e)
                         (cl-incf skipped))
                (goto-char pos)
                (let (org-loop-over-headlines-in-active-region)
                  (funcall cmd))
                ;; `post-command-hook' is not run yet.  We make sure any
                ;; pending log note is processed.
                (when (or (memq 'org-add-log-note (default-value 'post-command-hook))
                          (memq 'org-add-log-note post-command-hook))
                  (org-add-log-note))
                (cl-incf processed))))
          (when redo-at-end (org-agenda-redo))
          (unless org-agenda-persistent-marks (org-agenda-bulk-unmark-all))
          (message "Acted on %d entries%s%s"
                   processed
                   (if (= skipped 0)
                       ""
                     (format ", skipped %d (disappeared before their turn)"
                             skipped))
                   (if (not org-agenda-persistent-marks) "" " (kept marked)"))))
    (funcall cmd)))

;; Automatically save org files after transitioning tasks.
(advice-add 'org-agenda-todo :after 'org-save-all-org-buffers)
