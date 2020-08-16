;;; -*- lexical-binding: t; -*-

(defun ar/org-capture-schedule-day-earlier-dwim ()
  (interactive)
  (if (ar/org-capture--special-pos-p)
      (org-schedule (point) (time-add
                             (org-get-scheduled-time (point))
                             -86400))  ;; day in seconds
    (org-metaleft)))

(defun ar/org-capture-schedule-day-later-dwim ()
  (interactive)
  (if (ar/org-capture--special-pos-p)
      (org-schedule (point) (time-add
                             (org-get-scheduled-time (point))
                             86400))  ;; day in seconds
    (org-metaright)))

(defun ar/org-capture--special-pos-p ()
  "Either at beginning of line or next to TODO [#A]."
  (or (looking-back "^")
      (looking-back "TODO\\s-*\\(\\[#[A-Z]\\]\\s-*\\)*")))

(defun ar/org-capture-priority-up-dwim (N)
  "If at special location, increase priority."
  (interactive "p")
  (if (ar/org-capture--special-pos-p)
      (org-priority 'up)
    (org-self-insert-command N)))

(defun ar/org-capture-priority-down-dwim (N)
  "If at special location, decrease priority."
  (interactive "p")
  (if (ar/org-capture--special-pos-p)
      (org-priority 'down)
    (org-self-insert-command N)))

(defun ar/org-capture-todo (&optional vanilla)
  "Capture a todo. If VANILLA is non-nil, prompt the user for task type."
  (interactive "P")
  (if vanilla
      (org-capture)
    (org-capture nil "t")))
