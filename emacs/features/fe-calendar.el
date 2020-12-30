(use-package calendar
  :commands ar/year-calendar
  :bind (:map calendar-mode-map
              (">" . ar/scroll-year-calendar-forward)
              ("<" . ar/scroll-year-calendar-backward))
  :config
  ;; https://stackoverflow.com/questions/9547912/emacs-calendar-show-more-than-3-months
  (defun ar/year-calendar (&optional year)
    "Generate a one year calendar that can be scrolled by year in each direction.
This is a modification of:  http://homepage3.nifty.com/oatu/emacs/calendar.html
See also: https://stackoverflow.com/questions/9547912/emacs-calendar-show-more-than-3-months"
    (interactive)
    (require 'calendar)
    (let* (
           (current-year (number-to-string (nth 5 (decode-time (current-time)))))
           (month 0)
           (year (if year year (string-to-number (format-time-string "%Y" (current-time))))))
      (switch-to-buffer (get-buffer-create calendar-buffer))
      (when (not (eq major-mode 'calendar-mode))
        (calendar-mode))
      (setq displayed-month month)
      (setq displayed-year year)
      (setq buffer-read-only nil)
      (erase-buffer)
      ;; horizontal rows
      (dotimes (j 4)
        ;; vertical columns
        (dotimes (i 3)
          (calendar-generate-month
           (setq month (+ month 1))
           year
           ;; indentation / spacing between months
           (+ 5 (* 25 i))))
        (goto-char (point-max))
        (insert (make-string (- 10 (count-lines (point-min) (point-max))) ?\n))
        (widen)
        (goto-char (point-max))
        (narrow-to-region (point-max) (point-max)))
      (widen)
      (goto-char (point-min))
      (setq buffer-read-only t)))

  (defun ar/scroll-year-calendar-forward (&optional arg event)
    "Scroll the yearly calendar by year in a forward direction."
    (interactive (list (prefix-numeric-value current-prefix-arg)
                       last-nonmenu-event))
    (unless arg (setq arg 0))
    (save-selected-window
      (if (setq event (event-start event)) (select-window (posn-window event)))
      (unless (zerop arg)
        (let* (
               (year (+ displayed-year arg)))
          (ar/year-calendar year)))
      (goto-char (point-min))
      (run-hooks 'calendar-move-hook)))

  (defun ar/scroll-year-calendar-backward (&optional arg event)
    "Scroll the yearly calendar by year in a backward direction."
    (interactive (list (prefix-numeric-value current-prefix-arg)
                       last-nonmenu-event))
    (ar/scroll-year-calendar-forward (- (or arg 1)) event)))
