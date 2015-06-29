;;; ar-time.el --- Time support.

;;; Commentary:
;; Time helpers.


;;; Code:

(defun ar/time-range-to-week-string (range)
  "Convert RANGE to a week org headnline.
For example: ((21868 54482 769103 0) (21874 6866 769103 0)) =>
             <2015-06-01 Mon>--<2015-06-05 Fri>"
  (concat
   (format-time-string "<%Y-%m-%d %a>--" (nth 0 range))
   (format-time-string "<%Y-%m-%d %a>" (nth 1 range))))

(defun ar/time-week-string-to-range (string)
  "Parse STRING formatted as <2015-06-01 Mon>--<2015-06-05 Fri>.
For example: ((21867 41088) (21872 59008))."
  (when (string-match "<\\([[:digit:]]+\\)-\\([[:digit:]]+\\)-\\([[:digit:]]+\\).+>--<\\([[:digit:]]+\\)-\\([[:digit:]]+\\)-\\([[:digit:]]+\\).+>"
                      string)
    (list (encode-time 0 0 0
                       (string-to-number (match-string 3 string))
                       (string-to-number (match-string 2 string))
                       (string-to-number (match-string 1 string))
                       0 0 0 )
          (encode-time 0 0 0
                       (string-to-number (match-string 6 string))
                       (string-to-number (match-string 5 string))
                       (string-to-number (match-string 4 string))
                       0 0 0 ))))

(defun ar/time-current-work-week-string ()
  "Return the current work week string.
For example: <2015-06-01 Mon>--<2015-06-05 Fri>."
  (ar/time-range-to-week-string (ar/time-current-work-week-range)))

(defun ar/time-current-work-week-range ()
  "Return the current work week range.
For example: ((21868 53857 429468 0) (21874 6241 429468 0))."
  (let* ((floor nil)
         (ceiling nil)
         (date (current-time))
         (weekday (nth 6 (decode-time date))))
    ;; 1: Monday 5: Friday.
    (when (or (< weekday 1) (> weekday 5))
      (error "Today is not a week day"))
    (list (ar/time-increment-by-days date (- (- weekday 1)))
          (ar/time-increment-by-days date (- 5 weekday)))))

(defun ar/time-between-p (time floor ceiling)
  "Return non nil if TIME between FLOOR and CEILING."
  (cond ((not time) (error "Invalid TIME"))
        ((not floor) (error "Invalid FLOOR"))
        ((not ceiling) (error "Invalid CEILING"))
        (t (and (time-less-p time ceiling)
                (time-less-p floor time)))))

(defun ar/time-increment-by-days (time days)
  "Increment TIME by number of DAYS."
  (time-add time (days-to-time days)))

(defun ar/time-weekday-p (time)
  "Return non nil if TIME is week day."
  (let ((weekday (nth 6 (decode-time time))))
    (and (> weekday 0) (< weekday 6))))

(defun ar/time-prompt-ret ()
  "Insert a new line and time stamp. For example:

14:07:14 >

May want to bind to RET. For example:

(define-key org-mode-map (kbd "RET") 'ar/time-prompt-ret)
"
  (interactive)
  (insert (format-time-string "\n%H:%M:%S > "
                              (current-time))))

(provide 'ar-time)

;;; ar-time.el ends here
