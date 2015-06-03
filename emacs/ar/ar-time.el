;;; ar-time.el --- Time support.

;;; Commentary:
;; Time helpers.


;;; Code:

(defun ar/time-between-p (time floor ceiling)
  "Return non nil if TIME between FLOOR and CEILING."
  (cond ((not time) nil)
        ((not floor) nil)
        ((not ceiling) nil)
        (t (and (time-less-p time ceiling)
                (time-less-p floor time)))))

(defun ar/time-increment-by-days (time days)
  "Increment TIME by number of DAYS."
  (time-add time (days-to-time days)))

(defun ar/time-weekday-p (time)
  "Return non nil if TIME is week day."
  (let ((weekday (nth 6 (decode-time time))))
    (and (> weekday 0) (< weekday 6))))

(provide 'ar-time)

;;; ar-time.el ends here
