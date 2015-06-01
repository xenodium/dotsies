;;; ar-text.el --- Text support.

;;; Commentary:
;; Text helpers.


;;; Code:

(defun ar/text-char-upcasep (letter)
  "Check if LETTER is uppercase."
  (eq letter (upcase letter)))

;;  http://oremacs.com/2014/12/25/ode-to-toggle
(defun ar/text-capitalize-word-toggle ()
  "Capitalize word toggle."
  (interactive)
  (let ((start
         (car
          (bounds-of-thing-at-point 'symbol))))
    (if start
        (save-excursion
          (goto-char start)
          (funcall
           (if (ar/text-char-upcasep (char-after))
               'downcase-region
             'upcase-region)
           start (1+ start)))
      (capitalize-word -1))))

(defun ar/text-upcase-word-toggle ()
  "Toggle word case at point."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'symbol))
        beg end
        (regionp
         (if (eq this-command last-command)
             (get this-command 'regionp)
           (put this-command 'regionp nil))))
    (cond
     ((or (region-active-p) regionp)
      (setq beg (region-beginning)
            end (region-end))
      (put this-command 'regionp t))
     (bounds
      (setq beg (car bounds)
            end (cdr bounds)))
     (t
      (setq beg (point)
            end (1+ beg))))
    (save-excursion
      (goto-char (1- beg))
      (and (re-search-forward "[A-Za-z]" end t)
           (funcall (if (ar/text-char-upcasep (char-before))
                        'downcase-region
                      'upcase-region)
                    beg end)))))

(provide 'ar-text)

;;; ar-text.el ends here
