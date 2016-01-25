;;; ar-auto-correct.el --- Auto-correct functionality

;;; Commentary:
;; Auto-correct helpers.


;;; Code:

(require 'abbrev)
(require 'ispell)

;; From http://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html
(defun ar/auto-correct-ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev.  Otherwise it will
be global."
  (interactive "P")
  (let (bef aft)
    (save-excursion
      (while (if (setq bef (thing-at-point 'word))
                 ;; Word was corrected or used quit.
                 (if (ispell-word nil 'quiet)
                     nil ; End the loop.
                   ;; Also end if we reach `bob'.
                   (not (bobp)))
               ;; If there's no word at point, keep looking
               ;; until `bob'.
               (not (bobp)))
        (backward-word))
      (setq aft (thing-at-point 'word)))
    (if (and aft bef (not (equal aft bef)))
        (let ((aft (downcase aft))
              (bef (downcase bef)))
          (define-abbrev
            (if p local-abbrev-table global-abbrev-table)
            bef aft)
          (message "\"%s\" now expands to \"%s\" %sally"
                   bef aft (if p "loc" "glob")))
      (user-error "No typo at or before point"))))

(provide 'ar-auto-correct)

;;; ar-auto-correct.el ends here
