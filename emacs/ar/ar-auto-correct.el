;;; ar-auto-correct.el --- Auto-correct functionality

;;; Commentary:
;; Auto-correct helpers.


;;; Code:

(require 'abbrev)
(require 'ispell)

;; From http://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html
(defun ar/auto-correct-ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global."
  (interactive "P")
  (let (bef aft)
    (save-excursion
      (while (progn
               (backward-word)
               (and (setq bef (thing-at-point 'word))
                    (not (ispell-word nil 'quiet)))))
      (setq aft (thing-at-point 'word)))
    (when (and aft bef (not (equal aft bef)))
      (setq aft (downcase aft))
      (setq bef (downcase bef))
      (define-abbrev
        (if p local-abbrev-table global-abbrev-table)
        bef aft)
      (message "\"%s\" now expands to \"%s\" %sally"
               bef aft (if p "loc" "glob")))))


(provide 'ar-auto-correct)

;;; ar-auto-correct.el ends here
