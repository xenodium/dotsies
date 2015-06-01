;;; ar-nxml.el --- XML support.

;;; Commentary:
;; XML helpers.


;;; Code:

;; From http://blog.bookworm.at/2007/03/pretty-print-xml-with-emacs.html
(defun ar/nxml-pretty-print-region (begin end)
  "Format XML markup in region marked by BEGIN and END."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n") (setq end (1+ end)))
    (indent-region begin end))
  (message "Ah, much better!"))

(provide 'ar-nxml)

;;; ar-nxml.el ends here
