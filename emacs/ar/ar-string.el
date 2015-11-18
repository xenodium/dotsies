;;; ar-string.el --- String support.

;;; Commentary:
;; String helpers.


;;; Code:

(defun ar/string-spc-join (&rest strings)
  "Join strings in STRINGS list with spaces."
  (mapconcat 'identity strings " "))

(defun ar/string-join (separator &rest strings)
  "Join strings with SEPARATOR in STRINGS list."
  (mapconcat 'identity strings separator))

(defun ar/string-match-p (regex string)
  "Return t if REGEX is a match in STRING.  nil otherwise."
  (if (string-match regex string) t nil))

(defun ar/string-first-match (regex string)
  "Return first match if REGEX is a match in STRING.  nil otherwise."
  (when (ar/string-match-p regex string)
    (substring string
               (match-beginning 0)
               (match-end 0))))

(defun ar/string-numeric-p (string)
  "Return t if STRING is an unsigned integer.  nil otherwise."
  (ar/string-match-p "\\`[[:digit:]]+\\'" string))

(defun ar/string-alpha-numeric-p (string)
  "Return t if STRING is alphanumeric.  nil otherwise."
  (ar/string-match-p "\\`[[:alnum:]]+\\'" string))

(defun ar/string-replace-regex-pairs (haystack &rest pairs)
  "Replace in HAYSTACK all PAIRS. For example: (ar/string-replace \"one two\" '(\"one\" . \"three\") '(\"two\" . \"four\"))"
  (mapc (lambda (pair)
          (assert (consp pair) nil "Each pair must be a cons cell")
          (setq haystack (replace-regexp-in-string (car pair)
                                                   (cdr pair)
                                                   haystack)))
        pairs)
  haystack)

(provide 'ar-string)

;;; ar-string.el ends here
