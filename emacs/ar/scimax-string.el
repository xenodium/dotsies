;;; scimax-string.el --- Scimax helpers.

;;; Commentary:
;; Scimax helpers helpers.
;; Extracted r


;;; Code:

;; Extracted from https://raw.githubusercontent.com/jkitchin/scimax/2f57b337f6744c8ff3b9087bf7ebb95b4d9afa44/scimax-utils.el
(defmacro scimax-f-string (fmt)
  "Like `s-format' but with format fields in it.
FMT is a string to be expanded against the current lexical
environment. It is like what is used in `s-lex-format', but has
an expanded syntax to allow format-strings. For example:
${user-full-name 20s} will be expanded to the current value of
the variable `user-full-name' in a field 20 characters wide.
  (let ((f (sqrt 5)))  (f-string \"${f 1.2f}\"))
  will render as: 2.24
This function is inspired by the f-strings in Python 3.6, which I
enjoy using a lot.

You can also try putting expressions in for formatting, e.g.:
 (let ((a 11)) (f-string \"The sqrt of ${a} is ${(sqrt a) 1.2f}.\"))
 will render as \"The sqrt of 11 is 3.32\".
"
  (let* ((matches (s-match-strings-all"${\\(?3:\\(?1:[^} ]+\\) *\\(?2:[^}]*\\)\\)}" fmt))
         (agetter (cl-loop
		   for (m0 m1 m2 m3) in matches
		   collect
		   `(cons ,m3
			  ,(if (s-starts-with? "(" m3)
			       ;; This means an expression is used
			       (with-temp-buffer
				 (insert m3)
				 (goto-char (point-min))
				 (let ((expr (read (current-buffer)))
				       (fmt (s-trim (buffer-substring (point) (point-max)))))
				   `(format
				     (format "%%%s" (if (string= ,fmt "")
							(if s-lex-value-as-lisp "S" "s")
						      ,fmt))
				     ,expr)))

			     `(format
			       (format "%%%s" (if (string= ,m2 "")
						  (if s-lex-value-as-lisp "S" "s")
						,m2))
			       (symbol-value (intern ,m1))))))))

    `(s-format ,fmt 'aget (list ,@agetter))))

(provide 'scimax-string)

;;; scimax-string.el ends here
