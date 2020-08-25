;;; -*- lexical-binding: t; -*-

;; Swift helpers adjusted from:
;; https://github.com/jorgenschaefer/elpy/blob/060a4eb78ec8eba9c8fe3466c40a414d84b3dc81/snippets/python-mode/.yas-setup.el

(defun swift-snippet-init-assignments (arg-string)
  "Return init assignments for arguments."
  (let ((indentation (make-string (save-excursion
                                    (goto-char start-point)
                                    (current-indentation))
                                  ?\s)))
    (string-trim (mapconcat (lambda (arg)
                              (if (string-match "^\\*" arg)
                                  ""
                                (format "self.%s = %s\n%s"
                                        arg arg indentation)))
                            (swift-snippet-split-args arg-string)
                            ""))))

(defun swift-snippet-split-args (arg-string)
  "Split a python argument string into ((name, default)..) tuples"
  (mapcar (lambda (x)
            (if (and x (string-match "\\([[:alnum:]]*\\):" x))
                (match-string-no-properties 1 x)
              x))
          (split-string arg-string "[[:blank:]]*,[[:blank:]]*" t)))
