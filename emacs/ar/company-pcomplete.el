;;; company-pcomplete.el --- Company pcomplete support.

;;; Commentary:
;; Company pcomplete helpers.


;;; Code:

(require 'cl-lib)
(require 'company)
(require 'pcomplete)

(defun company-pcomplete--candidates ()
  "Get candidates for company completion using `pcomplete'."
  (all-completions "" (pcomplete-completions)))

(defun company-pcomplete (command &optional arg &rest ignored)
  "Complete using pcomplete. See `company''s COMMAND ARG and IGNORED for details."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-pcomplete))
    (prefix (company-grab-symbol))
    (candidates
     (company-pcomplete--candidates))))

(provide 'company-pcomplete)

;;; company-pcomplete.el ends here
