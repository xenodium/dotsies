;;; org-link.el --- Org link support. -*- lexical-binding: t; -*-

;;; Commentary:
;; Org link helpers.


;;; Code:

(require 'org)
(require 's)
(require 'cl)
(require 'ar-url)
(require 'ar-string)

(cl-defstruct
    org-link
  title
  url)

(defun org-link--clipboard-url-or-title ()
  "Return a link with URL if found in clipboard, else link with title."
  (let ((value (substring-no-properties (current-kill 0))))
    (make-org-link :url (when (s-matches-p "^http" value)
                          (ar/url-sans-query value))
                   :title (unless (s-matches-p "^http" value)
                            value))))

(defun org-link-resolved-clipboard-url-or-title (fn)
  "Return a link by reesolving a URL in clipboard.  Invoke FN with link."
  (let ((link (org-link--clipboard-url-or-title)))
    (if (org-link-url link)
        (org-cliplink-retrieve-title (org-link-url link)
                                     (lambda (url title)
                                       (funcall fn
                                                (make-org-link :url url
                                                               :title (read-string "Link title: " (ar/string-decode-html-entities title))))))
      (funcall fn link))))

(provide 'org-link)

;;; org-link.el ends here
