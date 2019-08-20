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
                          ;; Whitelist some domains to include query params.
                          (if (s-matches-p "^https?://news\\.ycombinator\\.com" value)
                              value
                            (ar/url-sans-query value)))
                   :title (unless (s-matches-p "^http" value)
                            value))))

(defun org-link-resolved-clipboard-url-or-title (fn)
  "Return a link by reesolving a URL in clipboard.  Invoke FN with link."
  (let* ((link (org-link--clipboard-url-or-title))
         (url (org-link-url link))
         (title (unwind-protect
                    (org-cliplink-retrieve-title-synchronously url))))
    (funcall fn (make-org-link :url (org-link-url link)
                               :title (read-string "Link title: " (if title
                                                                      (ar/string-decode-html-entities title)
                                                                    ""))))))

(provide 'org-link)

;;; org-link.el ends here
