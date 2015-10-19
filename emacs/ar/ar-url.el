;;; ar-url.el --- URL support

;;; Commentary:
;; URL helpers.


;;; Code:

(require 'ar-buffer)
(require 'ar-input)
(require 'enlive)
(require 'goto-addr)

(defun ar/url-fetch-urls (url)
  "Return URLs in URL content."
  (with-current-buffer (ar/buffer-fetch-url url)
    (let (urls url)
      (while (re-search-forward goto-address-url-regexp
                                nil t)
        (add-to-list 'urls
                     (buffer-substring-no-properties (match-beginning 0)
                                                     (match-end 0))))
      urls)))

(defun ar/url-fetch-anchor-elements (url)
  "Fetch anchor elements in URL as list of alist:
\((title . \"my title\")
 (url . \"http://some.location.com\"))."
  (let ((elements (enlive-query-all (enlive-fetch url) [a])))
    (mapcar (lambda (element)
              `((title . ,(enlive-text element))
                (url . ,(enlive-attr element 'href))))
            elements)))

(defun ar/url-view-links-at ()
  "View external links in HTML from prompted URL or clipboard."
  (interactive)
  (with-current-buffer (get-buffer-create "*links*")
    (org-mode)
    (read-only-mode -1)
    (erase-buffer)
    (mapc (lambda (anchor)
            (let-alist anchor
              (when (and .url (string-match "^http" .url))
                (insert (org-make-link-string .url
                                              .title) "\n"))))
          (ar/url-fetch-anchor-elements
           (ar/input-clipboard-url-or-prompt)))
    (delete-duplicate-lines (point-min) (point-max))
    (goto-char (point-min))
    (toggle-truncate-lines +1)
    (read-only-mode +1)
    (switch-to-buffer (current-buffer))))


(provide 'ar-url)

;;; ar-url.el ends here
