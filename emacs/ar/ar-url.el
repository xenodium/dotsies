;;; ar-url.el --- URL support

;;; Commentary:
;; URL helpers.


;;; Code:

(require 'ar-buffer)
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

(defun ar/url-view-links-at (url)
  "View external links in HTML at URL location."
  (interactive "s URL: ")
  (with-current-buffer (get-buffer-create "*anchor elements*")
    (read-only-mode -1)
    (erase-buffer)
    (mapc (lambda (anchor)
            (let-alist anchor
              (when (and .url (string-match "^http" .url))
                (insert (org-make-link-string href .title) "\n"))))
          (ar/url-fetch-anchor-elements url))
    (goto-char (point-min))
    (delete-duplicate-lines (point-min) (point-max))
    (sort-lines nil (point-min) (point-max))
    (org-mode)
    (toggle-truncate-lines +1)
    (read-only-mode +1)
    (switch-to-buffer (current-buffer))))


(provide 'ar-url)

;;; ar-url.el ends here
