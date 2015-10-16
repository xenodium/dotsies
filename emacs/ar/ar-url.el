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
  "Fetch anchors elements in URL as list of alist:
\((title . \"my title\")
 (url . \"http://some.location.com\"))."
  (let ((elements (enlive-query-all (enlive-fetch url) [a])))
    (mapcar (lambda (element)
              `((title . ,(enlive-text element))
                (url . ,(enlive-attr element 'href))))
            elements)))

(defun ar/url-view-anchor-elements-in-url (url)
  "View anchor elements in URL content."
  (interactive "s URL: ")
  (with-current-buffer (get-buffer-create "*anchor elements*")
    (erase-buffer)
    (let ((anchors (ar/url-fetch-anchor-elements url)))
      (mapc (lambda (anchor)
              (insert (format "%s\n" (cdr (assoc 'url anchor)))))
            anchors)
      (goto-char (point-min)))
    (switch-to-buffer (current-buffer))
    (keep-lines "^http")
    (delete-duplicate-lines (point-min) (point-max))
    (sort-lines nil (point-min) (point-max))
    (org-mode)))

(provide 'ar-url)

;;; ar-url.el ends here
