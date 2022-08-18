;;; ar-url.el --- URL support

;;; Commentary:
;; URL helpers.


;;; Code:

(require 'ar-buffer)
(require 'enlive)
(require 'goto-addr)

(defun ar/url-fetch-anchor-elements (url)
  "Fetch anchor elements in URL as list of alist:
\((title . \"my title\")
 (url . \"http://some.location.com\"))."
  (let ((elements (enlive-query-all (enlive-fetch url) [a])))
    (mapcar (lambda (element)
              `((title . ,(enlive-text element))
                (url . ,(enlive-attr element 'href))))
            elements)))

(defun ar/url-fetch-iframe-srcs (url)
  "Fetch iframe src urls in URL."
  (let ((elements (enlive-query-all (enlive-fetch url) [iframe])))
    (mapcar (lambda (element)
              (enlive-attr element 'src))
            elements)))

(defun ar/url-view-links-at ()
  "View external links in HTML from prompted URL or clipboard."
  (interactive)
  (with-current-buffer (get-buffer-create "*links*")
    (org-mode)
    (view-mode -1)
    (erase-buffer)
    (mapc (lambda (anchor)
            (let-alist anchor
              (when (and .url (string-match "^http" .url))
                (insert (org-make-link-string .url
                                              .title) "\n"))))
          (ar/url-fetch-anchor-elements
           (let* ((clipboard (current-kill 0))
                  (url (if (string-match "^https?://" clipboard)
                           clipboard
                         (read-string "URL: "))))
             (unless (string-match "^https?://" url)
               (error "Not a URL"))
             url)))
    (delete-duplicate-lines (point-min) (point-max))
    (goto-char (point-min))
    (toggle-truncate-lines +1)
    (view-mode +1)
    (switch-to-buffer (current-buffer))))

(defun ar/url-sans-query (url)
  "Remove query params from URL."
  (let ((u (url-generic-parse-url url)))
    (setf (url-filename u) (car (url-path-and-query u)))
    (url-recreate-url u)))


(provide 'ar-url)

;;; ar-url.el ends here
