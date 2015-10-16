;;; ar-helm-url.el --- Helm URL fetch support

;;; Commentary:
;; Fetch anchors from URLs and display with helm.

(require 'enlive)
(require 'helm)
(require 'helm-source)

;;; Code:

(defun ar/helm-url-fetch-anchor-helm-candidates (url)
  "Fetch anchors elements in URL as helm source (TITLE . URL)."
  (let ((elements (enlive-query-all (enlive-fetch url) [a])))
    (mapcar (lambda (element)
              (cons (enlive-text element)
                    (enlive-attr element 'href)))
            elements)))

(defun ar/helm-url-fetch-anchors (url)
  "Display anchors fetched from URL using helm."
  (interactive "sURL: ")
  (let ((helm-source (helm-build-sync-source "URLs"
                       :candidates (ar/helm-url-fetch-anchor-helm-candidates url)
                       :resume 'noresume
                       :action (lambda (href)
                                 (browse-url href)))))
    (helm :sources '(helm-source))))

(provide 'ar-helm-url)

;;; ar-helm-url.el ends here
