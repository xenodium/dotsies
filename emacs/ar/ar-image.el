;;; ar-image.el --- Image support.

;;; Commentary:
;; Image helpers.

(require 'ar-buffer)
(require 'ar-file)
(require 'ar-string)

;;; Code:

(defun ar/image-open-html-for-current-dir ()
  "Open generated HTML for current dir path."
  (interactive)
  (ar/image-open-html-for-dir-path (ar/buffer-current-dir-path) 15 "jpg"))

(defun ar/image-open-html-for-dir-path (dir-path image-percentage-size image-extension)
  "Open generated HTML page for DIR-PATH, IMAGE-PERCENTAGE-SIZE, and IMAGE-EXTENSION.  For example: (ar/image-open-html-for-dir-path \"path/to/images/\" 10 \"jpg\")."
  (let ((body-html-template "<html><header><title>{{dir-path}}</title></header><body><h1>{{dir-path}}<h1/>{{images}}</body></html>")
        (image-html-template (format "<img width='%d%%' src='{{image-path}}'/>" image-percentage-size))
        (images-html "")
        (images-paths (ar/file-find (format "*\\\\.%s" image-extension) (expand-file-name dir-path)))
        (output-file-path (format "/tmp/%d.html" (random 9999))))
    (assert (> (length images-paths) 0) nil "No images found")
    (mapc (lambda (image-path)
            (setq images-html
                  (concat images-html
                          (replace-regexp-in-string "{{image-path}}"
                                                    image-path image-html-template)
                          "\n")))
          images-paths)
    (append-to-file (ar/string-replace-regex-pairs body-html-template
                                                   (cons "{{dir-path}}" dir-path)
                                                   (cons "{{images}}" images-html))
                    nil
                    output-file-path)
    (browse-url-default-browser output-file-path)))

(provide 'ar-image)

;;; ar-image.el ends here
