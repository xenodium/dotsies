;;; ar-blog.el --- Org blogging support

;;; Commentary:
;; Org mode blogging helpers.


;;; Code:

(require 'ar-file)
(require 'ar-org)
(require 'ar-process)

(defun ar/org-blog-insert-image ()
  "Insert an image into the current headline, creating a subdirectory with CUSTOM_ID."
  (interactive)
  (let ((custom-id (ar/org-custom-id-at-heading-1))
        (image-path nil)
        (destination-dir-path nil)
        (destination-file-path nil)
        (default-name nil)
        (file-name nil))
    (unless custom-id
      (error "Can't find a heading 1 CUSTOM_ID"))
    (setq image-path (ar/file-read-image-name))
    (setq default-name (file-name-nondirectory image-path))
    (setq file-name (read-string (format "Default name (%s): "
                                         default-name)
                                 nil nil default-name))
    (setq destination-dir-path (format "images/%s" custom-id))
    (mkdir destination-dir-path t)
    (setq destination-file-path (format "%s/%s"
                                        destination-dir-path
                                        file-name))
    (copy-file image-path destination-file-path)
    (insert (format "[[file:%s]]" destination-file-path))
    destination-file-path))

(defun ar/org-blog-insert-resized-image ()
  "Insert an image (resized) into the current headline, creating a subdirectory with CUSTOM_ID."
  (interactive)
  (let* ((image-file-path (ar/org-blog-insert-image))
         (image-dir-path (file-name-directory image-file-path))
         (image-file-name (file-name-nondirectory image-file-path))
         (default-percentage "10"))
  (mkdir (concat image-dir-path "/original") t)
  (copy-file image-file-path (concat image-dir-path "/original/" image-file-name))
  (unless (eq 0 (ar/process-call "mogrify"
                                 "-resize"
                                 (format "%s%%"
                                         (read-string (format "Default %% (%s): "
                                                              default-percentage)
                                                      nil nil default-percentage))
                                 image-file-path))
    (error "Unable to resize image"))))

(provide 'ar-org-blog)

;;; ar-blog.el ends here
