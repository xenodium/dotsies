;;; ar-org.el --- Org support.

;;; Commentary:
;; Org mode helpers.

(require 'ar-file)

(defun ar/org-point-to-heading-1 ()
  "Move point to heading level 1."
  (while (org-up-heading-safe)))

(defun ar/org-element-at-heading-1 ()
  "Get nearest heading 1 element."
  (save-excursion
    (ar/org-point-to-heading-1)
    (org-element-at-point)))

(defun ar/org-get-element-type (element)
  "Get ELEMENT type."
  (nth 0 element))

(defun ar/org-get-element-properties (element)
  "Get ELEMENT properties."
  (nth 1 element))

(defun ar/org-custom-id-at-heading-1 ()
  "Get :CUSTOM_ID: property at nearest heading 1."
  (plist-get (ar/org-get-element-properties
              (ar/org-element-at-heading-1))
             :CUSTOM_ID))

;; TODO. Move to ar-blog.el.
(defun ar/org-insert-image ()
  "Insert an image into the current headline, creating a subdirectory with CUSTOM_ID."
  (interactive)
  (let ((custom-id (ar/org-custom-id-at-heading-1))
        (image-path nil)
        (destination-path nil)
        (default-name nil)
        (file-name nil))
    (unless custom-id
      (error "Can't find a heading 1 CUSTOM_ID"))
    (setq image-path (ar/file-read-image-name))
    (setq default-name (file-name-nondirectory image-path))
    (setq file-name (read-string (format "Default name (%s): "
                                         default-name)
                                 nil nil default-name))
    (setq destination-path (format "images/%s" custom-id))
    (mkdir destination-path t)
    (setq destination-path (format "%s/%s" destination-path
                                   file-name))
    (copy-file image-path destination-path)
    (insert (format "[[file:%s]]" destination-path))
    (org-display-inline-images)))

(provide 'ar-org)

;;; ar-org.el ends here
