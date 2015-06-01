;;; ar-org.el --- Org support.

;;; Commentary:
;; Org mode helpers.

(require 'ar-file)

(defun ar/org-entry-child-headings (id)
  "Get org child headings for entry with ID."
  (save-excursion
    (org-open-link-from-string (format "[[#%s]]" id))
    (org-end-of-meta-data-and-drawers)
    (let ((child-headings '())
          (child-heading))
      (when (org-at-heading-p)
        ;; Extract first child.
        (setq child-heading (substring-no-properties (org-get-heading 'no-tags)))
        (add-to-list 'child-headings child-heading)
        ;; Now handle remaining siblings.
        (while (org-get-next-sibling)
          (setq child-heading (substring-no-properties (org-get-heading 'no-tags)))
          (add-to-list 'child-headings child-heading) ))
      child-headings)))

(defmacro ar/org-with-file-location (file-path item-id &rest body)
  "Open org file at FILE-PATH, ITEM-ID location and execute BODY."
  `(with-current-buffer (find-file-noselect (expand-file-name ,file-path))
     (save-excursion
       (org-open-link-from-string (format "[[#%s]]" ,item-id))
       (org-end-of-meta-data-and-drawers)
       (progn ,@body))))

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

;; https://github.com/howardabrams/dot-files/blob/HEAD/emacs-client.org
(defun ar/org-src-color-blocks-light ()
  "Color the block headers and footers to make them stand out more for lighter themes."
  (interactive)
  (custom-set-faces
   '(org-block-begin-line
     ((t (:underline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF"))))
   '(org-block-background
     ((t (:background "#FFFFEA"))))
   '(org-block-end-line
     ((t (:overline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF"))))
   '(mode-line-buffer-id ((t (:foreground "#005000" :bold t))))
   '(which-func ((t (:foreground "#008000"))))))

;; Based on https://github.com/howardabrams/dot-files/blob/HEAD/emacs-client.org
(defun ar/org-src-color-blocks-dark ()
  "Color the block headers and footers to make them stand out more for dark themes."
  (interactive)
  (custom-set-faces
   '(org-block-begin-line
     ((t (:foreground "#008ED1" :background nil))))
   '(org-block ((t (:background "SlateBlue4" :foreground nil :box nil))))
   '(org-block-background
     ((t (:background "#111111"))))
   '(org-block-end-line
     ((t (:foreground "#008ED1" :background nil))))
   '(mode-line-buffer-id ((t (:foreground "black" :bold t))))
   '(which-func ((t (:foreground "green"))))))

(setq org-drawers(append '("MODIFIED") org-drawers))

(provide 'ar-org)

;;; ar-org.el ends here
