;;; ar-blog.el --- Org blogging support

;;; Commentary:
;; Org mode blogging helpers.


;;; Code:

(require 'ar-file)
(require 'ar-org)
(require 'ar-process)
(require 'org-element)
(require 'outline)

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

(defun ar/org-blog-links-map (buffer category-fun link-fun)
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (outline-hide-sublevels 1)
        (org-element-map (org-element-parse-buffer 'greater-element t) 'headline
          (lambda (headline)
            (goto-char (org-element-property :begin headline))
            (show-entry)
            (narrow-to-region (org-element-property :begin headline)
                              (org-element-property :end headline))
            (org-element-map (org-element-parse-buffer) 'link
              (lambda (link)
                (when (string-equal (org-element-property :type link) "http")
                  (let* ((link-description-begin
                          (org-element-property :contents-begin link))
                         (link-description-end
                          (org-element-property :contents-end link))
                         (link-description
                          (and link-description-begin
                               link-description-end
                               (buffer-substring-no-properties link-description-begin
                                                               link-description-end))))
                    (funcall link-fun
                             ;; Default to url when no description.
                             (or link-description
                                 (org-element-property :raw-link link))
                             (org-element-property :raw-link link))))
                nil))
            (funcall category-fun (org-element-property :title headline))
            nil))))))

(defun ar/org-blog-bookmarks ()
  (interactive)
  (let ((helm-sources '())
        (candidates '()))
    (ar/org-blog-links-map (find-file-noselect "~/stuff/active/blog/index.org")
                           (lambda (headline)
                             (push (helm-build-sync-source (replace-regexp-in-string "\\[.*\\]" "" headline)
                                     :candidates candidates
                                     :resume 'noresume
                                     :action (lambda (url)
                                               (browse-url url)))
                                   helm-sources)
                             (setq candidates '()))
                           (lambda (description url)
                             (push (cons description
                                         url)
                                   candidates)))
    (helm :sources helm-sources)))

(provide 'ar-org-blog)

;;; ar-blog.el ends here
