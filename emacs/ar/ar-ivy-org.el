;;; ar-ivy-org.el --- This is my init.    -*- lexical-binding: t; -*-

(require 'f)
(require 'dash)
(require 'ar-org)
(require 's)
(require 'org-cliplink)

(defun ar/ivy-org-add-bookmark ()
  "Add a bookmark to blog."
  (interactive)
  (ar/ivy-org-add-bookmark-url (if (string-match-p "^http" (current-kill 0))
                                   (current-kill 0)
                                 (read-string "URL: "))))

(defun ar/ivy-org-add-bookmark-url (url)
  "Add a bookmark to blog."
  (org-cliplink-retrieve-title
   url
   (lambda (url default-description)
     (setq default-description
           (ar/org--preprocess-url-title
            (read-string "Description: " default-description)))
     (ivy-read "Heading: "
               (ar/ivy-org--ivy-blog-items "~/stuff/active/blog/index.org" "bookmarks")
               :action (lambda (item)
                         (let ((breadcrumb-marker (point-marker))
                               (marker (cdr item)))

                           (switch-to-buffer (marker-buffer marker))
                           (goto-char (marker-position marker))
                           (org-show-context)
                           (re-search-backward "^\\*+ " nil t)
                           (org-show-entry)

                           (org-show-subtree)
                           (org-end-of-meta-data t)
                           (insert (format "- %s.\n" (ar/org-build-link url
                                                                        default-description)))
                           (org-sort-list nil ?a)
                           (ar/org-timestamp-at-point)
                           (hide-other)
                           (save-buffer)

                           (switch-to-buffer (marker-buffer breadcrumb-marker)))
                         (message "Added: \"%s\"" url))))))

(defun ar/ivy-org-add-backlog-link ()
  "Add a link to blog."
  (interactive)
  (ar/ivy-org-add-backlog-url (if (string-match-p "^http" (current-kill 0))
                                   (current-kill 0)
                                 (read-string "URL: "))))

(defun ar/ivy-org-add-backlog-url (url)
  "Add a bookmark to blog."
  (org-cliplink-retrieve-title
   url
   (lambda (url description)
     (setq description
           (ar/org--preprocess-url-title
            (read-string "Description: " description)))
     (ivy-read "Heading: "
               (ar/ivy-org--ivy-blog-items "~/stuff/active/blog/index.org" "backlog")
               :action (lambda (item)
                         (let ((breadcrumb-marker (point-marker))
                               (marker (cdr item)))

                           (save-excursion
                             (save-restriction
                               (switch-to-buffer (marker-buffer marker))
                               (goto-char (marker-position marker))
                               (org-show-context)
                               (re-search-backward "^\\*+ " nil t)
                               (org-show-entry)

                               (org-show-subtree)
                               (org-end-of-meta-data t)
                               (org-insert-heading)

                               (insert (format "TODO [[%s][%s]]." url description))
                               (ar/org-timestamp-at-point)
                               (save-buffer)

                               (switch-to-buffer (marker-buffer breadcrumb-marker)))))
                         (message "Added: \"%s\"" url))))))


(defun ar/ivy-org--ivy-blog-items (filename needle)
  (sort
   (mapcar (lambda (item)
             ;;  * [2014-07-13 Sun] [[#emacs-meetup][#]] Emacs London meetup bookmarks
             ;; <------------ remove ------------------>
             (let* ((text (replace-regexp-in-string ".*#\\]\\] " ""
                                                    (car item)))
                    (text-no-properties (substring-no-properties text)))
               (setcar item text-no-properties)
               item))
           (ar/org-get-headings-in-file filename needle))
   (lambda (a b)
     (string< (car a) (car b)))))

(provide 'ar-ivy-org)
