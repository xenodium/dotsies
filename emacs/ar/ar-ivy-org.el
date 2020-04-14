;;; ar-ivy-org.el --- This is my init.    -*- lexical-binding: t; -*-

(require 'ar-org)
(require 'ar-string)
(require 'cl)
(require 'dash)
(require 'f)
(require 'org)
(require 'org-element)
(require 's)
(require 'org-link)

(defun ar/ivy-org-add-bookmark-dwim ()
  "Add a bookmark or TODO to backlog from clipboard."
  (interactive)
  (org-link-resolved-clipboard-url-or-title
   (lambda (link)
     (ivy-read "Save to: "
               (ar/ivy-org--ivy-blog-items "~/stuff/active/blog/index.org" "\\(bookmarks\\)\\|\\(backlog\\)")
               :action (lambda (selection)
                         (let ((breadcrumb-marker (point-marker))
                               (category (car selection))
                               (marker (cdr selection)))
                           (switch-to-buffer (marker-buffer marker))
                           (widen)
                           (goto-char (marker-position marker))
                           (org-show-context)
                           (re-search-backward "^\\*+ " nil t)
                           (org-show-entry)
                           (org-show-subtree)
                           (org-end-of-meta-data t)

                           (cond ((s-matches-p "Bookmarks" category)
                                  (if (org-link-url link)
                                      (insert (format "- [[%s][%s]].\n" (org-link-url link) (org-link-title link)))
                                    (insert (format "- %s.\n" (read-string "Bookmark: " (org-link-title link)))))
                                  (org-sort-list nil ?a)

                                  ;; Remove duplicates after inserting new one.
                                  (let ((begin (org-in-item-p)))
                                    (assert begin nil "Not in list")
                                    (goto-char begin)
                                    (let* ((struct (org-list-struct))
                                           (prevs (org-list-prevs-alist struct)))
                                      (delete-duplicate-lines (org-list-get-list-begin begin struct prevs)
                                                              (org-list-get-list-end begin struct prevs)))))
                                 ((s-matches-p "backlog" category)
                                  (org-insert-heading)
                                  (if (org-link-url link)
                                      (insert (format "TODO [[%s][%s]]." (org-link-url link) (org-link-title link)))
                                    (insert (format "TODO %s." (read-string "TODO: " (org-link-title link)))))
                                  (ar/org-timestamp-at-point)))

                           (ar/org-timestamp-at-point)

                           (hide-other)
                           (save-buffer)
                           (switch-to-buffer (marker-buffer breadcrumb-marker))
                           (message "Added \"%s\" to \"%s\"" (org-link-title link) category)))))))

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
                               (widen)
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

(defun ar/ivy-org-my-todos ()
  "Navigate to TODO or mark as done."
  (interactive)
  (ivy-set-actions 'ar/ivy-org-my-todos
                   '(("d" (lambda (item)
                            "Mark ITEM with (title . marker) cons as done."
                            (let ((marker (cdr item)))
                              (with-current-buffer (marker-buffer marker)
                                (save-excursion
                                  (goto-char (marker-position marker))
                                  (error "No longer supported")))))
                      "mark DONE")))
  (ivy-read "TODOs: "
            (sort
             (mapcar (lambda (item)
                       ;;  ********* TODO Emacs London meetup bookmarks
                       ;; <--- remove --->
                       (cons (substring (replace-regexp-in-string "[* ]*TODO[ ]*" ""
                                                                  (car item)))
                             (cdr item)))
                     (ar/org-get-headings-in-file (ar/org-get-daily-file-path) "TODO"))
             (lambda (a b)
               (string< (car a) (car b))))
            :require-match t
            :caller 'ar/ivy-org-my-todos
            :action (lambda (item)
                      (let ((marker (cdr item)))
                        (org-goto-marker-or-bmk marker)
                        (org-show-siblings)))))

(provide 'ar-ivy-org)
