;;; ar-helm-org.el --- Helm org support.

;;; Commentary:
;; Helm org helpers.


;;; Code:


(require 'cl)
(require 'helm)
(require 'helm-org)
(require 'org)
(require 'org-cliplink)

(defvar ar/helm-org-source-my-todos
  `((name . "TODOS")
    (candidates . ar/helm-org-todo-candidates)
    (action . ,(helm-make-actions "goto" (lambda (marker)
                                           (org-goto-marker-or-bmk marker)
                                           (org-show-siblings))
                                  "mark DONE" (lambda (marker)
                                                (with-current-buffer (marker-buffer marker)
                                                  (save-excursion
                                                    (goto-char (marker-position marker))
                                                    (ar/org-move-to-current-week-as-done))))))))

(defun ar/helm-org-todos ()
  "Current TODOS."
  (interactive)
  (helm :sources '(ar/helm-org-source-my-todos)))

(defun ar/helm-org-todo-candidates ()
  "Get this week's TODOS helm candidates."
  (ar/helm-org-entry-child-candidates "~/stuff/active/non-public/daily/daily.org" "backlog"))

(defun ar/helm-org-entry-child-candidates (path id)
  "Get org child headings for entry with PATH and ID."
  (with-current-buffer (find-file-noselect (expand-file-name path))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (assert (ar/buffer-string-match-p (format ":CUSTOM_ID:[ ]*%s" id))
                (format "Cannot find %s#%s" path id))
        (goto-char (ar/buffer-first-match-beginning))
        (org-end-of-meta-data t)
        (let ((child-headings '())
              (child-heading))
          (when (org-at-heading-p)
            ;; Extract first child.
            (add-to-list 'child-headings
                         (cons (org-get-heading 'no-tags)
                               (copy-marker (point))))
            (while (org-get-next-sibling)
              (add-to-list 'child-headings
                           (cons (org-get-heading 'no-tags)
                                 (copy-marker (point))))))
          child-headings)))))

(defun ar/helm-org-add-backlog-link ()
  "Add a bookmark to blog."
  (interactive)
  (let ((new-backlog-link (ar/org-build-backlog-link)))
    (helm :sources '(((name . "Blog backlogs")
                      (candidates . ar/helm-org-get-blog-backlog-candidates)
                      (action . (lambda (candidate)
                                  (save-excursion
                                    (save-restriction
                                      (helm-org-goto-marker candidate)
                                      (org-show-subtree)
                                      (org-end-of-meta-data t)
                                      (org-insert-heading)
                                      (insert (concat new-backlog-link "."))
                                      (ar/update-blog-timestamp-at-point)
                                      (save-buffer))))))))))

(defun ar/helm-org--blog-bookmark-candidates ()
  "Gets helm candidates for my blog bookmarks."
  (ar/helm-org-candidates "~/stuff/active/blog/index.org"
                          "bookmarks"))

(defun ar/helm-org-goto-marker (marker)
  "Go to org file MARKER."
  (helm-org-goto-marker marker)
  (org-show-subtree)
  (recenter-top-bottom 3))

(defun ar/helm-org-add-bookmark ()
  "Add a bookmark to blog."
  (interactive)
  (org-cliplink-retrieve-title
   (if (string-match-p "^http" (current-kill 0))
       (current-kill 0)
     (read-string "URL: "))
   (lambda (url default-description)
     (helm :sources `(((name . "Blog bookmarks")
                       (candidates . ar/helm-org--blog-bookmark-candidates)
                       (action . (lambda (candidate)
                                   (helm-org-goto-marker candidate)
                                   (org-show-subtree)
                                   (org-end-of-meta-data t)
                                   (org-insert-heading)
                                   (insert (format "%s." ,(ar/org-build-link url
                                                                             (read-string "Description: " default-description))))
                                   (org-sort-list nil ?a)
                                   (ar/update-blog-timestamp-at-point)
                                   (hide-other)
                                   (save-buffer)))))))))

(defun ar/helm-org-format-candidates (helm-candidates)
  "Format and sort HELM-CANDIDATES.  For each candidate:

index.org: * [2014-07-13 Sun] [[#emacs-meetup][#]] Emacs London meetup bookmarks
<-------------------- remove ------------------->"
  (sort
   (mapcar (lambda (helm-candidate)
             (let* ((text (replace-regexp-in-string ".*#\\]\\] " ""
                                                    (car helm-candidate)))
                    (text-no-properties (substring-no-properties text)))
               (setcar helm-candidate text-no-properties)
               helm-candidate))
           helm-candidates)
   (lambda (a b)
     (string< (car a) (car b)))))

(defun ar/helm-org-filter-candidates (helm-candidates match)
  "Remove candidates in HELM-CANDIDATES not containing MATCH."
  (cl-remove-if-not (lambda (helm-candidate)
                      (string-match-p match
                                      (car helm-candidate)))
                    helm-candidates))

(defun ar/helm-org-get-blog-backlog-candidates ()
  "Gets helm candidates for my blog backlogs."
  (ar/helm-org-format-candidates
   (ar/helm-org-filter-candidates
    (helm-org-get-candidates (list "~/stuff/active/blog/index.org")) "backlog")))

(defun ar/helm-org-candidates (org-file-path filter-re)
  "Get helm candidates in ORG-FILE-PATH and filter matching FILTER-RE."
  (ar/helm-org-format-candidates
   (ar/helm-org-filter-candidates
    (helm-org-get-candidates (list org-file-path)) filter-re)))

(provide 'ar-helm-org)

;;; ar-helm-org.el ends here
