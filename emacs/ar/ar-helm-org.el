;;; ar-helm-org.el --- Helm org support.

;;; Commentary:
;; Helm org helpers.


;;; Code:


(require 'helm)
(require 'org)

(defvar ar/helm-org-bookmark-link-in-process nil)

(defvar ar/helm-org-source-my-todos
  `((name . "TODOS")
    (candidates . ar/helm-org-todo-candidates)
    (action . ,(helm-make-actions "goto" (lambda (marker)
                                           (org-goto-marker-or-bmk marker)
                                           (show-all))
                                  "mark DONE" (lambda (marker)
                                                (with-current-buffer (marker-buffer marker)
                                                  (goto-char (marker-position marker))
                                                  (org-shiftright)
                                                  (ar/org-add-child-to-current-week (org-get-heading))
                                                  (kill-whole-line)
                                                  (save-buffer)))))))

(defun ar/helm-org-todos ()
  "Current TODOS."
  (interactive)
  (helm :sources '(ar/helm-org-source-my-todos)))

(defun ar/helm-org-my-hotspots ()
  "Show my hotspots."
  (interactive)
  (unless helm-source-buffers-list
    (setq helm-source-buffers-list
          (helm-make-source "Buffers" 'helm-source-buffers)))
  (helm :sources '(helm-source-buffers-list
                   ar/helm-source-local-hotspots
                   ar/helm-source-web-hotspots
                   ar/helm-source-blog
                   ar/helm-org-source-my-todos
                   helm-source-ido-virtual-buffers
                   helm-source-buffer-not-found)
        :buffer "*helm buffers*"
        :keymap helm-buffer-map
        :truncate-lines t))

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
        (if (ar/buffer-string-match-p (format ":CUSTOM_ID:[ ]*%s" id))
            (progn
              (goto-char (ar/buffer-first-match-beginning))
              (org-end-of-meta-data-and-drawers)
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
                child-headings))
          (message "Cannot find %s#%s" path id)
          '())))))

(defun ar/helm-org-save-bookmark-link-in-process ()
  "Prompt and save a bookmark link in process."
  (setq ar/helm-org-bookmark-link-in-process (ar/org-build-link)))

(defun ar/helm-org-retrieve-bookmark-link-in-process ()
  "Get bookmark link in process."
  (let ((bookmark-link-in-process ar/helm-org-bookmark-link-in-process))
    (setq ar/helm-org-bookmark-link-in-process nil)
    bookmark-link-in-process))

(defun ar/helm-org-add-bookmark ()
  "Add a bookmark to blog."
  (interactive)
  (ar/helm-org-save-bookmark-link-in-process)
  (helm :sources '(((name . "Blog bookmarks")
                    (candidates . ar/helm-org-get-blog-bookmark-candidates)
                    (action . (lambda (candidate)
                                (helm-org-goto-marker candidate)
                                (org-show-subtree)
                                (org-end-of-meta-data-and-drawers)
                                (org-insert-heading)
                                (insert (ar/helm-org-retrieve-bookmark-link-in-process))
                                (org-sort-list nil ?a)
                                (ar/update-blog-timestamp-at-point)
                                (hide-other)
                                (save-buffer)))))))

(defun ar/helm-org-cleanse-candidates (helm-candidates)
  "Format HELM-CANDIDATES.  For each candidate:

index.org: * [2014-07-13 Sun] [[#emacs-meetup][#]] Emacs London meetup bookmarks
<-------------------- remove ------------------->"
  (mapcar (lambda (helm-candidate)
            (let* ((text (replace-regexp-in-string ".*#\\]\\] " ""
                                                   (car helm-candidate)))
                   (text-no-properties (substring-no-properties text)))
              (setcar helm-candidate text-no-properties)
              helm-candidate))
          helm-candidates))

(defun ar/helm-org-filter-candidates (helm-candidates match)
  "Remove candidates in HELM-CANDIDATES not containing MATCH."
  (cl-remove-if-not (lambda (helm-candidate)
                      (string-match-p match
                                      (car helm-candidate)))
                    helm-candidates))

;; TODO: Merge with ar/helm-org-get-blog-candidates.
(defun ar/helm-org-get-blog-bookmark-candidates ()
  "Gets helm candidates for my blog bookmarks."
  (let* ((org-filepath (expand-file-name "~/stuff/active/blog/index.org"))
         (helm-candidates (helm-get-org-candidates-in-file org-filepath 0 1)))
    (ar/helm-org-cleanse-candidates (ar/helm-org-filter-candidates helm-candidates "bookmarks"))))

(defun ar/helm-org-get-blog-candidates ()
  "Gets helm candidates for my blog."
  (let* ((org-filepath (expand-file-name "~/stuff/active/blog/index.org"))
         (helm-candidates (helm-get-org-candidates-in-file org-filepath 0 1)))
    (ar/helm-org-cleanse-candidates helm-candidates)))

(provide 'ar-helm-org)

;;; ar-helm-org.el ends here
