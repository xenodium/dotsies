;;; ar-org.el --- Org support.

;;; Commentary:
;; Org mode helpers.

;;; Code:

(require 'ar-file)
(require 'ar-time)
(require 'ar-buffer)
(require 'org)

;; TODO: Move to ar/org-daily.
(defvar ar/org-daily-file-path "set/path/to/daily.org"
  "Path to daily.org file.")

(defun ar/org-get-daily-file-path ()
  (ar/file-assert-file-exists ar/org-daily-file-path))

(defun ar/org-search-file-buffer-forward (file-path regex)
  "Search FILE-PATH buffer using regex REGEX.  Move point to first instance."
  (ar/buffer-switch-to-file file-path)
  (goto-char 0)
  (re-search-forward regex nil)
  (beginning-of-line))

(defun ar/org-goto-file (file-path id)
  "Open org FILE-PATH at ID."
  (org-open-link-from-string (format "[[#%s]]" id)))

(defun ar/org-open-file-special-path (file-path)
  "Open special FILE-PATH.
Examples: path/to/file.txt#/s/regex Opens file.txt and moves cursor to regex."
  (cond ((ar/string-match-p "#/s/" file-path)
         (let* ((split-path (split-string file-path "#/s/"))
                (path (expand-file-name (nth 0 split-path)))
                (regex (nth 1 split-path)))
           (ar/org-search-file-buffer-forward path regex)
           (org-flag-heading nil)))
        (t
         (find-file file-path))))

(defun ar/org-move-to-current-week-as-done ()
  "Move current tree to current week as DONE."
  (interactive)
  (org-shiftright) ; mark done
  (org-cut-subtree 1)
  (ar/org-paste-subtree-to-current-week))

(defun ar/org-add-current-week-headline ()
  "Add current week to daily.org."
  (interactive)
  (ar/org-with-file-location
      (ar/org-get-daily-file-path) "snippets"
      (unless (ar/org-now-in-week-headline-p)
        (org-meta-return)
        (insert (format "Week of %s"
                        (ar/time-current-work-week-string)))
        (save-buffer))))

(defun ar/org-add-child-to-heading (file-path heading-id child)
  "Go to heading in FILE-PATH with HEADING-ID and add CHILD."
  (save-excursion
    (save-restriction
      (ar/org-goto-file file-path heading-id)
      (org-narrow-to-subtree)
      (org-show-subtree)
      (org-end-of-meta-data t)
      (org-insert-heading)
      (insert child)
      (ar/update-blog-timestamp-at-point)
      (save-buffer))))

(defun ar/org-goto-current-week ()
  "Go to current week."
  (ar/org-add-current-week-headline)
  (ar/buffer-goto-first-match-beginning (format "Week of %s"
                                                (ar/time-current-work-week-string))))

(defun ar/org-paste-subtree-to-current-week (&optional subtree)
  "Paste SUBTREE to current week."
  (ar/file-with-current-file (ar/org-get-daily-file-path)
    (save-excursion
      (ar/org-goto-current-week)
      (org-end-of-line)
      ;; See org-refile for details.
      (goto-char (or (save-excursion (org-get-next-sibling))
                     (org-end-of-subtree t t)
                     (point-max)))
      (org-paste-subtree (+ (org-current-level) 2) subtree))
    (save-buffer)))

(defun ar/org-now-in-week-headline-p ()
  "Check if current date corresponds to existing week headline."
  (save-excursion
    (let ((weeks (mapcar #'ar/time-week-string-to-range
                         (ar/buffer-re-string-match-list "^*** Week of .*"))))
      ;; Check current time against all ranges and create a list. is t present?
      (member t (mapcar (lambda (dates)
                          (ar/time-between-p (current-time)
                                             (nth 0 dates)
                                             (nth 1 dates)))
                        weeks)))))

(defun ar/org-move-current-tree-to-top ()
  "Move entire current tree to top."
  (interactive)
  (ar/org-point-to-heading-1)
  (while (not (org-first-sibling-p))
    (outline-move-subtree-up 1)))

(defun ar/org-update-drawer (drawer content)
  "Update DRAWER with CONTENT."
  (save-excursion
    (save-restriction
      ;; e.g match drawer like:
      ;; :MODIFIED:
      ;; [2015-03-22 Sun]
      ;; :END:
      (let ((drawer-re (concat "^[ \t]*:"
                               drawer
                               ":[[:ascii:]]*?:END:[ \t]*\n")))
        (ar/org-point-to-heading-1)
        (narrow-to-region (point)
                          (save-excursion
                            (outline-next-heading)
                            (point)))
        (if (re-search-forward drawer-re
                               nil t)
            ;; Remove existing drawer.
            (progn
              (goto-char (match-beginning 0))
              (replace-match ""))
          (org-end-of-meta-data t))
        ;; Insert new drawer + format.
        (org-insert-drawer nil drawer)
        (beginning-of-line 0)
        (org-indent-line)
        (forward-line)
        (insert content)
        (beginning-of-line 1)
        (org-indent-line)
        (beginning-of-line 2)
        (org-indent-line)
        ;; TODO: Avoid adding trailing caused by org-indent-line.
        (delete-trailing-whitespace)))))

(defun ar/org-add-done (done)
  "Add DONE task to current week."
  (interactive "sDONE: ")
  (ar/org-paste-subtree-to-current-week (format "* DONE %s" done)))

(defun ar/org-add-todo (todo)
  "Add a new TODO."
  (interactive "sTODO: ")
  (ar/org-with-file-location (ar/org-get-daily-file-path) "backlog"
                             (show-subtree)
                             (org-meta-return)
                             (insert (format "TODO %s" todo))
                             (save-buffer)))

(defun ar/org-build-backlog-link ()
  "Build an org backlog link, prompting for url and description."
  (format "TODO [[%s][%s]]"
          (if (string-match-p "^http" (current-kill 0))
              (current-kill 0)
            (read-string "URL: "))
          (read-string "Description: ")))

(defun ar/org-build-link (url default-description)
  "Build an org link with URL and DEFAULT-DESCRIPTION."
  (format "[[%s][%s]]" url description.))

(defun ar/org-blog-custom-id-from-title (title)
  "Create an org CUSTOM_ID from a TITLE."
  (replace-regexp-in-string " "
                            "-"
                            (replace-regexp-in-string "[^-a-zA-Z 0-9]"
                                                      ""
                                                      (downcase title))))

(defun ar/org-insert-prefixed-link (prefix prompt)
  "Insert a link with PREFIX and PROMPT if not found in clipboard."
  (interactive)
  (let* ((clipboard (current-kill 0))
         (cl-number (if (ar/string-numeric-p clipboard)
                        clipboard
                      (read-string (format "%s: "
                                           prompt))))
         (rendered-cl (format "[[http://%s%s][%s%s]]"
                              prefix
                              cl-number
                              prefix
                              cl-number)))
    (insert rendered-cl)))

(defun ar/org-entry-child-headings (id)
  "Get org child headings for entry with ID."
  (save-excursion
    (org-open-link-from-string (format "[[#%s]]" id))
    (org-end-of-meta-data t)
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
  (declare (indent 1))
  `(with-current-buffer (find-file-noselect (expand-file-name ,file-path))
     (save-excursion
       (org-open-link-from-string (format "[[#%s]]" ,item-id))
       (org-end-of-meta-data t)
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
   '(which-func ((t (:foreground "green"))))))

(provide 'ar-org)

;;; ar-org.el ends here
