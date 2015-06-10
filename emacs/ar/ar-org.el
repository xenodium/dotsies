;;; ar-org.el --- Org support.

;;; Commentary:
;; Org mode helpers.

;;; Code:

(require 'ar-file)
(require 'ar-time)
(require 'ar-buffer)
(require 'org)

(defun ar/org-add-current-week-headline ()
  "Add current week to daily.org."
  (ar/org-with-file-location
      "~/stuff/active/non-public/daily/daily.org" "snippets"
      (unless (ar/org-now-in-week-headline-p)
        (org-meta-return)
        (insert (format "Week of %s"
                        (ar/time-current-work-week-string)))
        (save-buffer))))

(defun ar/org-add-child-to-current-week (child)
  "Add CHILD to current week."
  (interactive "sAdd to this week: ")
  (ar/file-with-current-file "~/stuff/active/non-public/daily/daily.org"
    (ar/org-add-current-week-headline)
    (ar/buffer-goto-first-match-beginning (format "Week of %s"
                                                  (ar/time-current-work-week-string)))
    (org-end-of-line)
    (org-meta-return)
    (org-metaright)
    (insert child)
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
          (org-end-of-meta-data-and-drawers))
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

(defun ar/org-add-todo (todo)
  "Add a new TODO."
  (interactive "sTODO: ")
  (ar/org-with-file-location "~/stuff/active/non-public/daily/daily.org" "backlog"
                             (org-meta-return)
                             (insert (format "TODO %s" todo))
                             (save-buffer)))

(defun ar/org-build-link ()
  "Build an org link, prompting for url and description."
  (format "[[%s][%s]]"
          (if (string-match-p "^http" (current-kill 0))
              (current-kill 0)
            (read-string "URL: "))
          (read-string "Description: ")))

(defun ar/org-blog-custom-id-from-title (title)
  "Create an org CUSTOM_ID from a TITLE."
  (replace-regexp-in-string " " "-" (downcase title)))

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

(defun ar/org-insert-cl-link ()
  "Insert a CL link."
  (interactive)
  (ar/org-insert-prefixed-link "cl/" "CL number"))

(defun ar/org-insert-bug-link ()
  "Insert a bug link."
  (interactive)
  (ar/org-insert-prefixed-link "b/" "Bug number"))

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
  (declare (indent 1))
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
