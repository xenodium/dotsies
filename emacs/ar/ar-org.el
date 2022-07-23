;;; ar-org.el --- Org support. -*- lexical-binding: t; -*-

;;; Commentary:
;; Org mode helpers.

;;; Code:

(require 'ar-file)
(require 'ar-time)
(require 'ar-buffer)
(require 'org)
(require 'dash)
(require 's)
(require 'cl-lib)

(defvar ar/org-short-link-regex "^http://goo.gl")

(cl-defstruct
    ar/org-link
  description
  url)

(defun ar/org-short-links ()
  "Extracts short links from `ar/org-get-daily-file-path'."
  (with-current-buffer (find-file-noselect (expand-file-name (ar/org-get-daily-file-path)))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (cl-assert (ar/buffer-string-match-p (format ":CUSTOM_ID:[ ]*%s" "short-links"))
                (format "Cannot find %s#%s" path "short-links"))
        (goto-char (ar/buffer-first-match-beginning))
        (org-end-of-meta-data t)
        (let ((element (org-element-at-point)))
          (-map
           (lambda (line)
             ;; Short links links are of the form:
             ;; my/link - some description
             (let* ((entry (s-split " - " line))
                    (link (s-trim (nth 0 entry)))
                    (description (s-trim (nth 1 entry))))
               (make-ar/org-link :description description
                                 :url link)))
           (-filter (lambda (line)
                      (not (s-blank-str? line)))
                    (s-split "\n" (buffer-substring-no-properties
                                   (org-element-property :contents-begin element)
                                   (org-element-property :contents-end element))))))))))

;; TODO: Move to ar/org-daily.
(defvar ar/org-daily-file-path "~/stuff/active/non-public/private.org"
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
  (cond ((string-match "#/s/" file-path)
         (let* ((split-path (split-string file-path "#/s/"))
                (path (expand-file-name (nth 0 split-path)))
                (regex (nth 1 split-path)))
           (ar/org-search-file-buffer-forward path regex)
           (org-flag-heading nil)))
        (t
         (find-file file-path))))

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

(defun ar/org-build-backlog-link ()
  "Build an org backlog link, prompting for url and description."
  (format "TODO [[%s][%s]]"
          (if (string-match-p "^http" (current-kill 0))
              (current-kill 0)
            (read-string "URL: "))
          (read-string "Description: ")))

(defun ar/org-build-link (url description)
  "Build an org link with URL and DESCRIPTION."
  (format "[[%s][%s]]" url description))

(defun ar/org-blog-custom-id-from-title (title)
  "Create an org CUSTOM_ID from a TITLE."
  (replace-regexp-in-string " "
                            "-"
                            (replace-regexp-in-string "[^-a-zA-Z 0-9]"
                                                      ""
                                                      (downcase title))))

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
   '(org-block-end-line
     ((t (:foreground "#008ED1" :background nil))))
   '(which-func ((t (:foreground "green"))))))

;; http://kitchingroup.cheme.cmu.edu/blog/2017/04/09/A-better-return-in-org-mode/?utm_source=feedburner&utm_medium=twitter&utm_campaign=Feed:+TheKitchinResearchGroup+(The+Kitchin+Research+Group)
(defun ar/org-return ()
  "Add new list or headline "
  (interactive)
  (cond
   ((org-in-item-p)
    (if (org-element-property :contents-begin (org-element-context))
        (org-insert-heading)
      (beginning-of-line)
      (setf (buffer-substring
             (line-beginning-position) (line-end-position)) "")
      (org-return)))
   ((org-at-heading-p)
    (if (not (string= "" (org-element-property :title (org-element-context))))
        (progn (org-end-of-meta-data)
               (org-insert-heading))
      (beginning-of-line)
      (setf (buffer-substring
             (line-beginning-position) (line-end-position)) "")))
   ((org-at-table-p)
    (if (-any?
         (lambda (x) (not (string= "" x)))
         (nth
          (- (org-table-current-dline) 1)
          (org-table-to-lisp)))
        (org-return)
      ;; empty row
      (beginning-of-line)
      (setf (buffer-substring
             (line-beginning-position) (line-end-position)) "")
      (org-return)))
   (t
    (org-return))))

(defun ar/org-timestamp-at-point ()
  "Update blog entry timestamp at point."
  (interactive)
  (ar/org-update-drawer "MODIFIED"
                        (format-time-string "[%Y-%m-%d %a]")))

(defun ar/org--preprocess-url-title (url-title)
  "Reformat page URL-TITLE For example:
HTTPS Is Easy | Irreal => HTTPS Is Easy (Irreal)"
  (string-match "^\\(.*\\)\\( | \\)\\(.*\\)$" url-title)
  (let ((site (match-string 3 url-title))
        (description (match-string 1 url-title)))
    (if (and site description)
        (format "%s (%s)" description site)
      url-title)))

(defun ar/org-insert-clipboard-link ()
  "Insert a bookmark link from clipboard."
  (interactive)
  (cl-assert (string-match-p "^http" (current-kill 0)) nil "Not URL in clipboard")
  (org-cliplink-retrieve-title
   (current-kill 0)
   (lambda (url default-description)
     (insert (format "%s."
                     (ar/org-build-link url
                                        (read-string "Description: " (ar/org--preprocess-url-title
                                                                      default-description))))))))

(defun ar/org-clipboard-url-or-title ()
  "Return URL if found in clipboard or query for title otherwise."
  (let ((value (substring-no-properties (current-kill 0))))
    (if (string-match-p "^http" value)
        value
      (read-string (format "(%s): " value) nil nil value))))

(defun ar/org-get-headings-in-file (filename &optional needle target-level)
  "Return a list of cons: (heading . marker) for FILENAME searching matching NEEDLE regexp and TARGET-LEVEL if set.  Ignored otherwise."
  ;; This method is mostly distilled from `helm-org--get-candidates-in-file'.
  (with-current-buffer (pcase filename
                         ((pred bufferp) filename)
                         ((pred stringp) (find-file-noselect filename t)))
    (let ((match-fn #'match-string)
          (search-fn (lambda ()
                       (re-search-forward
                        org-complex-heading-regexp nil t))))
      (save-excursion
        (save-restriction
          (unless (and (bufferp filename)
                       (buffer-base-buffer filename))
            ;; Only widen direct buffers, not indirect ones.
            (widen))
          (goto-char (point-min))
          (and (boundp 'org-outline-path-cache)
               (setq org-outline-path-cache nil))
          (-filter (lambda (item)
                     (if needle
                         (s-matches-p needle (car item))
                       t))
                   (cl-loop with width = 1000
                            while (funcall search-fn)
                            for beg = (point-at-bol)
                            for end = (point-at-eol)
                            when (null (text-property-any
                                        beg end 'fontified t))
                            do (jit-lock-fontify-now beg end)
                            for level = (length (match-string 1))
                            for heading = (funcall match-fn 4)
                            if (or (null target-level)
                                   (eq level target-level))
                            collect `(,(funcall match-fn 0)
                                      . ,(point-marker)))))))))

(defun ar/org-entry-child-headings (path id)
  "Get org child headings for entry with PATH and ID."
  (with-current-buffer (find-file-noselect (expand-file-name path))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (cl-assert (ar/buffer-string-match-p (format ":CUSTOM_ID:[ ]*%s" id))
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

(defun ar/org-goto-marker (marker)
  "Go to org file MARKER."
  (switch-to-buffer (marker-buffer marker))
  (goto-char (marker-position marker))
  (org-show-context)
  (re-search-backward "^\\*+ " nil t)
  (org-show-entry)
  (org-show-subtree)
  (recenter-top-bottom 3))

(provide 'ar-org)

;;; ar-org.el ends here
