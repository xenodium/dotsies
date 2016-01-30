;;; ar-org-iter.el --- Org mode iteration support.

;;; Commentary:
;; Org mode iteration helpers.


;;; Code:

(require 'ar-file)
(require 'outline)
(require 'org)
(require 'cl)

(defun ar/org-iter-heading-1-markers (org-file-path)
  (let (markers)
    (ar/file-with-current-file org-file-path
      ;; TODO: Make read-only.
      (outline-next-heading)
      (assert (org-on-heading-p) nil "No heading found")
      (add-to-list 'markers (point-marker) t)
      ;; (message (substring-no-properties (org-get-heading 'no-tags 'no-todo)))
      ;; (add-to-list 'markers (copy-marker (point)) t)
      (while (outline-get-next-sibling)
        (assert (org-on-heading-p) nil "Unexpected item found")
        (add-to-list 'markers (point-marker) t))
        ;; (add-to-list 'markers (copy-marker (point)) t))
      markers)))

(ert-deftest ar/org-iter-heading-1-markers-test ()
  (let ((org-file-path (make-temp-name temporary-file-directory)))
    (with-temp-file org-file-path
      (insert "#+TITLE: This is a test\n")
      (insert "* Heading 1-a\n")
      (insert "** Heading 2-a\n")
      (insert "*** Heading 3-a\n")
      (insert "* Heading 1-b\n")
      (insert "** Heading 2-b\n")
      (insert "* Heading 1-c\n"))
    (should (= (length (ar/org-iter-heading-1-markers org-file-path)) 3))))

(message "%d" (length (ar/org-iter-heading-1-markers "/Users/tuco/stuff/active/blog/index.org")))
(ar/file-with-current-file "/Users/tuco/stuff/active/blog/index.org"
  (mapc (lambda (marker)
          (goto-char marker)
          (message (substring-no-properties (org-get-heading 'no-tags 'no-todo))))
        (ar/org-iter-heading-1-markers "/Users/tuco/stuff/active/blog/index.org")))

(provide 'ar-org-iter)

;;; ar-org-iter.el ends here
