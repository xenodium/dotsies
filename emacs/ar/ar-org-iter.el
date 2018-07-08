;;; ar-org-iter.el --- Org mode iteration support.

;;; Commentary:
;; Org mode iteration helpers.


;;; Code:

(require 'ar-file)
(require 'cl)
(require 'org)
(require 'org-element)
(require 'outline)

(defun ar/org-iter--assert-org-mode ()
  "Assert buffer in `org-mode'."
  (assert (eq major-mode 'org-mode) nil "Not in org-mode"))

(defun ar/org-iter--assert-with-block ()
  "Assert method called within `ar/org-iter-with-org-file' block."
  (assert (boundp 'ar/org-iter--with-block)
          nil
          "Not in `ar/org-iter-with-org-file' block"))

(defmacro ar/org-iter-with-org-file (org-file-path &rest body)
  "With ORG-FILE-PATH, execute BODY."
  `(let ((ar/org-iter--with-block t))
     (ar/file-with-current-file ,org-file-path
       ,@body)))

(defun ar/org-iter-heading-1-markers ()
  "Get heading 1 markers.
Should be called within `ar/org-iter-with-org-file' body."
  (ar/org-iter--assert-org-mode)
  (ar/org-iter--assert-with-block)
  (let (markers)
    (ar/org-iter-for-each-heading-1
     (lambda (heading)
       (add-to-list 'markers (point-marker) t)))
    markers))

(defun ar/org-iter-for-each-heading-1 (fun)
  "For each heading 1, call FUN.
FUN should `save-excursion' if moving point."
  (assert (functionp fun) nil "FUN must be a function")
  (ar/org-iter--assert-org-mode)
  (ar/org-iter--assert-with-block)
  (goto-char 0)
  ;; TODO: Use org-element functions.
  (outline-next-heading)
  (assert (org-on-heading-p) nil "No heading found")
  (funcall fun (org-element-headline-parser (point-max)))
  ;; TODO: Use org-element functions.
  (while (outline-get-next-sibling)
    (assert (org-on-heading-p) nil "Unexpected item found")
    (funcall fun (org-element-headline-parser (point-max)))))

;; Copied from org-element-link-successor, no longer available in org-element.
;; TODO: Migrate to newer APIs.
(defun ar/org-iter--link-successor (limit)
  "Search for the next link and return position.
LIMIT bounds the search.
Return value is a cons cell whose car is `link' and cdr is
beginning position."
  (save-excursion
    (let ((link-regexp
           (if org-target-link-regexp
               (concat org-any-link-re "\\|" org-target-link-regexp)
             org-any-link-re)))
      (when (re-search-forward link-regexp limit t)
        (cons 'link (match-beginning 0))))))

(defun ar/org-iter-for-each-link (fun)
  "For each link, call FUN with link as argument.
FUN should `save-excursion' if moving point."
  (ar/org-iter--assert-org-mode)
  (ar/org-iter--assert-with-block)
  (save-excursion
    (save-restriction
      (let ((next-link (ar/org-iter--link-successor (point-max))))
        (while (not (null next-link))
          (goto-char (cdr next-link))
          (let ((link (org-element-link-parser)))
            (funcall fun link)
            (goto-char (org-element-property :end link)))
          (setq next-link (ar/org-iter--link-successor (point-max))))))))

(defun ar/org-iter-for-each-heading-1-link (heading-1-fun link-fun)
  (ar/org-iter--assert-org-mode)
  (ar/org-iter--assert-with-block)
  (ar/org-iter-for-each-heading-1
   (lambda (heading)
     (funcall heading-1-fun heading)
     ;; TODO: Consider using widen instead.
     (save-restriction
       (org-narrow-to-subtree)
       (ar/org-iter-for-each-link link-fun)))))

(ert-deftest ar/org-iter-heading-1-markers-test ()
  (with-temp-buffer
    ;; TODO: Remove org-mode.
    (org-mode)
    (insert "#+TITLE: This is a test\n")
    (insert "* Heading 1-a\n")
    (insert "** Heading 2-a\n")
    (insert "*** Heading 3-a\n")
    (insert "* Heading 1-b\n")
    (insert "** Heading 2-b\n")
    (insert "* Heading 1-c\n")
    (should (= (length
                (let (ar/org-iter--with-block)
                  (ar/org-iter-heading-1-markers)))
               3))))

(ert-deftest ar/org-iter-for-each-link-test ()
  (with-temp-buffer
    ;; TODO: Remove org-mode.
    (org-mode)
    (insert "#+TITLE: This is a test\n")
    (insert "* Heading 1\n")
    (insert "  - [[http://google.com][1]]\n")
    (insert "  - [[http://google.com][2]]\n")
    (insert "  - [[http://google.com][3]]\n")
    (insert "  - [[http://google.com][4]]\n")
    (insert "* Heading 2\n")
    (insert "  - [[http://google.com][5]]\n")
    (message (buffer-string))
    (goto-char 0)
    ;; TODO: Use org-element functions.
    (outline-next-heading)
    (org-narrow-to-subtree)
    (let ((called-count 0)
          (ar/org-iter--with-block))
      (ar/org-iter-for-each-link
       (lambda (link)
         (setq called-count (1+ called-count))))
      (should (= called-count 4)))))

(ert-deftest ar/org-iter-for-each-heading-1-link-test ()
  (with-temp-buffer
    ;; TODO: Remove org-mode.
    (org-mode)
    (insert "#+TITLE: This is a test\n")
    (insert "* Heading 1\n")
    (insert "  - [[http://google.com][1]]\n")
    (insert "  - [[http://google.com][2]]\n")
    (insert "  - [[http://google.com][3]]\n")
    (insert "  - [[http://google.com][4]]\n")
    (insert "* Heading 2\n")
    (insert "  - [[http://google.com][5]]\n")
    (message (buffer-string))
    (goto-char 0)
    ;; TODO: Use org-element functions.
    (outline-next-heading)
    (let ((heading-called-count 0)
          (link-called-count 0)
          (ar/org-iter--with-block))
      (ar/org-iter-for-each-heading-1-link
       (lambda (heading)
         (setq heading-called-count (1+ heading-called-count)))
       (lambda (link)
         (setq link-called-count (1+ link-called-count))))
      (should (= heading-called-count 2))
      (should (= link-called-count 5)))))

(provide 'ar-org-iter)

;;; ar-org-iter.el ends here
