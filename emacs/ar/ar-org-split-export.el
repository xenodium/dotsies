(require 'org-element)
(require 'ar-ox-html)
(require 'f)
(require 's)
(require 'cl-lib)

(defun ar/org-split-export-async ()
  (interactive)
  (async-shell-command (concat (expand-file-name invocation-name invocation-directory)
                               " --batch -Q"
                               " -l "(expand-file-name "~/.emacs.d/local/ar-org-export-init.el")
                               " --execute '(ar/org-split-export)'")
                       "*org html export*"))

(defun ar/org-split-export ()
  (with-current-buffer (find-file-noselect (expand-file-name
                                            "~/stuff/active/blog/index.org"))
    (org-element-map (org-element-parse-buffer 'headline)
        'headline
      (lambda (headline)
        (when (org-element-property :CUSTOM_ID headline)
          (ar/org-split-export-headline (expand-file-name "~/stuff/active/blog/index.org")
                                        (ar/org-split-export--parse-headline-title
                                         (org-element-property :raw-value headline))
                                        (org-element-property :CUSTOM_ID headline)
                                        (org-element-property :begin headline))
          ;; Adding a sleep prevents error: "Creating pipe: Too many open files".
          (sleep-for 0.1))))))

(defun ar/org-split-export--parse-headline-title (headline)
  ;; For example:
  ;; [2019-03-30 Sat] Reading spreadsheets with python/pandas
  ;; => Reading spreadsheets with python/pandas
  (let ((match (s-match (rx "[" (= 4 num) "-"(= 2 num) "-" (= 2 num) (= 1 blank)
                            (or "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
                            "]"
                            (= 1 blank)
                            (group (one-or-more anything)))
                        headline)))
    (when (> (length match)
             1)
      (nth 1 match))))

(defun ar/org-export--collect-tree-properties--postprocess (orig-fun &rest r)
  (save-excursion
    (save-restriction
      (widen)
      (org-combine-plists (apply orig-fun r)
                          `(:id-alist ,(org-element-map (org-element-parse-buffer 'headline)
                                           'headline
                                         (lambda (headline)
                                           (cons (org-element-property :CUSTOM_ID headline) "index.org"))))))))


(defun ar/org-export-current-headline-async ()
  "Export current headline to HTML asynchronously."
  (interactive)
  (let ((headline (ar/org-element-at-heading-1)))
    (cl-assert (eq major-mode 'org-mode))
    (async-shell-command (concat (expand-file-name invocation-name invocation-directory)
                                 " --batch -Q"
                                 " -l "(expand-file-name "~/.emacs.d/local/ar-org-export-init.el")
                                 (format " --execute '(ar/org-split-export-headline \"%s\" \"%s\" \"%s\" %d t)'"
                                         (expand-file-name "~/stuff/active/blog/index.org")
                                         ;; TODO: Remove this param and let ar/org-split-export-headline get the value.
                                         (s-replace "'" ""
                                                    (ar/org-split-export--parse-headline-title
                                                     (org-element-property :raw-value headline)))
                                         (org-element-property :CUSTOM_ID headline)
                                         (org-element-property :begin headline)))
                         "*org html export*")))

(defun ar/org-export-current-headline ()
  "Export current headline to HTML."
  (interactive)
  (let ((headline (ar/org-element-at-heading-1)))
    (cl-assert (eq major-mode 'org-mode))
    (ar/org-split-export-headline (expand-file-name "~/stuff/active/blog/index.org")
                                  (ar/org-split-export--parse-headline-title
                                   (org-element-property :raw-value headline))
                                  (org-element-property :CUSTOM_ID headline)
                                  (org-element-property :begin headline)) t))

(defun ar/org-split-export-headline (src-fpath title custom-id location &optional open)
  (message "Exporting %s %s at %s open file? %s" src-fpath custom-id location (if open "yes" "no"))
  (with-current-buffer (find-file-noselect src-fpath)
    (let ((dst-fpath))
      (unless (f-exists-p custom-id)
        (f-mkdir custom-id))
      (setq dst-fpath (f-join custom-id "index.html"))
      (message "Writing %s" dst-fpath)
      (save-excursion
        (save-restriction
          (goto-char location)
          (org-narrow-to-subtree)
          (let (
                ;; (org-export-with-broken-links nil)
                (org-time-stamp-custom-formats
                 '("<%d %B %Y>" . "<%A, %B %d, %Y %H:%M>"))
                (org-display-custom-times t))
            (unwind-protect
                (progn
                  (advice-add 'org-html-link
                              :around
                              'ar/ox-html-link-postprocess)
                  (advice-add 'org-export--collect-tree-properties
                              :around
                              'ar/org-export--collect-tree-properties--postprocess)
                  (advice-add 'org-timestamp-translate
                              :around
                              'ar/ox-html--timestamp-translate-advice-fun)
                  (org-export-to-file 'html dst-fpath nil nil nil nil nil
                                      (lambda (dst-fpath)
                                        (with-current-buffer (find-file-noselect dst-fpath t)
                                          (revert-buffer t t)
                                          (replace-regexp "<title>Álvaro Ramírez</title>"
                                                          (format "<title>%s</title>" title))
                                          (save-buffer)
                                          (kill-buffer (current-buffer)))
                                        (when open
                                          (shell-command (format "open file:%s" (expand-file-name dst-fpath)))))))
              (advice-remove 'org-html-link
                             'ar/ox-html-link-postprocess)
              (advice-remove 'org-timestamp-translate
                             'ar/ox-html--timestamp-translate-advice-fun)
              (advice-remove 'worg-export--collect-tree-properties
                             'ar/org-export--collect-tree-properties--postprocess))))))))

(provide 'ar-org-split-export)
