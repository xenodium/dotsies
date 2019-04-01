(require 'org-element)
(require 'ar-ox-html)
(require 'f)
(require 's)

(defun ar/org-split-export-async ()
  (interactive)
  (async-shell-command (concat (expand-file-name invocation-name invocation-directory)
                               " --batch -Q"
                               " -l "(expand-file-name "~/.emacs.d/ar/ar-org-export-init.el")
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
                                        (org-element-property :CUSTOM_ID headline)
                                        (org-element-property :begin headline))
          ;; Adding a sleep prevents error: "Creating pipe: Too many open files".
          (sleep-for 0.1))))))

(defun ar/org-html-link--postprocess (orig-fun &rest r)
  (let ((html-link (apply orig-fun r)))
    ;; Massage href from:
    ;; <a href=\"index.html#ID-trying-out-tesseract\"></a>
    ;; to:
    ;; <a href=\"../trying-out-tesseract\"></a>
    (setq html-link (s-replace-regexp "href=\\\"\\(.*#ID-\\)" "../"
                                      html-link nil nil 1))
    ;; Massage image from:
    ;; <img src=\"images/inserting-numbers-with-emacs-multiple-cursors/mc-number.gif\"></img>
    ;; to:
    ;; <img src=\"../images/inserting-numbers-with-emacs-multiple-cursors/mc-number.gif\"></img>
    (setq html-link (s-replace-regexp "src=\\\"\\(images\\)" "../images"
                                      html-link nil nil 1))
    html-link))

(defun ar/org-export--collect-tree-properties--postprocess (orig-fun &rest r)
  (save-excursion
    (save-restriction
      (widen)
      (org-combine-plists (apply orig-fun r)
                          `(:id-alist ,(org-element-map (org-element-parse-buffer 'headline)
                                           'headline
                                         (lambda (headline)
                                           (cons (org-element-property :CUSTOM_ID headline) "index.org"))))))))

(defun ar/org-split-export-headline (src-fpath custom-id location)
  (message "Exporting %s %s at %s" src-fpath custom-id location)
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
                              'ar/org-html-link--postprocess)
                  (advice-add 'org-export--collect-tree-properties
                              :around
                              'ar/org-export--collect-tree-properties--postprocess)
                  (advice-add 'org-timestamp-translate
                              :around
                              'ar/ox-html--timestamp-translate-advice-fun)
                  (org-export-to-file 'html dst-fpath))
              (advice-remove 'org-html-link
                             'ar/org-html-link--postprocess)
              (advice-remove 'org-timestamp-translate
                             'ar/ox-html--timestamp-translate-advice-fun)
              (advice-remove 'worg-export--collect-tree-properties
                             'ar/org-export--collect-tree-properties--postprocess))))))))
