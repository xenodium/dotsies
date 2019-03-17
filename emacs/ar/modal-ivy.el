(require 'ar-org-iter)
(require 'ivy)
(require 's)

(defvar ar/modal-ivy--bookmarks-source nil)

(defface modal-ivy--col1-face '((t :foreground "grey80" )) "Column 1 face")

(defun ar/modal-ivy-frame (buffer-name f)
  (with-current-buffer (get-buffer-create buffer-name)
    (let ((frame (make-frame '((auto-raise . t)
                               (background-color . "DeepSkyBlue3")
                               (cursor-color . "MediumPurple1")
                               (font . "Menlo 15")
                               (foreground-color . "#eeeeec")
                               (height . 20)
                               (internal-border-width . 20)
                               (left . 0.33)
                               (left-fringe . 0)
                               (line-spacing . 3)
                               (menu-bar-lines . 0)
                               (minibuffer . only)
                               (right-fringe . 0)
                               (tool-bar-lines . 0)
                               (top . 200)
                               (undecorated . t)
                               (unsplittable . t)
                               (vertical-scroll-bars . nil)
                               (width . 110)))))
      (set-face-attribute 'minibuffer-prompt frame
                          :foreground "pink")
      (set-face-attribute 'ivy-minibuffer-match-face-1 frame
                          :background nil
                          :foreground nil)
      (set-face-attribute 'ivy-minibuffer-match-face-2 frame
                          :background nil
                          :foreground "orange1")
      (set-face-attribute 'ivy-minibuffer-match-face-3 frame
                          :background nil
                          :foreground "orange1")
      (set-face-attribute 'ivy-minibuffer-match-face-4 frame
                          :background nil
                          :foreground "orange1")
      (set-face-attribute 'ivy-current-match frame
                          :background "#ffc911"
                          :foreground "red")
      ;; Workaround: Cannot get ivy-read to render properly in frame without delay.
      (run-with-timer 0.001 nil f))))

(defun ar/modal-ivy--load-bookmarks-source ()
  (ar/org-iter-with-org-file
   "~/stuff/active/blog/index.org"
   (let ((sources '()))
     (ar/org-iter-for-each-heading-1
      (lambda (heading)
        (save-restriction
          (org-narrow-to-subtree)
          (ar/org-iter-for-each-link
           (lambda (link)
             (let ((category (replace-regexp-in-string ".*#\\]\\] " ""
                                                       (org-element-property :raw-value heading)))
                   (title (cond ((and (org-element-property :contents-begin link)
                                      (org-element-property :contents-end link))
                                 (buffer-substring-no-properties (org-element-property :contents-begin link)
                                                                 (org-element-property :contents-end link)))
                                (t
                                 (org-element-property :raw-link link)))))
               ;; Filter out useless entries:
               ;; 1. My own headings with # link.
               ;; 2. Links to images (file:).
               (unless (or (string-equal title "#")
                           (string-match-p "^file:" title))
                 (add-to-list 'sources (propertize (format " %s %s "
                                                           ;; Category
                                                           (s-pad-right 40 " "
                                                                        (propertize category
                                                                                    'face
                                                                                    'modal-ivy--col1-face))
                                                           title)
                                                   'url (org-element-property :raw-link link))
                              t))))))))
     sources)))

(defun ar/modal-ivy-reload-bookmarks ()
  (interactive)
  (setq ar/modal-ivy--bookmarks-source
        (ar/modal-ivy--load-bookmarks-source))
  (message "Bookmarks reloaded"))

(defun ar/modal-ivy-search-org-links ()
  "Search all my org links."
  (ar/modal-ivy-frame "*modal-ivy*"
                      (lambda ()
                        (let ((ivy-height 20)
                              (ivy-count-format ""))
                          (ivy-read " " (if ar/modal-ivy--bookmarks-source
                                            ar/modal-ivy--bookmarks-source
                                          (setq ar/modal-ivy--bookmarks-source
                                                (ar/modal-ivy--load-bookmarks-source))
                                          ar/modal-ivy--bookmarks-source)
                                    ;; :sort t
                                    :action (lambda (item)
                                              (browse-url (get-text-property 0 'url item)))
                                    :unwind (lambda ()
                                              ;; Unless we first switch to another macOS app, Emacs will
                                              ;; refocus another frame after deleting the current frame.
                                              (call-process "/Applications/Hammerspoon.app/Contents/Resources/extensions/hs/ipc/bin/hs"
                                                            nil nil nil
                                                            "-c"
                                                            "backFromEmacs()")
                                              (delete-frame)
                                              (other-window 1)))))))

(defun ar/modal-ivy-search-short-links ()
  "Search all my short links."
  (ar/modal-ivy-frame "*modal-ivy*"
                      (lambda ()
                        (let ((ivy-height 20)
                              (ivy-count-format ""))
                          (ivy-read " "
                                    (-map (lambda (link)
                                            ;; Line is formatted as:
                                            ;; Description   URL
                                            (propertize (format " %s %s "
                                                                (s-pad-right 40 " "
                                                                             (propertize (ar/org-link-description link)
                                                                                         'face
                                                                                         'modal-ivy--col1-face))
                                                                (ar/org-link-url link))
                                                        'url (ar/org-link-url link)))
                                          (ar/org-short-links))
                                    :action (lambda (item)
                                              (message "=> %s " (get-text-property 0 'url item))
                                              (browse-url (if (s-starts-with-p "http" (get-text-property 0 'url item))
                                                              (get-text-property 0 'url item)
                                                            (format "http://%s" (get-text-property 0 'url item)))))
                                    :unwind (lambda ()
                                              (delete-frame)))))))

(defun ar/modal-ivy-clipboard ()
  "Search all my org links."
  (ar/modal-ivy-frame "*modal-ivy*"
                      (lambda ()
                        (let ((ivy-height 20)
                              (ivy-count-format ""))
                          (ivy-read "" (mapcar 'substring-no-properties
                                               (counsel--yank-pop-kills))
                                    :require-match t
                                    :action (lambda (item)
                                              (let ((select-enable-clipboard t))
                                                (kill-new item t)
                                                (call-process "/Applications/Hammerspoon.app/Contents/Resources/extensions/hs/ipc/bin/hs"
                                                              nil 0 nil
                                                              "-c"
                                                              (format "hs.eventtap.keyStrokes(\"%s\")" item))))
                                    :unwind (lambda ()
                                              ;; Unless we first switch to another macOS app, Emacs will
                                              ;; refocus another frame after deleting the current frame.
                                              (call-process "/Applications/Hammerspoon.app/Contents/Resources/extensions/hs/ipc/bin/hs"
                                                            nil nil nil
                                                            "-c"
                                                            "backFromEmacs()")
                                              (delete-frame)
                                              (other-window 1)))))))

(provide 'modal-ivy)
