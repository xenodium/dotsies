(package-initialize)

(require 'use-package)

(add-to-list 'load-path "~/.emacs.d/ar")
(add-to-list 'load-path "~/.emacs.d/local")
(add-to-list 'load-path "~/.emacs.d/external")

;; Temporary fix for exporting error:
;; Symbol’s value as variable is void: org-src-lang-modes
(defvar org-src-lang-modes nil)

(message "Running noninteractive? %s" (if noninteractive "yes" "no"))

(load "~/.emacs.d/features/fe-package-extensions.el")
(load "~/.emacs.d/features/fe-libs.el")
(load "~/.emacs.d/features/fe-mac.el")
(load "~/.emacs.d/features/fe-linux.el")
(load "~/.emacs.d/features/fe-org.el")
(load "~/.emacs.d/local/ar-ox-html.el")
(load "~/.emacs.d/local/ar-org-split-export.el")

(use-package whitespace)

(use-package github-theme
  :ensure t
  :config
  (load-theme 'github t)

  (with-eval-after-load 'org-faces
    (set-face-attribute 'org-link nil :underline nil)
    ;; Disable whitespace mode.
    (set-face-attribute 'whitespace-line nil
                        :foreground nil
                        :background nil)))

(use-package org-compat
  :config
  ;; Handle youtube org links in the form of [[youtube:XjKtkEMUYGc][Some description]]
  ;; Based on http://endlessparentheses.com/embedding-youtube-videos-with-org-mode-links.html
  (org-add-link-type
   "youtube"
   (lambda (handle)
     (browse-url (concat "https://www.youtube.com/watch?v=" handle)))
   (lambda (path desc backend)
     (cl-case backend
       (html (format
              "<p style='text-align:center;'>
                    <iframe width='420' height='315' align='middle'
                            src='https://www.youtube.com/embed/W4LxHn5Y_l4?controls=0'
                            allowFullScreen>
                    </iframe>
                 </p>"
              path (or desc "")))
       (latex (format "\href{%s}{%s}" path (or desc "video")))))))
