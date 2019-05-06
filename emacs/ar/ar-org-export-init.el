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
(load "~/.emacs.d/ar/ar-ox-html.el")
(load "~/.emacs.d/ar/ar-org-split-export.el")

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
