;;; ar-helm-hotspots-config.el --- Helm hotspots config.

;;; Commentary:
;; Helm org hotspots config helpers.


;;; Code:

(require 'ar-dired)
(require 'ar-org)
(require 'helm-buffers)
(require 'helm-for-files)

(defvar ar/helm-hotspots-config--local-source
  '((name . "Local")
    (candidates . (("Active" . ar/dired-split-active-to-current)
                   ("Blog" . "~/stuff/active/blog/index.org")
                   ("Downloads" . ar/dired-split-downloads-to-current)
                   ("Desktop" . ar/dired-split-desktop-to-current)
                   ("Init" . "~/stuff/active/code/dots/emacs/init.el")
                   ("Private" . "~/stuff/active/non-public/private.org")
                   ("Xcode Derived Data" . "~/Library/Developer/Xcode/DerivedData")
                   ("iPhone Simulator SDK" . "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator.sdk")
                   ("iPhone SDK" . "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk")
                   ("iPhone Simulator Devices" . "~/Library/Developer/CoreSimulator/Devices")
                   ("Yasnippets" . "~/.emacs.d/yasnippets")))
    (action . (("Open" . (lambda (item)
                           (if (functionp item)
                               (funcall item)
                             (ar/org-open-file-special-path item))))))))

(defvar ar/helm-hotspots-config--web-source
  '((name . "Web")
    (candidates . (("Github" . "https://github.com/xenodium")
                   ("Pinboard" . "https://www.pinterest.com/alvaro1192/wheretogo")
                   ("Twitter" . "http://twitter.com/xenodium")))
    (action . (("Open" . (lambda (url)
                           (browse-url url)))))))

(defvar ar/helm-hotspots-config--blog-source
  `((name . "Blog")
    (candidates . ,(ar/helm-org-candidates "~/stuff/active/blog/index.org"
                                           "^\\* \\["))
    (action . (lambda (candidate)
                (ar/org-goto-marker candidate)))))

(defvar ar/helm-hotspots-config--private-source
  `((name . "Private")
    (candidates . ,(ar/helm-org-candidates "~/stuff/active/non-public/private.org"
                                           "^\\* \\["))
    (action . (lambda (candidate)
                (ar/org-goto-marker candidate)))))
;; Append with:
;; (ar/alist-append-to-value ar/helm-hotspots-config--web-source
;;                           'candidates
;;                           '("Google Play" . "https://play.google.com/music/listen?u=my@gmail.com"))

(defvar ar/helm-hotspots-config--sources '(helm-source-buffers-list
                                           ar/helm-hotspots-config--local-source
                                           ar/helm-hotspots-config--web-source
                                           helm-source-recentf
                                           helm-source-files-in-current-dir
                                           helm-source-file-cache
                                           helm-source-buffer-not-found))

(defvar ar/helm-hotspots-config--advanced-sources '(helm-source-buffers-list
                                                    ar/helm-hotspots-config--local-source
                                                    ar/helm-hotspots-config--web-source
                                                    ar/helm-hotspots-config--blog-source
                                                    ar/helm-hotspots-config--private-source
                                                    helm-source-ido-virtual-buffers
                                                    helm-source-buffer-not-found))

(defun ar/helm-hotspots (p)
  "Show my hotspots."
  (interactive "P")
  (unless helm-source-buffers-list
    (setq helm-source-buffers-list
          (helm-make-source "Buffers" 'helm-source-buffers)))
  (helm :sources (cond (p
                        ar/helm-hotspots-config--advanced-sources)
                       (t
                        ar/helm-hotspots-config--sources))
        :buffer "*helm buffers*"
        :keymap helm-buffer-map
        :truncate-lines t))

(provide 'ar-helm-hotspots-config)

;;; ar-helm-hotspots-config.el ends here
