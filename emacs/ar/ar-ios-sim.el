;;; ar-ios-sim.el -*- lexical-binding: t; -*-

;;; Commentary:
;; iOS simulator helpers.


;;; Code:

(require 'f)
(require 'dash)
(require 'simple)
(require 'cl-lib)

(defun ar/ios-sim-booted-id ()
  "Booted simulator ID."
  (nth 0 (split-string (shell-command-to-string "xcrun simctl list devices | grep Booted | sed -n 's/^.*\\([A-F0-9]\\{8\\}-\\([A-F0-9]\\{4\\}-\\)\\{3\\}[A-F0-9]\\{12\\}\\).*$/\\1/p'") "\n" t)))

(defun ar/ios-sim-package-id-in-dir (dir)
  "Get package ID from directory."
  (nth 0 (process-lines "/usr/libexec/PlistBuddy"
                        "-c" "Print :MCMMetadataIdentifier" (format "%s/.com.apple.mobile_container_manager.metadata.plist" dir))))

(defun ar/ios-sim-app-directories ()
  "Get all app directories for booted simulator."
  (let ((booted-sim-id (ar/ios-sim-booted-id)))
    (cl-assert booted-sim-id nil "No booted simulator")
    (f-directories (expand-file-name (format "~/Library/Developer/CoreSimulator/Devices/%s/data/Containers/Data/Application" booted-sim-id)))))

(defun ar/ios-sim-app-bundle-ids ()
  "Get all app bundle IDs for booted simulator."
  (-map (lambda (dir)
          (ar/ios-sim-package-id-in-dir dir))
        (ar/ios-sim-app-directories)))

(defun ar/ios-sim-app-directory (bundle-id)
  "Get app directory from BUNDLE-ID."
  (-first (lambda (dir)
            (equal bundle-id
                   (ar/ios-sim-package-id-in-dir dir)))
          (ar/ios-sim-app-directories)))

(defvar ar/ios-sim--last-bundle-id nil)

(defun ar/ios-sim-documents-dir (arg)
  "Show contents of documents directory for app. With ARG, show completion."
  (interactive "P")
  (let* ((bundle-id (if (or arg (not ar/ios-sim--last-bundle-id))
                        (completing-read "Bundle ID: " (ar/ios-sim-app-bundle-ids))
                      (read-string
                       (if ar/ios-sim--last-bundle-id
                           (format "Bundle ID (%s): " ar/ios-sim--last-bundle-id)
                         "Bundle ID: "))))
         (path (ar/ios-sim-app-directory (if (> (length bundle-id) 0)
                                             bundle-id
                                           ar/ios-sim--last-bundle-id))))
    (cl-assert path nil "Not found")
    (when (> (length bundle-id) 0)
      (setq ar/ios-sim--last-bundle-id bundle-id))
    (find-file path)))

(defun ar/counsel-apple-search ()
  "Ivy interface for dynamically querying apple.com docs."
  (interactive)
  (require 'request)
  (require 'json)
  (ivy-read "apple docs: "
            (lambda (input)
              (let* ((url (url-encode-url (format "https://developer.apple.com/search/search_data.php?q=%s" input)))
                     (c1-width (round (* (- (window-width) 9) 0.3)))
                     (c2-width (round (* (- (window-width) 9) 0.5)))
                     (c3-width (- (window-width) 9 c1-width c2-width)))
                (or
                 (ivy-more-chars)
                 (let ((request-curl-options (list "-H" (string-trim (url-http-user-agent-string)))))
                   (request url
                     :type "GET"
                     :parser 'json-read
                     :success (cl-function
                               (lambda (&key data &allow-other-keys)
                                 (ivy-update-candidates
                                  (mapcar (lambda (item)
                                            (let-alist item
                                              (propertize
                                               (format "%s   %s   %s"
                                                       (truncate-string-to-width (propertize (or .title "")
                                                                                             'face '(:foreground "yellow")) c1-width nil ?\s "…")
                                                       (truncate-string-to-width (or .description "") c2-width nil ?\s "…")
                                                       (truncate-string-to-width (propertize (string-join (or .api_ref_data.languages "") "/")
                                                                                             'face '(:foreground "cyan1")) c3-width nil ?\s "…"))
                                               'url .url)))
                                          (cdr (car data)))))))
                   0))))
            :action (lambda (selection)
                      (browse-url (concat "https://developer.apple.com"
                                          (get-text-property 0 'url selection))))
            :dynamic-collection t
            :caller 'ar/counsel-apple-search))

(provide 'ar-ios-sim)

;;; ar-ios-sim.el ends here
