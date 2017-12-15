;;; ar-ios-sim.el --- iOS simulator support.

;;; Commentary:
;; iOS simulator helpers.


;;; Code:

(require 'f)
(require 'dash)
(require 'simple)

(defun ar/ios-sim-booted-id ()
  "Booted simulator ID."
  (nth 0 (split-string (shell-command-to-string "xcrun simctl list devices | grep Booted | sed -n 's/^.*\\([A-F0-9]\\{8\\}-\\([A-F0-9]\\{4\\}-\\)\\{3\\}[A-F0-9]\\{12\\}\\).*$/\\1/p'") "\n" t)))

(defun ar/ios-sim-package-id-in-dir (dir)
  "Get package ID from directory."
  (nth 0 (process-lines "/usr/libexec/PlistBuddy"
                        "-c" "Print :MCMMetadataIdentifier" (format "%s/.com.apple.mobile_container_manager.metadata.plist" dir))))

(defun ar/ios-sim-app-directories ()
  "Get all app directories for booted simulator."
  (f-directories (expand-file-name (format "~/Library/Developer/CoreSimulator/Devices/%s/data/Containers/Data/Application" (ar/ios-sim-booted-id)))))

(defun ar/ios-sim-app-bundle-ids ()
  "Get all app bundle IDs for booted simulator."
  (-map (lambda (dir)
          (ar/ios-sim-package-id-in-dir dir))
        (ar/ios-sim-app-directories)))

(defvar ar/ios-sim--last-bundle-id nil)

(defun ar/ios-sim-documents-dir (arg)
  (interactive "P")
  (let* ((bundle-id (if (or arg (not ar/ios-sim--last-bundle-id))
                        (completing-read "Bundle ID: " (ar/ios-sim-app-bundle-ids))
                      (read-string (format "Bundle ID%s: "
                                           (if ar/ios-sim--last-bundle-id
                                               (format " (%s)" ar/ios-sim--last-bundle-id)
                                             "")) )))
         (path (-first (lambda (dir)
                         (equal (if (> (length bundle-id) 0)
                                    bundle-id
                                  ar/ios-sim--last-bundle-id)
                                (ar/ios-sim-package-id-in-dir dir)))
                       (ar/ios-sim-app-directories))))
    (assert path nil "Not found")
    (when (> (length bundle-id) 0)
      (setq ar/ios-sim--last-bundle-id bundle-id))
    (find-file path)))

(provide 'ar-ios-sim)

;;; ar-ios-sim.el ends here
