;;; ar-osx.el --- Mac OS X support.

;;; Commentary:
;; Mac OS X helpers.


;;; Code:

;; Based on http://hints.macworld.com/article.php?story=20050526162847879
;; Convert plists on Mac OS to xml equivalent and open.
(push '(".plist'" . ar/convert-plist-to-xml) auto-mode-alist)

(defun ar/osx-p ()
  "Return t if the system is a Mac OS X machine, nil otherwise."
  (string-equal system-type "darwin"))

(defun ar/osx-convert-plist-to-xml ()
  (interactive)
  (when (string-match "plist"
                      (buffer-string))
    (shell-command-on-region (point-min) (point-max)
                             ;; yes, the temp file is necessary :-(
                             (format "plutil -convert xml1 -o /tmp/temp.plist %s; cat /tmp/temp.plist"
                                     (shell-quote-argument (buffer-file-name)))
                             t t))
  (set-buffer-modified-p nil)
  (nxml-mode))

(defun ar/osx-init ()
  "Perform initializations for Mac OS X."
  (unless (ar/osx-p)
    (error "Loading OS X config on different platform"))
  ;; On Mac, this is effectively fn-M-backspace.
  (bind-key "M-(" #'kill-word)
  ;; Keep menu bar under graphical OS X for fullscreen.
  (when (window-system)
    (menu-bar-mode 1))
  ;; Sets the command (Apple) key as Meta.
  (setq mac-command-modifier 'meta)
  ;; Sets the option (Apple) key also as Meta.
  (setq mac-option-modifier 'meta)
  (setq exec-path (append exec-path '("~/homebrew/bin"
                                      "~/homebrew/Cellar/llvm/HEAD/bin"
                                      "/usr/local/bin"))))

(when (ar/osx-p)
  (ar/osx-init))

(provide 'ar-osx)

;;; ar-osx.el ends here
