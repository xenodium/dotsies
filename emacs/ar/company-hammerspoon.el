;;; company-hammerspoon.el --- Company Hammerspoon support.

;;; Commentary:
;; Company Hammerspoon helpers.


;;; Code:

(defvar company-hs-bin-path "/Applications/Hammerspoon.app/Contents/Resources/extensions/hs/ipc/bin/hs")

(require 'company)

(defun hammerspoon-shell ()
  (interactive)
  (let ((shell-file-name company-hs-bin-path)
        (shell-command-switch nil))
    (shell)))

(defun company-hs--prefix ()
  (cond ((looking-back "\s" (line-beginning-position))
         "")
        ;; For example foo:
        ((looking-back "[a-zA-Z]+:" (line-beginning-position) t)
         (buffer-substring-no-properties (match-beginning 0)
                                         (match-end 0)))
        ;; For example hs.grid. or hs. or hs
        ((looking-back "\\([a-zA-Z]*\\.\\)*[a-zA-Z]+\\.*"
                       (line-beginning-position) t)
         (buffer-substring-no-properties (match-beginning 0)
                                         (match-end 0)))))

(defun company-hammerspoon (command &optional arg &rest ignored)
  "Complete using hammerspoon instance. See `company''s COMMAND ARG and IGNORED for details."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-hammerspoon))
    (prefix (company-hs--prefix))
    (candidates
     (company-hs--candidates (company-hs--prefix)))
    (post-completion (company-hs--post-complete arg))))

(defun company-hs--post-complete (arg)
  (message (format "completed: %s" arg)))

(defun company-hs--candidates (prefix)
  "Get candidates for PREFIX company completion using `pcomplete'."
  (-map (lambda (item)
          (if (s-starts-with? prefix item)
              item
            nil))
        (json-read-from-string
         (company-hs--candidates-json prefix))))

(defun company-hs--candidates-json (prefix)
  "Candidates for PREFIX."
  (let ((json (shell-command-to-string (format "%s -c \"hs.printf(hs.json.encode(signatureCompletionForText('%s')))\"" company-hs-bin-path prefix))))
    (if (equal json "[\"\"]\n")
        nil
      json)))

(company-hs--candidates-json "hs.grid")

(provide 'company-hammerspoon)

;;; company-hammerspoon.el ends here
