;;; ar-hammerspoon.el --- Hammerspoon support.

;;; Commentary:
;; Hammerspoon helpers.


;;; Code:

(defun ar/hammerspoon-repl ()
  "Start a hammerspoon repl."
  (interactive)
  (if (get-buffer "*hammerspoon*")
      (switch-to-buffer "*hammerspoon*")
    (ansi-term "/Applications/Hammerspoon.app/Contents/Resources/extensions/hs/ipc/bin/hs"
               "hammerspoon")))

(provide 'ar-hammerspoon)

;;; ar-hammerspoon.el ends here
