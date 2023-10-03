;;; color-picker.el --- Color picker support.

;;; Commentary:
;; Color picker helpers.

;; Straight from https://gist.github.com/a3ammar/2357d6115ddd999c23e6

;;; Code:

(defun custom-color--choose-action (widget &optional _event) ; this function is only needed if you want to use color-picker in Easy Customization
  "customize `widget-color--chose-action' to not split the screen"
  (list-colors-display
   nil nil
   `(lambda (color)
      (when (buffer-live-p ,(current-buffer))
	(widget-value-set ',(widget-get widget :parent) color)
        (pop-to-buffer ,(current-buffer))))))

(defun nscolor2hex (color)
  "Converts colors from `NSColor' format to hex values"
  (concat "#"                           ; Add # at the front
          (mapconcat 'identity          ; concatenate the list
                     (mapcar '(lambda (x) ;returns ("hex" "hex" "hex")
                                (let ((col (lsh (string-to-number x) -8)))
                                  (if (< col 16)
                                      (format "0%x" col)
                                    (format "%x" col))))
                             (split-string (s-replace "\"" "" color) ",")) "")))

(defun color-picker (&optional list buffer-name callback)
  "Calls OS X color picker and insert the chosen color. It is really messy because of applyscript"
  (interactive)
  (let ((result
         (do-applescript "tell application \"Emacs\"
set result to \"\"
set x to (choose color)
set result to item 1 of x as string
set result to result & \",\"
set result to result & item 2 of x as string
set result to result & \",\"
set result to result & item 3 of x as string
return result
end tell")))
    (if callback ; For Easy Customization
        (funcall callback (nscolor2hex result))
      (message "Copied %s" (nscolor2hex result))
      (kill-new (nscolor2hex result)))
    (do-applescript "tell application \"Emacs\" to activate")))

(provide 'color-picker)

;;; color-picker.el ends here
