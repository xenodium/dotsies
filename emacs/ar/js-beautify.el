;;; ar-js-beautify.el --- js beautify support.

;;; Commentary:
;; js beautify helpers.


;;; Code:

(require 'ar-process)

;; Clean up alternative.
;; (defun js-beautify-buffer()
;;   "Beautify buffer using js-beautify."
;;   (interactive)
;;   (let* ((buffer (current-buffer))
;;          (temp-file-path (make-temp-file "js-beautify")))
;;     (with-temp-file temp-file-path
;;       (insert-file-contents (buffer-file-name buffer))
;;       (when (js-beautify-file temp-file-path)
;;         (let ((temp-buffer (current-buffer)))
;;           (with-current-buffer buffer
;;             (save-excursion
;;               (buffer-swap-text temp-buffer))))))))

(defun js-beautify-buffer()
  "Beautify buffer using js-beautify."
  (interactive)
  (js-beautify-file (buffer-file-name (current-buffer)))
  (revert-buffer t t))

(defun js-beautify-file (file-path)
  "Js-beautify FILE-PATH."
  (ar/process-assert-binary-installed "js-beautify"
                                      "Install with: npm -g install js-beautify")
  (if (= 0 (call-process "js-beautify"
                         nil "*js-beautify*"
                         t "-r"
                         "-f" file-path))
      t
    (error "Unable to js-beautify %s" file-path)))

(define-minor-mode js-beautify-mode
  "yadda"
  :init-value nil
  :lighter " js beautify"
  (if js-beautify-mode
      (add-hook 'after-save-hook #'js-beautify-buffer t t)
    (remove-hook 'after-save-hook #'
                 js-beautify-buffer)))

(provide 'js-beautify)

;;; js-beautify.el ends here
