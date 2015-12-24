;;; ar-imagemagick.el --- Imagemagick support

;;; Commentary:
;; Imagemagick helpers


;;; Code:

(require 'ar-process)
(require 'cl)

(defun ar/imagemagick-to-grayscale ()
  "Convert current buffer image to grayscale."
  (interactive)
  (ar/imagemagick--assert-prerequisites)
  (ar/process-call "mogrify" "-type" "Grayscale" (buffer-file-name))
  (revert-buffer t t))

(defun ar/imagemagick-auto-orient ()
  "Auto orient buffer image."
  (interactive)
  (ar/imagemagick--assert-prerequisites)
  (ar/process-call "mogrify" "-auto-orient" (buffer-file-name))
  (revert-buffer t t))

(defun ar/imagemagick--assert-prerequisites ()
  "Assert `image-mode'."
  (assert (eq major-mode 'image-mode) nil "Not in image-mode")
  (ar/process-assert-binary-installed "mogrify" "Install with: brew install imagemagick or apt-get imagemagick"))

(provide 'ar-imagemagick)

;;; ar-imagemagick.el ends here
