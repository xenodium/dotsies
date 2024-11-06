;;; -*- lexical-binding: t; -*-

(use-package ar-image
  ;; Generate and open an HTML page with all images in current directory.
  ;; Much nicer for browsing pictures.
  :commands ar/image-open-html-for-current-dir)

(use-package image-crop
  :commands ar/image-crop
  :bind (:map image-mode-map
              ("i c" . ar/image-crop))
  :config
  ;; Like Sacha Chua's, but automatically generating unique filename.
  ;; https://sachachua.com/blog/2024/11/emacs-extract-part-of-an-image-to-another-file
  (defun ar/image-crop ()
    "Copy a section of the image under point to a different file.
This command presents the image with a rectangular area superimposed
on it, and allows moving and resizing the area to define which
part of it to crop.

While moving/resizing the cropping area, the following key bindings
are available:

`q':   Exit without changing anything.
`RET': Save the image.
`m':   Make mouse movements move the rectangle instead of altering the
       rectangle shape.
`s':   Same as `m', but make the rectangle into a square first."
    (interactive)
    (when-let* ((orig-data (buffer-string))
                (area (ar/image-select-rect "write"))
                (inhibit-read-only t)
                (type (image-crop--content-type orig-data))
                (left (plist-get area :left))
                (top (plist-get area :top))
                (width (plist-get area :width))
                (height (plist-get area :height))
                (new-name (if (buffer-file-name)
                              (progn
                                (require 'dwim-shell-command)
                                (dwim-shell-command--unique-new-file-path
                                 (buffer-file-name)))
                            (read-file-name "File: "))))
      (with-temp-file new-name
        (set-buffer-multibyte nil)
        (insert orig-data)
        (image-crop--process image-crop-crop-command
                             `((?l . ,left)
                               (?t . ,top)
                               (?w . ,width)
                               (?h . ,height)
                               (?f . ,(cadr (split-string type "/"))))))
      (find-file new-name)))

  (defun ar/image-select-rect (op)
    "Select a region of the current buffer's image.

`q':   Exit without changing anything.
`RET': Select this region.
`m':   Make mouse movements move the rectangle instead of altering the
       rectangle shape.
`s':   Same as `m', but make the rectangle into a square first."
    (unless (image-type-available-p 'svg)
      (error "SVG support is needed to crop and cut images"))
    (let ((image (image--get-image)))
      (unless (imagep image)
        (user-error "No image under point"))
      (when (overlays-at (point))
        (user-error "Can't edit images that have overlays"))
      ;; We replace the image under point with an SVG image that looks
      ;; just like that image.  That allows us to draw lines over it.
      ;; At the end, we replace that SVG with a cropped version of the
      ;; original image.
      (let* ((data (cl-getf (cdr image) :data))
             (type (cond
                    ((cl-getf (cdr image) :format)
                     (format "%s" (cl-getf (cdr image) :format)))
                    (data
                     (image-crop--content-type data))))
             (image-scaling-factor 1)
             (orig-point (point))
             (size (image-size image t))
             (svg (svg-create (car size) (cdr size)
                              :xmlns:xlink "http://www.w3.org/1999/xlink"
                              :stroke-width 5))
             ;; We want to get the original text that's covered by the
             ;; image so that we can restore it.
             (image-start
              (save-excursion
                (let ((match (text-property-search-backward 'display image)))
                  (if match
                      (prop-match-end match)
                    (point-min)))))
             (image-end
              (save-excursion
                (let ((match (text-property-search-forward 'display image)))
                  (if match
                      (prop-match-beginning match)
                    (point-max)))))
             (text (buffer-substring image-start image-end))
             (inhibit-read-only t)
             orig-data svg-end)
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (if (null data)
              (insert-file-contents-literally (cl-getf (cdr image) :file))
            (insert data))
          (let ((image-crop-exif-rotate nil))
            (image-crop--possibly-rotate-buffer image))
          (setq orig-data (buffer-string))
          (setq type (image-crop--content-type orig-data))
          (image-crop--process image-crop-resize-command
                               `((?w . 600)
                                 (?f . ,(cadr (split-string type "/")))))
          (setq data (buffer-string)))
        (svg-embed svg data type t
                   :width (car size)
                   :height (cdr size))
        (with-temp-buffer
          (svg-insert-image svg)
          (switch-to-buffer (current-buffer))
          (setq svg-end (point))
          ;; Area
          (let ((area
                 (condition-case _
                     (save-excursion
                       (forward-line 1)
                       (image-crop--crop-image-1
                        svg op))
                   (quit nil))))
            (when area
              ;;  scale to original
              (let* ((image-scaling-factor 1)
                     (osize (image-size (create-image orig-data nil t) t))
                     (factor (/ (float (car osize)) (car size)))
                     ;; width x height + left + top
                     (width (abs (truncate (* factor (- (cl-getf area :right)
                                                        (cl-getf area :left))))))
                     (height (abs (truncate (* factor (- (cl-getf area :bottom)
                                                         (cl-getf area :top))))))
                     (left (truncate (* factor (min (cl-getf area :left)
                                                    (cl-getf area :right)))))
                     (top (truncate (* factor (min (cl-getf area :top)
                                                   (cl-getf area :bottom))))))
                (list :left left :top top
                      :width width :height height
                      :right (+ left width)
                      :bottom (+ top height))))))))))
