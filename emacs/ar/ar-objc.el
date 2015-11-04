;;; ar-objc.el --- Objective-C support.

;;; Commentary:
;; Objective-C helpers.


;;; Code:

(require 'ar-buffer)
(require 'ar-file)

(defun ar/objc--own-header-p (header)
  "Check if HEADER is own header (ie. file.m including file.h."
  (string-equal (file-name-base (buffer-file-name))
                (file-name-base header)))

(defun ar/objc--own-header ()
  "Return own header name.

file.h -> file.h
file.m -> file.h"
  (concat (file-name-base (buffer-file-name)) ".h"))

(defun ar/objc--header-from-line (line)
  "Return a header name from LINE of code.

#include \"hello.h\" => hello.h"
  (when (string-match "[<\"]\\(.+?\\)[>\"]" line)
    (match-string 1 line)))

(defun ar/objc--directive-from-line (line)
  "Return a compiler directive (ie. #include) from LINE of code."
  (when (string-match "^#\\([a-z]+\\)" line)
    (match-string 1 line)))

(defun ar/objc--directives-from-block (block)
  "Return a list of compiler directives from a BLOCK of text."
  (mapcar #'ar/objc--directive-from-line (split-string block "\n")))

(defun ar/objc--headers-from-block (block)
  "Return a list of headers from a BLOCK of text."
  (mapcar #'ar/objc--header-from-line (split-string block "\n")))

(defun ar/objc--valid-header-block-p (directive block)
  "Return t if new header with DIRECTIVE can be inserted in BLOCK, nil otherwise."
  (and (member directive
               (ar/objc--directives-from-block block))
       (not (member (ar/objc--own-header)
                    (ar/objc--headers-from-block block)))))

(defun ar/objc--new-include-pos (directive)
  "Buffer position for adding a new import or include DIRECTIVE."
  (let ((header-blocks (ar/buffer-groups-of (format "^#%s.*" directive)))
        (include-pos nil)
        (index 0))
    (while (and header-blocks
                (not include-pos)
                (< index (length header-blocks)))
      (when (ar/objc--valid-header-block-p directive
                                           (nth index header-blocks))
        (setq include-pos
              (ar/buffer-last-match-end (nth index header-blocks))))
      (setq index (1+ index)))
    (when (and (not include-pos) (> (length header-blocks) 0))
      ;; Need a new block, below own include.
      (goto-char (ar/buffer-last-match-end (nth 0 header-blocks)))
      (insert "\n")
      (setq include-pos (point)))
    include-pos))

(defun ar/objc--insert-new (directive header)
  "Insert new DIRECTIVE (include/import) and HEADER (ie. hello.h)."
  (save-excursion
    (let ((insert-pos (ar/objc--new-include-pos directive)))
      (if insert-pos
          (progn
            (goto-char insert-pos)
            (insert (format "\n#%s \"%s\"" directive header))
            (ar/buffer-sort-current-block)
            (ar/buffer-remove-region-dups (region-beginning)
                                          ;; Include next line (\n).
                                          (1+ (region-end))))
        (message "Don't know where to insert.")))))

(defun ar/objc-import (prefix filename)
  "Insert a new header, with PREFIX, use #include else #import and FILENAME."
  (interactive "P\nsfile: ")
  (ar/objc--insert-new (if prefix "include" "import")
                       (or filename (read-string "name: "))))

(defun ar/objc-include ()
  "Insert a new header use #include."
  (interactive)
  (ar/objc-import t nil))

(provide 'ar-objc)

;;; ar-objc.el ends here
