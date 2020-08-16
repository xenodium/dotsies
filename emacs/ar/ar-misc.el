;;; ar-misc.el --- Miscellaneous support. -*- lexical-binding: t; -*-

;;; Commentary:
;; Miscellaneous helpers.


;;; Code:

(defun ar/misc-clipboard-to-qr ()
  "Convert text in clipboard to qrcode and display within Emacs."
  (interactive)
  (let ((temp-file (concat (temporary-file-directory) "qr-code")))
    (if (eq 0 (shell-command (format "qrencode -s10 -o %s %s"
                                     temp-file
                                     (shell-quote-argument (current-kill 0)))
                             "*qrencode*"))
        (switch-to-buffer (find-file-noselect temp-file t))
      (error "Error: Could not create qrcode, check *qrencode* buffer"))))

(defcustom ar/misc-financial-symbols nil "Default financial symbols to look up (cons \"title\" \"symbol\")"
  :type 'list
  :group 'ar-misc)

(defun ar/misc-financial-times-lookup-symbol ()
  "Look up tearsheet for symbol at Financial Times."
  (interactive)
  (ivy-read "Symbol: " ar/misc-financial-symbols
            :action (lambda (item)
                      (assert (consp item) nil "List items must be a cons.")
                      (browse-url (format "https://markets.ft.com/data/funds/tearsheet/charts?s=%s"
                                          (cdr item))))))

;; From https://www.reddit.com/r/emacs/comments/b058f8/weekly_tipstricketc_thread/eilbynr
(defun ar/misc-diff-last-2-yanks ()
  "Run ediff on latest two entries in `kill-ring'."
  (interactive)
  ;; Implementation depends on `lexical-binding' being t, otherwise #'clean-up
  ;; will not be saved as closure to `ediff-cleanup-hook' and thus will lose
  ;; reference to itself.
  (let ((a (generate-new-buffer "*diff-yank*"))
        (b (generate-new-buffer "*diff-yank*")))
    (cl-labels ((clean-up ()
                          (kill-buffer a)
                          (kill-buffer b)
                          (remove-hook 'ediff-cleanup-hook #'clean-up)))
      (add-hook 'ediff-cleanup-hook #'clean-up)
      (with-current-buffer a
        (insert (elt kill-ring 0)))
      (with-current-buffer b
        (insert (elt kill-ring 1)))
      (ediff-buffers a b))))

;; https://gist.github.com/syohex/626af66ba3650252b0a2
(defun ar/misc-hash-region (algorithm beg end)
  "Hash region using ALGORITHM with BEG and END endpoints."
  (interactive
   (list
    (completing-read "Hash type: " '(md5 sha1 sha224 sha256 sha384 sha512))
    (if (use-region-p)
        (region-beginning)
      (point-min))
    (if (use-region-p)
        (region-end)
      (point-max))))
  (message "%s: %s"
           algorithm (secure-hash (intern algorithm) (current-buffer) beg end)))

;; From https://www.reddit.com/r/emacs/comments/b5n1yh/weekly_tipstricketc_thread/ejessje?utm_source=share&utm_medium=web2x
(defun ar/misc-list-faces-for-color (color &optional distance)
  "List faces which use COLOR as fg or bg color.

            Accept colors within DISTANCE which defaults to 0."
  (interactive (list (read-color "Color: ")
                     (and current-prefix-arg
                          (prefix-numeric-value current-prefix-arg))))
  (with-help-window (get-buffer-create (format " *%s*" this-command))
    (dolist (face (sort
                   (ar/misc--list-faces-for-color color distance)
                   (lambda (f1 f2)
                     (string< (symbol-name f1)
                              (symbol-name f2)))))
      (ar/misc-list-faces-print-face face)
      (terpri))))

(defun ar/misc-list-faces-print-face (face)
  "Print face and its parents if any."
  (with-current-buffer standard-output
    (let ((fchain (cdr (ar/list-faces--inheritance-chain face :foreground)))
          (bchain (cdr (ar/list-faces--inheritance-chain face :background))))
      (insert (propertize (format "%s" face) 'face face))
      (cond (fchain
             (dolist (face fchain)
               (insert " > " (propertize (format "%s" face) 'face face))))
            (bchain
             (dolist (face bchain)
               (insert " > " (propertize (format "%s" face) 'face face))))))))

(defun ar/misc--list-faces-inheritance-chain (face attr)
  "Return inheritence change for face and attr."
  (let ((g (face-attribute face attr)))
    (if (and (stringp g)
             (not (string= "unspecified" g)))
        (list face)
      (let ((inherit (face-attribute face :inherit)))
        (when inherit
          (if (facep inherit)
              (cons face
                    (ar/misc--list-faces-inheritance-chain inherit attr))
            (if (consp inherit)
                (cl-dolist (face inherit)
                  (let ((res nil))
                    (when (and (facep face)
                               (setq res (ar/misc--list-faces-inheritance-chain face attr)))
                      (cl-return res)))))))))))

(defun ar/misc--list-faces-attribute (face attr)
  "Get face attribute of face as defined or inherited."
  (let* ((chain (ar/list-faces--inheritance-chain face attr)))
    (cl-dolist (f (nreverse chain))
      (let ((g (face-attribute f attr)))
        (when (and (stringp g)
                   (not (string= "unspecified" g)))
          (cl-return g))))))

(defun ar/misc--list-faces-for-color (color &optional distance)
  "Return all faces with COLOR as fg or bg withing DISTANCE."
  (let ((faces ())
        (distance (or distance 0)))
    (mapatoms (lambda (atom)
                (when (facep atom)
                  (let ((fg (ar/misc--list-faces-attribute atom :foreground))
                        (bg (ar/misc--list-faces-attribute atom  :background)))
                    (when (or (and fg
                                   (<= (color-distance
                                        fg
                                        color)
                                       distance))
                              (and bg
                                   (<= (color-distance
                                        bg
                                        color)
                                       distance)))
                      (push atom faces))))))
    (delete-dups faces)))

(defun ar/misc-pick-font ()
  (interactive)
  (let ((font-name (completing-read "Select font:"
                                    (font-family-list))))
    (if (member font-name (font-family-list))
        (set-face-attribute 'default nil :font font-name)
      (error "'%s' font not found" font-name))))

(provide 'ar-misc)

;;; ar-misc.el ends here
