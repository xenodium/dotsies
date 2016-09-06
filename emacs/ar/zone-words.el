;;; zone-words.el --- Discover words to describe emotions while you zone out.

;;; Commentary:
;; Discover words to describe emotions while you zone out.


;;; Code:

(require 'zone)
(require 'zone-words-emotions-dictionary)

(defvar zone-words-top-margin 2)
(defvar zone-words-left-margin 5)
(defvar zone-words-word-definition-margin 1)
(defvar zone-words-word-pause 10)
(defvar zone-words--word-lookup-func nil)
(defvar zone-words-word-word-wrap 75)

(defface zone-words--word-face
  '((((class color) (background light))
     :foreground "DarkGrey" :weight bold :height 2.0 :inherit variable-pitch)
    (((class color) (background dark))
     :foreground "white" :weight bold :height 2.5 :inherit variable-pitch)
    (t :height 1.5 :weight bold :inherit variable-pitch))
  "Face for title" :group 'zone-words)

(defun zone-words-preview ()
  "Previews `zone-words'."
  (interactive)
  (let ((zone-programs [zone-words]))
    (zone)))

(defun zone-words ()
  "Display words to describe your emotions while you zone out."
  (delete-other-windows)
  (setq mode-line-format nil)
  (zone-fill-out-screen (window-width) (window-height))
  (while (not (input-pending-p))
    (delete-region (point-min) (point-max))
    (goto-char (point-min))
    (zone-words--insert-vertical-space zone-words-top-margin)
    (let ((term (zone-words--word-lookup)))
      (insert (zone-words-word--position-text (car term)))
      (add-text-properties (line-beginning-position) (point)
                           (list 'face 'zone-words--word-face))
      (zone-words--insert-vertical-space zone-words-word-definition-margin)
      (insert (zone-words-word--position-text (cdr term))))
    (zone-park/sit-for (point-min) zone-words-word-pause)))

(defun zone-words-word--position-text (text)
  "Indent and wrap TEXT using `zone-words-left-margin' and `zone-words-word-word-wrap'."
  (zone-words-word--indent-text zone-words-left-margin
                                (zone-words-word--word-wrap
                                 text zone-words-word-word-wrap)))

(defun zone-words-word--indent-text (len text)
  "Indent by LEN characters, given TEXT."
  (replace-regexp-in-string "^"
                            (zone-words-word--string-repeat " " len)
                            text))

;; From s.el.
(defun zone-words-word--word-wrap (s len)
  "If S is longer than LEN, wrap the words with newlines."
  (with-temp-buffer
    (insert s)
    (let ((fill-column len))
      (fill-region (point-min) (point-max)))
    (buffer-substring-no-properties (point-min) (point-max))))

;; From s.el.
(defun zone-words-word--string-repeat (s num)
  "Make a string of S repeated NUM times."
  (let (ss)
    (while (> num 0)
      (setq ss (cons s ss))
      (setq num (1- num)))
    (apply 'concat ss)))

(defun zone-words--word-lookup ()
  "Look up a term and return a cons with term and definition."
  (if (functionp zone-words--word-lookup-func)
      (funcall zone-words--word-lookup-func)
    (let* ((word (zone-words-emotions-dictionary-lookup-emotion))
           (definition (if (locate-file "wn" exec-path)
                           (shell-command-to-string (format "wn %s -over" word))
                         "\n\nFor the definition, you need wordnet installed  on your machine.\n\nInstall with:\n\nbrew install wordnet (Mac OS)\n\napt-get install wordnet (Linux)")
             ))
      (cons word definition))))

(defun zone-words--insert-vertical-space (n)
  "Insert vertical space (ie. N new lines)."
  (when (< n 1)
    (error "Argument must be positive"))
  (while (> n 0)
    (insert "\n")
    (setq n (- n 1))))

(provide 'zone-words)

;;; zone-words.el ends here
