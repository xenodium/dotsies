;;; ar-buffer.el --- Query and manipulate buffer text.

;;; Commentary:
;; Buffer text helpers.


;;; Code:

(require 'ar-string)
(require 'goto-addr)
(require 'url)
(require 'url-http)

(defun ar/buffer-current-dir-path ()
  "Return current buffer directory path."
  (if (equal major-mode 'dired-mode)
      default-directory
    (file-name-directory (buffer-file-name))))

(defun ar/buffer-fetch-url (url)
  "Fetch URL and return as buffer."
  (let* ((url-show-status nil) ;Silence fetch in minibuffer.
         (response-buffer (url-retrieve-synchronously url)))
    (with-current-buffer
        response-buffer
      (unless (> (buffer-size) 0)
        (error "Could not fetch %s" url))
      response-buffer)))

(defun ar/buffer-file-name-equal-p (file-name)
  "Return t if buffer file name equals FILE-NAME."
  (string-equal (file-name-nondirectory (buffer-file-name))
                file-name))

(defun ar/buffer-file-extension-equal-p (extension)
  "Return t if buffer file has EXTENSION."
  (string-equal (file-name-extension (buffer-file-name))
                extension))

(defun ar/buffer-re-string-match-list (re &optional match-fn)
  "Return a list with strings matching RE, optionally using MATCH-FN to extract match (useful to extract groups)."
  (save-excursion
    (goto-char 0)
    (let ((results '()))
      (while (search-forward-regexp re nil t)
        (add-to-list 'results (if match-fn
                                  (funcall match-fn)
                                (match-string-no-properties 0))))
      (when (> (length results) 0)
        results))))

(defun ar/buffer-string-match-p (re)
  "Return t if RE matches current buffer.  nil otherwise."
  (goto-char 0)
  (re-search-forward re nil t))

(defun ar/buffer-kill-others ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              ;; Disables "required at runtime" warning for cl package.
              (with-no-warnings
                (remove-if-not 'buffer-file-name (buffer-list))))))

(defun ar/buffer-switch-to-file (file-path)
  "Switch to buffer with FILE-PATH."
  (switch-to-buffer (find-file-noselect (expand-file-name file-path))))

;; Based on http://emacswiki.org/emacs/DuplicateLines
(defun ar/buffer-remove-region-dups (beg end)
  "Remove dups in region's adjacent lines or pass BEG END."
  (interactive "*r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
      (replace-match "\\1"))))

(defun ar/buffer-select-current-line ()
  "Select the current line."
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))

(defun ar/buffer-select-current-block ()
  "Select the current block of text between blank lines.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2015-02-07."
  (interactive)
  (let (p1 p2)
    (if (re-search-backward "\n[ \t]*\n" nil "move")
        (progn (re-search-forward "\n[ \t]*\n")
               (setq p1 (point)))
      (setq p1 (point)))
    (if (re-search-forward "\n[ \t]*\n" nil "move")
        (progn (re-search-backward "\n[ \t]*\n")
               (setq p2 (point)))
      (setq p2 (point)))
    (set-mark p1)))

(defun ar/buffer-sort-lines-ignore-case ()
  "Sort region (case-insensitive)."
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively #'sort-lines)))

(defun ar/buffer-sort-current-block (case-insesitive)
  "Select and sort current block.  With prefix CASE-INSESITIVE is activated."
  (interactive "P")
  ;; Why is save-excursion not working?
  (let ((saved-point (point)))
    (ar/buffer-select-current-block)
    (let ((sort-fold-case case-insesitive))
      (call-interactively #'sort-lines))
    (goto-char saved-point)))

(defun ar/buffer-first-match-beginning (&optional re)
  "Return the first match beginning position for RE.  Nil otherwise."
  (save-excursion
    (goto-char (point-min))
    (when re
      (re-search-forward re nil t))
    (match-beginning 0)))

(defun ar/buffer-occurr-insert ()
  "Search current buffer and insert matching string."
  (interactive)
  (insert (completing-read "occurr insert "
                           (split-string (buffer-substring-no-properties (point-min)
                                                                         (point-max))
                                         "\n"))))

(defun ar/buffer-goto-first-match-beginning (re)
  "Go to first match of RE."
  (goto-char (ar/buffer-first-match-beginning re)))

(defun ar/buffer-last-match-end (re)
  "Return the last match ending position for RE."
  (save-excursion
    (goto-char (point-min))
    (let ((end-pos nil))
      (while (re-search-forward re nil t)
        (setq end-pos (match-end 0)))
      end-pos)))

(defun ar/buffer-lines-matching (re)
  "Return lines in buffer matching RE."
  (save-excursion
    (goto-char (point-min))
    (let ((results '()))
      (while (re-search-forward re nil t)
        (add-to-list 'results (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position))))
      (when (> (length results) 0)
        results))))

(defun ar/buffer-copy-file-name ()
  "Copy the current buffer's file name."
  (interactive)
  (kill-new (file-name-nondirectory (buffer-file-name))))

(defun ar/buffer-groups-of (re)
  "Return a list of any RE consecutive match separated by two or more newlines.

\(ar/buffer-groups-of \"#include\") =>

\(\"#include \"one.h\"\\n#include \"two.h\"\"
 \"#include \"three.h\"\\n#include \"four.h\"\"
 \"#include \"six.h\"\"
 \"#include \"seven.h\"\")

For:

#include \"one.h\"
#include \"two.h\"

#include \"three.h\"
#include \"four.h\"


#include \"six.h\"

#include \"seven.h\""
  (let ((beg-pos (ar/buffer-first-match-beginning re))
        (end-pos (ar/buffer-last-match-end re))
        (substring nil))
    (when (and beg-pos end-pos)
      (setq substring (buffer-substring-no-properties beg-pos
                                                      end-pos))
      ;; Repeat match 2 or more consecutive new lines.
      (split-string substring "\\(\n\\)\\{2,\\}"))))

(provide 'ar-buffer)

;;; ar-buffer.el ends here
