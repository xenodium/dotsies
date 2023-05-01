;;; ar-misc.el --- Miscellaneous support. -*- lexical-binding: t; -*-

;;; Commentary:
;; Miscellaneous helpers.


;;; Code:

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

(defun ar/misc-pick-font ()
  (interactive)
  (let ((font-name (completing-read "Select font:"
                                    (font-family-list))))
    (if (member font-name (font-family-list))
        (set-face-attribute 'default nil :font font-name)
      (error "'%s' font not found" font-name))))


(defun ar/misc-open-file-at-point ()
  "Open the file path at point.
If there is text selection, uses the text selection for path.
If the path starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
Path may have a trailing “:‹n›” that indicates line number.
If so, jump to that line number.
If path does not have a file extention, automatically try with “.el” for elisp
files.
This command is similar to `find-file-at-point' but without prompting for
confirmation.

URL `http://ergoemacs.org/emacs/emacs_open_file_path_fast.html'"
  (interactive)
  (let ((ξpath (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (let (p0 p1 p2)
                   (validate-setq p0 (point))
                   ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
                   (skip-chars-backward "^  \"\t\n`'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\`")
                   (validate-setq p1 (point))
                   (goto-char p0)
                   (skip-chars-forward "^  \"\t\n`'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\'")
                   (validate-setq p2 (point))
                   (goto-char p0)
                   (buffer-substring-no-properties p1 p2)))))
    (if (string-match-p "\\`https?://" ξpath)
        (browse-url ξpath)
      (progn ; not starting “http://”
        (if (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\'" ξpath)
            (progn
              (let (
                    (ξfpath (match-string 1 ξpath))
                    (ξline-num (string-to-number (match-string 2 ξpath))))
                (if (file-exists-p ξfpath)
                    (progn
                      (find-file ξfpath)
                      (goto-char 1)
                      (forward-line (1- ξline-num)))
                  (progn
                    (when (y-or-n-p (format "File doesn't exist: %s.  Create? " ξfpath))
                      (find-file ξfpath))))))
          (progn
            (if (file-exists-p ξpath)
                (find-file ξpath)
              (if (file-exists-p (concat ξpath ".el"))
                  (find-file (concat ξpath ".el"))
                (when (y-or-n-p (format "File doesn't exist: %s.  Create? " ξpath))
                  (find-file ξpath ))))))))))

;; From http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun ar/misc-narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or defun,
whichever applies first. Narrowing to org-src-block actually
calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is
already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if you
         ;; don't want it.
         (cond ((ignore-errors (org-edit-src-code))
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

(defun ar/misc-open-clipboard-file ()
  "Open clipboard file URL."
  (interactive)
  (find-file (current-kill 0)))

(defun ar/misc-download-clipboard-url ()
  "Download clipboard URL to ~/Downloads."
  (interactive)
  (let ((url (current-kill 0)))
    (cl-assert (string-match-p "^http[s]?://\\(www\\.\\)?" url)
            nil "Not a downloadable URL: %s" url)
    (url-copy-file url (concat (expand-file-name "~/Downloads/")
                               (file-name-nondirectory url)))))

(provide 'ar-misc)

;;; ar-misc.el ends here
