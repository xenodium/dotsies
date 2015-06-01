;;; ar-ox-html.el --- Org HTML export support.

;;; Commentary:
;; Org HTML export helpers.


;;; Code:

(require 'ox-html)

(defun ar/ox-html-setup ()
  "Setup org HTML export."
  (setq org-html-preamble t)
  (setq org-html-preamble-format '(("en" "
<table id='contact-header'>
  <tr>
    <td id='contact-left'>
   </td>
    <td id='contact-right'>
      <a href='https://twitter.com/xenodium'>twitter</a>
      <a href='http://github.com/xenodium'>github</a>
      <a href='http://uk.linkedin.com/in/xenodium'>linkedin</a>
      <a href='mailto:me@xenodium.com'>email</a>
    </td>
  </tr>
</table>")))
  (setq org-html-postamble nil)
  (setq org-html-format-drawer-function #'ar/org-html-export-format-drawer))

(defun ar/ox-html-export ()
  "Export blog to HTML."
  (interactive)
  (with-current-buffer (find-file-noselect (expand-file-name
                                            "~/stuff/active/blog/index.org"))
    (org-html-export-to-html)
    (browse-url (format "file:%s" (expand-file-name
                                   "~/stuff/active/blog/index.html")))))

(provide 'ar-ox-html)

;;; ar-ox-html.el ends here
