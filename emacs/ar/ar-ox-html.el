;;; ar-ox-html.el --- Org HTML export support.

;;; Commentary:
;; Org HTML export helpers.


;;; Code:

(require 'ox-html)

(defun ar/ox-html-filter-timestamp-in-drawer-content (content)
  "Remove unnecessary HTML from exported modified CONTENT drawer."
  (string-match "\\(\\[.*\\]\\)" content)
  (match-string 0 content))

(defun ar/ox-html-export-format-drawer (name content)
  "Format drawer NAME and CONTENT for HTML export."
  (concat "<br>"
          "<span class=\"modified-timestamp\">"
          "  <em>"
          (ar/ox-html-filter-timestamp-in-drawer-content content)
          "  updated"
          "  </em>"
          "</span>"))

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
  (setq org-html-format-drawer-function #'ar/ox-html-export-format-drawer))

(defun ar/ox-html-export ()
  "Export blog to HTML."
  (interactive)
  (with-current-buffer (find-file-noselect (expand-file-name
                                            "~/stuff/active/blog/index.org"))
    (org-html-export-to-html)
    (browse-url (format "file:%s" (expand-file-name
                                   "~/stuff/active/blog/index.html")))))

(setq org-html-head-extra
      "<style type='text/css'>
         body {
           padding: 25px;
           margin: 0 auto;
           font-size: 100%;
           width: 50%;
         }
         .figure {
           padding: 0;
         }
         .title {
           font-size: 1em;
           text-align: right;
           color: rgb(51, 51, 51);
         }
         #contact-header {
           width: 100%;
         }
         #contact-right {
           text-align: right;
         }
         #contact-left {
           text-align: left;
         }
         #content {
         }
         .modified-timestamp {
           font-family: jaf-bernino-sans, 'Lucida Grande',
               'Lucida Sans Unicode', 'Lucida Sans', Geneva,
               Verdana, sans-serif;
           text-rendering: optimizelegibility;
           font-size: 0.8em;
           color: #a9a9a9;
         }
         pre {
           box-shadow: none;
           border: none;
         }
         pre.src {
           overflow: auto;
         }
         /* Hide sh/bash/Emacs Lisp overlay */
         pre.src:hover:before {
           display: none;
         }
         p, .org-ol, .org-ul {
           color: rgb(77, 77, 77);
           font-size: 1em;
           font-style: normal;
           font-family: jaf-bernino-sans, 'Lucida Grande',
               'Lucida Sans Unicode', 'Lucida Sans', Geneva,
               Verdana, sans-serif;
           font-weight: 300;
           text-rendering: optimizelegibility;
           line-height: 1.5;
           letter-spacing: 0.01rem;
         }
         h1, h2, h3, h4, h5, #preamble {
           font-family: jaf-bernino-sans, 'Lucida Grande',
               'Lucida Sans Unicode', 'Lucida Sans', Geneva,
               Verdana, sans-serif;
           text-rendering: optimizelegibility;
           color: rgb(51, 51, 51);
         }
         h1 {
           font-size: 3em;
         }
         h2 {
           font-size: 2em;
           letter-spacing: -0.02em;
           line-height: 1.2;
           font-weight: 700;
           margin-bottom: 0px;
         }
         h3 {
           font-size: 1.6em;
         }
         #preamble {
           text-align: right;
         }
         .timestamp {
          color: #FF3E96;
          font-family: jaf-bernino-sans, 'Lucida Grande',
               'Lucida Sans Unicode', 'Lucida Sans', Geneva,
               Verdana, sans-serif;
          font-size: 0.5em;
          font-style: normal;
          font-weight: 300;
          display: block;
         }
         a {
          text-decoration: none;
          color: #4183C4;
         }
         a:visited {
          background-color: #4183C4;
         }
         .outline-2 {
           margin-bottom: 50px;
         }
       </style>")

(provide 'ar-ox-html)

;;; ar-ox-html.el ends here
