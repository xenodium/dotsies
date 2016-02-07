;;; ar-ox-html.el --- Org HTML export support.

;;; Commentary:
;; Org HTML export helpers.


;;; Code:

(require 'ox-html)
(require 'ar-file)

(defun ar/ox-html-filter-timestamp-in-drawer-content (content)
  "Remove unnecessary HTML from exported modified CONTENT drawer."
  (string-match "<span class=\"timestamp\">\\(.*?\\)</span>" content)
  (match-string 1 content))

(defun ar/ox-html-export-format-drawer (name content)
  "Format drawer NAME and CONTENT for HTML export."
  (concat "<span class=\"modified-timestamp\">"
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

(defun ar/ox-html--timestamp-translate-advice-fun (orig-fun &rest r)
  "Translate advice function around ORIG-FUN and R arguments.
Remove angle brackets: <06 February 2016> => 06 February 2016"
  (let ((orig-timestamp (apply orig-fun r)))
    (if (string-match "<\\(.*\\)?>" orig-timestamp)
        (match-string 1 orig-timestamp)
      orig-timestamp)))

(defun ar/ox-html-export ()
  "Export blog to HTML."
  (interactive)
  (ar/file-assert-file-exists org-plantuml-jar-path)
  (ar/file-assert-file-exists (getenv "GRAPHVIZ_DOT"))
  (with-current-buffer (find-file-noselect (expand-file-name
                                            "~/stuff/active/blog/index.org"))
    (let ((org-time-stamp-custom-formats
           '("<%d %B %Y>" . "<%A, %B %d, %Y %H:%M>"))
          (org-display-custom-times 't))
      (unwind-protect
          (advice-add 'org-timestamp-translate
                      :around
                      'ar/ox-html--timestamp-translate-advice-fun)
        (advice-remove 'org-timestamp-translate
                       'ar/ox-html--timestamp-translate-advice-fun))
      (browse-url (format "file:%s" (expand-file-name
                                     "~/stuff/active/blog/index.html"))))))

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
         /* Table left border */
         .left { border-left: 1px solid #ccc; }
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
           color: #3A4145;
           font-size: 1.2em;
           font-style: normal;
           font-family: 'Lucida Grande', 'Lucida Sans Unicode',
               'Lucida Sans', Geneva, Verdana, sans-serif;
           font-weight: 300;
           text-rendering: optimizelegibility;
           line-height: 1.5;
           letter-spacing: 0.01rem;
         }
         h1, h2, h3, h4, h5, #preamble {
           font-family:'Helvetica Neue', Helvetica, Arial, sans-serif;
           color: #2E2E2E;
           line-height: 1.15em;
         }
         h1 {
           font-size: 4em;
         }
         h2 {
           text-indent: -3px;
           font-size: 3em;
           letter-spacing: -0.02em;
           margin-bottom: 0px;
         }
         h3 {
           font-size: 1.6em;
         }
         #preamble {
           text-align: right;
         }
         .timestamp {
          line-height: 1em;
          color: #a9a9a9;
          font-family: 'Lucida Grande', 'Lucida Sans Unicode',
              'Lucida Sans', Geneva, Verdana, sans-serif;
          font-size: 0.5em;
          font-style: normal;
          font-weight: 300;
          display: block;
         }
         .modified-timestamp {
           font-family: 'Lucida Grande', 'Lucida Sans Unicode',
               'Lucida Sans', Geneva, Verdana, sans-serif;
           text-rendering: optimizelegibility;
           font-size: 0.8em;
           color: #D3d3d3;
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
         .example {
           white-space: pre-wrap;
         }
       </style>")

(provide 'ar-ox-html)

;;; ar-ox-html.el ends here
