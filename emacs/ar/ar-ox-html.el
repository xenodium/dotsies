;;; ar-ox-html.el --- Org HTML export support.

;;; Commentary:
;; Org HTML export helpers.


;;; Code:

(require 'ox-html)
(require 'ar-file)
(require 'ob-plantuml)

(defun ar/org-html-export-to-kill-ring ()
  "Export current buffer as HTML to kill ring.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region."
  (interactive)
  (kill-new (org-export-as 'html nil nil t nil)))

(defun ar/ox-html-filter-timestamp-in-drawer-content (content)
  "Remove unnecessary HTML from exported modified CONTENT drawer."
  (string-match "<span class=\"timestamp\">\\(.*?\\)</span>" content)
  (match-string 1 content))

(defun ar/ox-html-export-format-drawer (name content)
  "Format drawer NAME and CONTENT for HTML export."
  (concat "<span class=\"modified-timestamp\">"
          "  <em>"
          "updated: "
          (ar/ox-html-filter-timestamp-in-drawer-content content)
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

(defun ar/ox-export-async ()
  (interactive)
  (async-shell-command (concat (expand-file-name invocation-name invocation-directory) " --batch -Q -l "
                               (expand-file-name "~/.emacs.d/ar/ar-org-export-init.el && ")
                               "open " (format "file:%s" (expand-file-name
                                                          "~/stuff/active/blog/index.html")))
                       "*org html export*"))

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
          (progn
            (advice-add 'org-timestamp-translate
                        :around
                        'ar/ox-html--timestamp-translate-advice-fun)
            (org-html-export-to-html))
        (advice-remove 'org-timestamp-translate
                       'ar/ox-html--timestamp-translate-advice-fun))
      (browse-url (format "file:%s" (expand-file-name
                                     "~/stuff/active/blog/index.html"))))))

(setq org-html-head-extra
      "<style type='text/css'>
         /* https://stackoverflow.com/questions/6370690/media-queries-how-to-target-desktop-tablet-and-mobile */

         @media (min-width:320px)  {
           /* smartphones, iPhone, portrait 480x320 phones */
           body {
             font-size: 100%;
             max-width: 300px;
             margin: 0 auto;
             padding: 5px;
             width: 95%;
           }
         }

         @media (min-width:481px)  {
           /* portrait e-readers (Nook/Kindle), smaller tablets @ 600 or @ 640 wide. */
           body {
             font-size: 100%;
             max-width: 300px;
             margin: 0 auto;
             padding: 5px;
             width: 95%;
           }
         }

         @media (min-width:641px)  {
           /* portrait tablets, portrait iPad, landscape e-readers, landscape 800x480 or 854x480 phones */
           body {
             font-size: 100%;
             max-width: 300px;
             margin: 0 auto;
             padding: 5px;
             width: 95%;
           }
         }

         @media (min-width:961px)  {
           /* tablet, landscape iPad, lo-res laptops ands desktops */
           body {
             font-size: 100%;
             margin: 0 auto;
             max-width: 710px;
             padding: 25px;
             width: 50%;
           }
         }

         @media (min-width:1025px) {
           /* big landscape tablets, laptops, and desktops */
           body {
             font-size: 100%;
             margin: 0 auto;
             max-width: 710px;
             padding: 25px;
             width: 50%;
           }
         }

         @media (min-width:1281px) {
           /* hi-res laptops and desktops */
           body {
             font-size: 100%;
             margin: 0 auto;
             max-width: 710px;
             padding: 25px;
             width: 50%;
           }
         }

         .figure {
           padding: 0;
         }

         /* Table left border */
         .left {
           border-left: 1px solid #ccc;
         }

         .title {
           color: rgb(51, 51, 51);
           font-size: 1em;
           text-align: right;
         }

         .org-src-container {
           background-color: #fbfbfb;
           border-radius: 10px;
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

         blockquote {
           overflow: auto;
         }

         pre {
           border: none;
           box-shadow: none;
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
           font-family: 'Lucida Grande', 'Lucida Sans Unicode',
               'Lucida Sans', Geneva, Verdana, sans-serif;
           font-size: 1em;
           font-style: normal;
           font-weight: 300;
           letter-spacing: 0.01rem;
           line-height: 1.5em;
           text-rendering: optimizelegibility;
         }

         h1, h2, h3, h4, h5, #preamble {
           color: #2E2E2E;
           font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
           line-height: 1.15em;
         }

         h1 {
           font-size: 2em;
         }

         h2 {
           font-size: 1.6em;
           letter-spacing: -0.02em;
           margin-bottom: 0px;
           text-indent: -3px;
           cursor: pointer;
         }

         h3 {
           font-size: 1.2em;
         }

         #preamble {
           text-align: right;
         }

         .timestamp {
          color: #a9a9a9;
          display: block;
          font-family: 'Lucida Grande', 'Lucida Sans Unicode',
              'Lucida Sans', Geneva, Verdana, sans-serif;
          font-size: 0.5em;
          font-style: normal;
          font-weight: 300;
          line-height: 1em;
         }

         .modified-timestamp {
           color: #D3d3d3;
           font-family: 'Lucida Grande', 'Lucida Sans Unicode',
               'Lucida Sans', Geneva, Verdana, sans-serif;
           font-size: 0.8em;
           text-rendering: optimizelegibility;
         }

         a {
          color: #4183C4;
          text-decoration: none;
         }

         a:visited {
          background-color: #4183C4;
         }

         .outline-2 {
         }

         .default-visibility, .outline-text-2, .outline-3, .outline-4, .outline-5, .outline-6, .org-ul {
           display: none;
         }

         .example {
           white-space: pre-wrap;
           background-color: #f8ffe1;
         }
       </style>

       <!-- Start of Woopra Code -->
       <script>
         (function(){
           var t,i,e,n=window,o=document,a=arguments,s=\"script\",r=[\"config\",\"track\",\"identify\",\"visit\",\"push\",\"call\",\"trackForm\",\"trackClick\"],c=function(){var t,i=this;for(i._e=[],t=0;r.length>t;t++)(function(t){i[t]=function(){return i._e.push([t].concat(Array.prototype.slice.call(arguments,0))),i}})(r[t])};for(n._w=n._w||{},t=0;a.length>t;t++)n._w[a[t]]=n[a[t]]=n[a[t]]||new c;i=o.createElement(s),i.async=1,i.src=\"//static.woopra.com/js/w.js\",e=o.getElementsByTagName(s)[0],e.parentNode.insertBefore(i,e)
         })(\"woopra\");
         woopra.config({
             domain: 'xenodium.com'
         });
         woopra.track();
       </script>
       <!-- End of Woopra Code -->

       <script>
         function getClosest(elem, selector) {
             for ( ; elem && elem !== document; elem = elem.parentNode ) {
         	if ( elem.matches( selector ) ) return elem;
             }
             return null;
         };


         function setNodeVisible (node, visible) {
             for (var i = 0; i < node.childNodes.length; i++) {
                 var child = node.childNodes[i];
                 if (node.classList.contains('outline-text-2') ||
                     node.classList.contains('outline-3') ||
                     node.classList.contains('outline-4') ||
                     node.classList.contains('outline-5') ||
                     node.classList.contains('outline-6')) {
                     node.style.display = visible ? 'inline' : 'none';
                 } else if (node.classList.contains('org-ul')) {
                     node.style.display = visible ? 'block' : 'none';
                  }
                 setNodeVisible(child, visible);
             }
         }

         window.onload = function() {
             var parts = document.URL.split('#');
             if (parts.length > 1) {
                 var entry = getClosest(document.getElementById(parts[1]), '.outline-2');
                 setNodeVisible(entry, true)
             }

             document.body.onclick = function(e){
                 if (e.target.tagName.toLowerCase() === 'h2') {
                     var entry = getClosest(e.target, '.outline-2');
                     var elements = entry.getElementsByClassName('outline-text-2');
                     setNodeVisible(entry, elements[0].style.display !== 'inline');
                 }
             };
         };
       </script>

")

(provide 'ar-ox-html)

;;; ar-ox-html.el ends here
