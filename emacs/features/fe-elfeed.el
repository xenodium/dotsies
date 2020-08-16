;;; -*- lexical-binding: t; -*-
(use-package elfeed
  :ensure t
  :commands
  (elfeed)
  :hook ((elfeed-search-mode . ar/elfeed-set-style))
  :bind (:map elfeed-search-mode-map
              ("R" . ar/elfeed-mark-all-as-read)
              ("d" . elfeed-search-untag-all-unread)
              ("v" . ar/elfeed-mark-visible-as-read)
              ("<tab>" . ar/elfeed-completing-filter)
              ("M-RET" . ar/elfeed-search-browse-background-url)
              ("B" . ar/elfeed-search-browse-background-url))
  :validate-custom
  (elfeed-search-title-max-width 120)
  (elfeed-search-print-entry-function #'ar/elfeed-search-print-entry)
  :init
  (defun ar/elfeed-search-print-entry (entry)
    "My preferred format for displaying each elfeed search result ENTRY.
Based on `elfeed-search-print-entry--default'."
    (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
           ;; Decode HTML entities (ie. &amp;)
           ;; (title (ar/string-decode-html-entities (or (elfeed-meta entry :title) (elfeed-entry-title entry) "")))
           (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
           (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
           (feed (elfeed-entry-feed entry))
           (feed-title
            (when feed
              (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
           (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
           (tags-str (mapconcat
                      (lambda (s) (propertize s 'face 'elfeed-search-tag-face))
                      tags ","))
           (title-width (- (window-width) 10 elfeed-search-trailing-width))
           (title-column (elfeed-format-column
                          title (elfeed-clamp
                                 elfeed-search-title-min-width
                                 title-width
                                 elfeed-search-title-max-width)
                          :left)))
      (insert (propertize date 'face 'elfeed-search-date-face) " ")
      (insert (propertize title-column 'face title-faces 'kbd-help title) " ")
      (when feed-title
        (insert (propertize feed-title 'face 'elfeed-search-feed-face) " "))
      (when tags
        (insert "(" tags-str ")"))))

  (defun ar/elfeed-set-style ()
    ;; Separate elfeed lines for readability.
    (setq line-spacing 25))
  :config
  (load "~/.emacs.d/features/config-elfeed"))

(use-package org-web-tools
  :ensure t
  :commands ar/feed-for-url-in-clipboard
  :config
  (use-package esxml
    :ensure t)

  (require 'cl-macs)

  ;; From https://github.com/alphapapa/unpackaged.el#feed-for-url
  (cl-defun ar/feed-for-url-in-clipboard (url &key (prefer 'atom) (all nil))
    "Return feed URL for web page at URL.
Interactively, insert the URL at point.  PREFER may be
`atom' (the default) or `rss'.  When ALL is non-nil, return all
feed URLs of all types; otherwise, return only one feed URL,
preferring the preferred type."
    (interactive (list (org-web-tools--get-first-url)))
    (require 'esxml-query)
    (require 'org-web-tools)
    (cl-flet ((feed-p (type)
                      ;; Return t if TYPE appears to be an RSS/ATOM feed
                      (string-match-p (rx "application/" (or "rss" "atom") "+xml")
                                      type)))
      (let* ((preferred-type (format "application/%s+xml" (symbol-name prefer)))
             (html (org-web-tools--get-url url))
             (dom (with-temp-buffer
                    (insert html)
                    (libxml-parse-html-region (point-min) (point-max))))
             (potential-feeds (esxml-query-all "link[rel=alternate]" dom))
             (return (if all
                         ;; Return all URLs
                         (cl-loop for (tag attrs) in potential-feeds
                                  when (feed-p (alist-get 'type attrs))
                                  collect (url-expand-file-name (alist-get 'href attrs) url))
                       (or
                        ;; Return the first URL of preferred type
                        (cl-loop for (tag attrs) in potential-feeds
                                 when (equal preferred-type (alist-get 'type attrs))
                                 return (url-expand-file-name (alist-get 'href attrs) url))
                        ;; Return the first URL of non-preferred type
                        (cl-loop for (tag attrs) in potential-feeds
                                 when (feed-p (alist-get 'type attrs))
                                 return (url-expand-file-name (alist-get 'href attrs) url))))))
        (assert return nil "No feed found")
        (if (called-interactively-p)
            (insert (if (listp return)
                        (s-join " " return)
                      return))
          return)))))

(defun ar/open-youtube-url (url)
  "Download and open youtube URL."
  ;; Check for URLs like:
  ;; https://www.youtube.com/watch?v=rzQEIRRJ2T0
  ;; https://youtu.be/rzQEIRRJ2T0
  (require 's)
  (setq url (s-trim url))
  (assert (or (string-match-p "^http[s]?://\\(www\\.\\)?\\(\\(youtube.com\\)\\|\\(m.youtube.com\\)\\|\\(youtu.be\\)\\|\\(soundcloud.com\\)\\|\\(redditmedia.com\\)\\)" url)
              (string-match-p "^http[s]?://.*bandcamp.com" url))
          nil "Not a downloadable URL: %s" url)
  (message "Downloading: %s" url)
  (async-start
   `(lambda ()
      (shell-command-to-string
       (format "youtube-dl --newline -o \"~/Downloads/%%(title)s.%%(ext)s\" %s" ,url)))
   `(lambda (output)
      (if (string-match-p "ERROR:" output)
          (message "%s" output)
        (message "Downloaded: %s" ,url)))))

(defun ar/open-youtube-clipboard-url ()
  "Open youtube video from url in clipboard."
  (interactive)
  (ar/open-youtube-url (current-kill 0)))

(defun ar/open-youtube-clipboard-from-page-url ()
  "Open youtube video from page url in clipboard."
  (interactive)
  (require 'ar-url)
  (require 'dash)
  (let ((youtube-urls (-union
                       ;; Look for iframe.
                       (-filter (lambda (iframe-url)
                                  (string-match "\\(youtube.com\\)\\|\\(youtu.be\\)\\|\\(redditmedia.com\\)" iframe-url))
                                (ar/url-fetch-iframe-srcs (current-kill 0)))
                       ;; Look for links.
                       (-map (lambda (anchor)
                               (let-alist anchor
                                 .url))
                             (-filter (lambda (anchor)
                                        (let-alist anchor
                                          (string-match "youtube" .url)))
                                      (ar/url-fetch-anchor-elements (current-kill 0)))))))
    (assert (> (length youtube-urls) 0) nil "No youtube links found")
    (if (= (length youtube-urls) 1)
        (ar/open-youtube-url (nth 0 youtube-urls))
      (ar/open-youtube-url (completing-read "choose:" youtube-urls)))))

(use-package ytel
  :ensure t
  :bind (:map ytel-mode-map
              ("RET" . ar/ytel-watch)
              ("M-RET" . ar/ytel-download-video-at-point))
  :hook ((ytel-mode . (lambda ()
                        (toggle-truncate-lines nil))))
  :config
  (defun ar/ytel-watch ()
    "Stream video at point in mpv."
    (interactive)
    (let* ((video (ytel-get-current-video))
     	   (id    (ytel-video-id video)))
      (start-process "ytel mpv" nil
		     "mpv"
		     (concat "https://www.youtube.com/watch?v=" id))
      "--ytdl-format=bestvideo[height<=?720]+bestaudio/best")
    (message "Starting streaming..."))


  (defun ar/ytel-download-video-at-point ()
    "Download video at point."
    (interactive)
    (let* ((video (ytel-get-current-video))
     	   (id    (ytel-video-id video)))
      (ar/open-youtube-url (concat "https://www.youtube.com/watch?v=" id)))
    (message "Downloading...")))
