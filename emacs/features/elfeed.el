(require 'ar-vsetq)

(defun ar/open-youtube-url (url)
    "Download and open youtube URL."
    ;; Check for URLs like:
    ;; https://www.youtube.com/watch?v=rzQEIRRJ2T0
    ;; https://youtu.be/rzQEIRRJ2T0
    (assert (string-match-p "^https://\\(www\\.\\)?\\(youtube.com\\)\\|\\(youtu.be\\)\\|\\(soundcloud.com\\)\\)" url)
            nil "Not a downloadable URL: %s" url)
    (message "Downloading: %s" url)
    (async-start
     `(lambda ()
        (shell-command-to-string
         (format "youtube-dl --newline --exec \"open -a VLC {}\" -o \"~/Downloads/%%(title)s.%%(ext)s\" %s" ,url)))
     `(lambda (output)
        (if (string-match-p "ERROR:" output)
            (message "%s" output)
          (message "Opened: %s" ,url)))))

(defun ar/open-youtube-clipboard-url ()
  "Download youtube video from url in clipboard."
  (interactive)
  (ar/open-youtube-url (current-kill 0)))

(use-package elfeed :ensure t
  :commands elfeed
  :after centered-cursor-mode
  :hook ((elfeed-search-mode . centered-cursor-mode)
         (elfeed-search-mode . ar/elfeed-set-style))
  :init
  (defun ar/elfeed-set-style ()
    ;; Separate elfeed lines for readability.
    (ar/vsetq line-spacing 25))
  :config
  (use-package elfeed-goodies :ensure t
    :after elfeed
    :config
    (ar/vsetq elfeed-goodies/entry-pane-position 'bottom)
    (ar/vsetq elfeed-goodies/tag-column-width 35)
    (elfeed-goodies/setup))

  (defun ar/elfeed-open-youtube-video ()
    (interactive)
    (let ((link (elfeed-entry-link elfeed-show-entry)))
      (when link
        (ar/open-youtube-url link))))

  (ar/vsetq elfeed-feeds
            '(
              ("http://200ok.ch/atom.xml" blog emacs tech 200ok)
              ("http://akkartik.name/feeds.xml" blog tech KartikAgaram)
              ("http://ben-evans.com/benedictevans?format=RSS" blog tech Ben-Evans)
              ("http://blog.davep.org/feed.xml" blog emacs tech davep)
              ("http://blog.josephholsten.com/feed.xml" blog hammerspoon tech Libera-Ideoj)
              ("http://cestlaz.github.io/rss.xml" blog emacs Zamansky)
              ("http://cmsj.net/feed.xml" blog hammerspoon tech Chris-Jones)
              ("http://dangrover.com/feed.xml" blog dangrover emacs tech)
              ("http://emacsredux.com/atom.xml" blog emacs emacs-redux)
              ("http://emacsworld.blogspot.com/feeds/posts/default?alt=rss" blog emacs EmacsWorld)
              ("http://evgeni.io/categories/posts/index.xml" blog emacs Evgeni)
              ("http://feeds.bbci.co.uk/news/uk/rss.xml?edition=uk" news BBCUK)
              ("http://feeds.bbci.co.uk/news/world/rss.xml?edition=uk" news BBCWorld)
              ("http://feeds.feedburner.com/japaneseruleof7" blog japan japanese-rule-of-7)
              ("http://francismurillo.github.io/hacker/feed.xml" blog tech emacs francismurillo)
              ("http://francismurillo.github.io/watcher/feed.xml" blog anime francismurillo)
              ("http://irreal.org/blog/?feed=rss2" blog emacs tech Irreal)
              ("http://kundeveloper.com/feed" blog emacs tech KunDeveloper)
              ("http://nullprogram.com/feed" blog emacs tech Chris-Wellons)
              ("http://petercheng.net/index.xml" blog emacs PeterCheng)
              ("http://planet.emacsen.org/atom.xml" blog emacs tech emacsen)
              ("http://prodissues.com/feeds/all.atom.xml" blog emacs tech  Prodissues)
              ("http://reddit.com/r/emacs/.rss" social reddit emacs)
              ("http://rubyronin.com/wp-feed.php" blog japan the-ruby-ronin)
              ("http://sachachua.com/blog/feed" blog emacs tech sachachua)
              ("http://sdegutis.com/blog/atom.xml" blog tech StevenDegutis)
              ("http://tech.memoryimprintstudio.com/feed" blog emacs tech MemoryImprintStudio)
              ("http://www.arcadianvisions.com/blog/rss.xml" blog tech emacs arcadianvisions)
              ("http://www.badykov.com/feed.xml" blog emacs KrakenOfThought)
              ("http://www.brool.com/index.xml" blog emacs Brool)
              ("http://www.gonsie.com/blorg/feed.xml" blog emacs dev)
              ("http://www.ict4g.net/adolfo/feed.xml" blog tech dev Adolfo)
              ("http://www.modernemacs.com/index.xml" blog emacs tech ModernEmacs)
              ("http://www.sastibe.de/index.xml" blog emacs SebastianSchweer)
              ("http://www.thisiscolossal.com/feed" blog tech Colossal)
              ("http://zzamboni.org/index.xml" blog hammerspoon tech Diego-Martín-Zamboni)
              ("https://ambrevar.xyz/rss.xml" blog ambrevar)
              ("https://babbagefiles.blogspot.com/feeds/posts/default" blog emacs tech)
              ("https://blog.danielgempesaw.com/rss" blog emacs tech DanielGempesaw)
              ("https://changelog.complete.org/feed" blog emacs tech JohnGoerzen)
              ("https://colelyman.com/index.xml" blog emacs ColeLyman)
              ("https://copyninja.info/feeds/all.atom.xml" blog tech dev copyninja)
              ("https://dmolina.github.io/index.xml" blog emacs DanielMolina)
              ("https://dschrempf.github.io/index.xml" blog emacs DominikSchrempf)
              ("https://elephly.net/feed.xml" blog emacs Elephly)
              ("https://emacs-doctor.com/feed.xml" blog emacs emacs-doctor)
              ("https://emacsist.github.io/index.xml" blog emacs tech emacsist)
              ("https://emacsnotes.wordpress.com/feed" blog tech emacs)
              ("https://feeds.feedburner.com/codinghorror" blog tech Coding-Horror)
              ("https://ghuntley.com/rss" blog tech ghuntley ghuntley)
              ("https://hacks.mozilla.org/feed" blog tech Mozilla)
              ("https://harryrschwartz.com/atom.xml" bloc emacs HarryRSchwartz)
              ("https://hasanyavuz.ozderya.net/?feed=rss2" blog emacs HasanYavuz)
              ("https://increment.com/feed.xml" blog dev Increment)
              ("https://kdecherf.com/feeds/blog.atom.xml" blog tech dev kdecherf-blog)
              ("https://kdecherf.com/feeds/le-kdecherf.atom.xml" blog tech dev kdecherf)
              ("https://manuel-uberti.github.io/feed.xml" blog emacs ManuelUberti)
              ("https://martinralbrecht.wordpress.com/feed" blog emacs tech MartinAlbrecht)
              ("https://matt.hackinghistory.ca/feed/" blog emacs MattPrice)
              ("https://medium.com/feed/@mwfogleman" blog tech emacs meditation MichaelFogleman)
              ("https://news.ycombinator.com/rss" news hackernews tech)
              ("https://ogbe.net/blog.xml" blog emacs tech DennisOgbe)
              ("https://palikar.github.io/index.xml" blog emacs StanislavArnaudov)
              ("https://piware.de/post/index.xml" blog tech)
              ("https://punchagan.muse-amuse.in/feed.xml" blog emacs Punchagan)
              ("https://sam217pa.github.io/index.xml" blog emacs BacterialFinches)
              ("https://sciencebasedmedicine.org/feed" blog medicine ScienceBasedMedicine)
              ("https://scripter.co/posts/index.xml" blog emacs tech dev)
              ("https://stingh711.github.io/feed.xml" blog emacs StringsNote)
              ("https://swiftnews.curated.co/issues.rss" blog swift tech ShiftNewsCurated)
              ("https://swiftweekly.github.io/feed.xml" blog swift tech SwiftWeekly)
              ("https://webgefrickel.de/blog/feed" blog tech dev SteffenRademacker)
              ("https://wincent.com/blog.rss" blog tech dev wincent)
              ("https://writequit.org/posts.xml" blog tech emacs writequit)
              ("https://www.baty.net/index.xml" blog emacs JackBaty)
              ("https://www.bytedude.com/feed.xml" blog emacs MarcinS)
              ("https://www.hasecke.eu/index.xml" blog emacs tech hasecke)
              ("https://www.johndcook.com/blog/comments/feed" blog emacs JohnDCook)
              ("https://www.ogre.com/blog/feed" blog dev Ogre)
              ("https://www.steventammen.com/index.xml" blog emacs StevenTammen)
              ("https://ytrss.co/feed/UCkRmQ_G_NbdbCQMpALg6UPg" youtube emacs EmacsRocks)
              ("https://ytrss.co/feed/UCxkMDXQ5qzYOgXPRnOBrp1w" youtube emacs Zamansky)
              ))

  (defun ar/elfeed-view-filtered (filter)
    "Filter the elfeed-search buffer to show feeds tagged with FILTER."
    (interactive)
    (elfeed)
    (unwind-protect
        (let ((elfeed-search-filter-active :live))
          (setq elfeed-search-filter filter))
      (elfeed-search-update :force)))

  (defun ar/elfeed-view-emacs ()
    "Filter the elfeed-search buffer to show emacs-tagged feeds."
    (interactive)
    (ar/elfeed-view-filtered "@6-months-ago +unread +emacs"))

  (defun ar/elfeed-view-news ()
    "Filter the elfeed-search buffer to show news-tagged feeds."
    (interactive)
    (ar/elfeed-view-filtered "@6-months-ago +unread +news")))
