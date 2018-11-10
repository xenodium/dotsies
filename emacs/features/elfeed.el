(require 'ar-vsetq)

(defun ar/open-youtube-url (url)
  "Download and open youtube URL."
  ;; Check for URLs like:
  ;; https://www.youtube.com/watch?v=rzQEIRRJ2T0
  ;; https://youtu.be/rzQEIRRJ2T0
  (assert (string-match-p "^http[s]?://\\(www\\.\\)?\\(\\(youtube.com\\)\\|\\(youtu.be\\)\\|\\(soundcloud.com\\)\\)" url)
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
  :hook ((elfeed-search-mode . ar/elfeed-set-style))
  :bind (:map elfeed-search-mode-map
              ("R" . ar/elfeed-mark-all-as-read)
              ("v" . ar/mark-visible-as-read))
  :init
  (defun ar/elfeed-set-style ()
    ;; Separate elfeed lines for readability.
    (ar/vsetq line-spacing 25))
  (defun ar/elfeed-mark-all-as-read ()
    "Mark all entries in search as read."
    (interactive)
    (mark-whole-buffer)
    (elfeed-search-untag-all-unread))
  (defun ar/mark-visible-as-read ()
    (interactive)
    (ar/with-marked-visible-buffer
     (lambda ()
       (elfeed-search-untag-all-unread)
       (elfeed-search-update--force)
       (goto-char 0)
       )))
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
              ("http://200ok.ch/atom.xml" blog emacs 200ok)
              ("http://akkartik.name/feeds.xml" blog dev)
              ("http://ben-evans.com/benedictevans?format=RSS" blog dev Ben-Evans)
              ("http://blog.abhixec.com/index.xml" blog emacs RandomMusings)
              ("http://blog.davep.org/feed.xml" blog emacs davep)
              ("http://blog.josephholsten.com/feed.xml" blog hammerspoon dev Libera-Ideoj)
              ("http://blog.nawaz.org/feeds/all.atom.xml" blog dev )
              ("http://cestlaz.github.io/rss.xml" blog emacs Zamansky)
              ("http://cmsj.net/feed.xml" blog hammerspoon dev Chris-Jones)
              ("http://dangrover.com/feed.xml" blog dangrover emacs )
              ("http://emacsredux.com/atom.xml" blog emacs emacs-redux)
              ("http://emacsworld.blogspot.com/feeds/posts/default?alt=rss" blog emacs EmacsWorld)
              ("http://evgeni.io/categories/posts/index.xml" blog emacs Evgeni)
              ("http://feeds.bbci.co.uk/news/uk/rss.xml?edition=uk" news BBCUK bbc)
              ("http://feeds.bbci.co.uk/news/world/rss.xml?edition=uk" news BBCWorld bbc)
              ("http://feeds.feedblitz.com/atomicspin&x=1" blog dev emacs AtomicObject)
              ("http://feeds.feedburner.com/AffordAnythingFeed" blog money AffordAnything)
              ("http://feeds.feedburner.com/FinancialSamurai" blog money FinancialSamurai)
              ("http://feeds.feedburner.com/japaneseruleof7" blog japan japanese-rule-of-7)
              ("http://feeds2.feedburner.com/Monevatorcom" blog money Monevator)
              ("http://francismurillo.github.io/hacker/feed.xml" blog emacs francismurillo)
              ("http://francismurillo.github.io/watcher/feed.xml" blog anime francismurillo)
              ("http://irreal.org/blog/?feed=rss2" blog emacs Irreal)
              ("http://kundeveloper.com/feed" blog emacs KunDeveloper)
              ("http://nullprogram.com/feed" blog emacs Chris-Wellons)
              ("http://petercheng.net/index.xml" blog emacs PeterCheng)
              ("http://planet.emacsen.org/atom.xml" blog emacs emacsen)
              ("http://prodissues.com/feeds/all.atom.xml" blog emacs Prodissues)
              ("http://reddit.com/r/emacs/.rss" social reddit emacs)
              ("http://rubyronin.com/wp-feed.php" blog japan the-ruby-ronin)
              ("http://sachachua.com/blog/feed" blog emacs sachachua)
              ("http://sdegutis.com/blog/atom.xml" blog dev StevenDegutis)
              ("http://tangent.libsyn.com" blog money  ChristopherRyan)
              ("http://tech.memoryimprintstudio.com/feed" blog emacs MemoryImprintStudio)
              ("http://thefirestarter.co.uk/feed" blog money FireStarter)
              ("http://www.arcadianvisions.com/blog/rss.xml" blog emacs arcadianvisions)
              ("http://www.badykov.com/feed.xml" blog emacs KrakenOfThought)
              ("http://www.brool.com/index.xml" blog emacs Brool)
              ("http://www.gonsie.com/blorg/feed.xml" blog emacs)
              ("http://www.modernemacs.com/index.xml" blog emacs ModernEmacs)
              ("http://www.msziyou.com/feed/" blog money ZiYou)
              ("http://www.sastibe.de/index.xml" blog emacs SebastianSchweer)
              ("http://www.thisiscolossal.com/feed" blog Colossal)
              ("http://zzamboni.org/index.xml" blog hammerspoon dev Diego-Martín-Zamboni)
              ("https://affordanything.com/blog/feed" blog money AffordAnything)
              ("https://affordanything.com/comments/feed/" blog money AffordAnything)
              ("https://ambrevar.xyz/rss.xml" blog emacs PierreNeidhardt)
              ("https://babbagefiles.xyz/index.xml" blog emacs BabbageFiles)
              ("https://blog.burntsushi.net/index.xml" blog dev BurnedSushi)
              ("https://blog.danielgempesaw.com/rss" blog emacs DanielGempesaw)
              ("https://blog.moneysavingexpert.com/blog.rss" blog money MoneySavingExpert MartinLewis)
              ("https://changelog.com/feed" blog dev news JohnGoerzen)
              ("https://changelog.complete.org/feed" blog emacs JohnGoerzen)
              ("https://colelyman.com/index.xml" blog emacs ColeLyman)
              ("https://copyninja.info/feeds/all.atom.xml" blog dev copyninja)
              ("https://dev.to/feed" blog dev DevTo)
              ("https://dmolina.github.io/index.xml" blog emacs DanielMolina)
              ("https://drfire.co.uk/feed" blog money DrFire)
              ("https://dschrempf.github.io/index.xml" blog emacs DominikSchrempf)
              ("https://elephly.net/feed.xml" blog emacs Elephly)
              ("https://emacs-doctor.com/feed.xml" blog emacs emacs-doctor)
              ("https://emacsist.github.io/index.xml" blog emacs emacsist)
              ("https://emacsnotes.wordpress.com/feed" blog emacs EmacsNotes)
              ("https://ericasadun.com/feed" blog swift ios EricaSadun)
              ("https://feeds.feedburner.com/codinghorror" blog dev Coding-Horror)
              ("https://ghuntley.com/rss" blog dev ghuntley)
              ("https://github.crookster.org/feed.xml" blog dev emacs Crook)
              ("https://hacks.mozilla.org/feed" blog dev Mozilla)
              ("https://harryrschwartz.com/atom.xml" bloc emacs HarryRSchwartz)
              ("https://hasanyavuz.ozderya.net/?feed=rss2" blog emacs HasanYavuz)
              ("https://increment.com/feed.xml" blog dev Increment)
              ("https://indeedably.com/feed/" blog money InDeedABly)
              ("https://kdecherf.com/feeds/blog.atom.xml" blog dev kdecherf-blog)
              ("https://kdecherf.com/feeds/le-kdecherf.atom.xml" blog dev kdecherf)
              ("https://manuel-uberti.github.io/feed.xml" blog emacs ManuelUberti)
              ("https://martinralbrecht.wordpress.com/feed" blog emacs MartinAlbrecht)
              ("https://matt.hackinghistory.ca/feed/" blog emacs MattPrice)
              ("https://medium.com/feed/@tasshin" blog emacs meditation MichaelFogleman)
              ("https://news.ycombinator.com/rss" hackernews tech)
              ("https://ogbe.net/blog.xml" blog emacs dev DennisOgbe)
              ("https://palikar.github.io/index.xml" blog emacs StanislavArnaudov)
              ("https://piware.de/post/index.xml" blog dev MartinPitt)
              ("https://punchagan.muse-amuse.in/feed.xml" blog emacs Punchagan)
              ("https://ryanholiday.net/comments/feed/" blog meditation RyanHolidayComments)
              ("https://ryanholiday.net/feed/" blog meditation health RyanHoliday)
              ("https://ryanholiday.net/feed/atom" blog emacs StringsNote)
              ("https://sam217pa.github.io/index.xml" blog emacs BacterialFinches)
              ("https://sciencebasedmedicine.org/feed" blog medicine ScienceBasedMedicine)
              ("https://scripter.co/posts/index.xml" blog emacs dev)
              ("https://swiftnews.curated.co/issues.rss" blog swift ios ShiftNewsCurated)
              ("https://swiftweekly.github.io/feed.xml" blog swift ios SwiftWeekly)
              ("https://thesavingninja.com/feed" blog money SavingNinja)
              ("https://vicarie.in/archive.xml" blog emacs NarendraJoshi)
              ("https://webgefrickel.de/blog/feed" blog dev SteffenRademacker)
              ("https://wincent.com/blog.rss" blog dev wincent)
              ("https://writequit.org/posts.xml" blog emacs writequit)
              ("https://www.baty.net/index.xml" blog emacs JackBaty)
              ("https://www.bytedude.com/feed.xml" blog emacs MarcinS)
              ("https://www.campfirefinance.com/feed" blog money CampFireFinance)
              ("https://www.choosefi.com/category/podcast-episodes/feed" blog money ChoosefiPodcasts)
              ("https://www.choosefi.com/feed/" blog money Choosefi)
              ("https://www.drweil.com/blog/health-tips/feed" blog health healthTips DrWeil)
              ("https://www.drweil.com/blog/spontaneous-happiness/feed" blog health happinessTips DrWeil)
              ("https://www.getrichslowly.org/feed" blog money GetRichSlowly)
              ("https://www.hasecke.eu/index.xml" blog emacs hasecke)
              ("https://www.ict4g.net/adolfo/site.atom" blog dev Adolfo)
              ("https://www.johndcook.com/blog/feed" blog emacs JohnDCook)
              ("https://www.madfientist.com/comments/feed/" blog money MadFientistComments)
              ("https://www.madfientist.com/feed/" blog money MadFientist)
              ("https://www.moneysavingexpert.com/news/feeds/news.rss" blog money MoneySavingExpert news)
              ("https://www.ogre.com/blog/feed" blog dev Ogre)
              ("https://www.reddit.com/r/UKPersonalFinance/.rss" social reddit money UKPersonalFinance)
              ("https://firevlondon.com/feed/" blog money FIREvsLondon)
              ("https://www.romanzolotarev.com/rss.xml" blog bsd dev RomanZolotarev)
              ("https://www.steventammen.com/index.xml" blog emacs StevenTammen)
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

  (defun ar/elfeed-view-bbc ()
    "Filter the elfeed-search buffer to show news-tagged feeds."
    (interactive)
    (ar/elfeed-view-filtered "@6-months-ago +unread +bbc"))

  (defun ar/elfeed-view-hackernews ()
    "Filter the elfeed-search buffer to show news-tagged feeds."
    (interactive)
    (ar/elfeed-view-filtered "@6-months-ago +unread +hackernews"))

  (defun ar/elfeed-view-money ()
    "Filter the elfeed-search buffer to show news-tagged feeds."
    (interactive)
    (ar/elfeed-view-filtered "@6-months-ago +unread +money")))

(use-package org-web-tools
  :ensure t
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
        (if (called-interactively-p)
            (insert (if (listp return)
                        (s-join " " return)
                      return))
          return)))))
