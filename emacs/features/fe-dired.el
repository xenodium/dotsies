;;; -*- lexical-binding: t; -*-
(require 'ar-vsetq)

(use-package projectile
  :ensure t
  :defer 2
  :ensure-system-package fd
  :validate-custom
  (projectile-dynamic-mode-line nil)
  :config
  (ar/vsetq projectile-enable-caching t)
  (ar/vsetq projectile-completion-system 'ivy)
  ;; Use `hybrid' since `alien' ignores .projectile file, which is
  ;; handy for very large repositories.
  (ar/vsetq projectile-indexing-method 'hybrid)
  ;; fd is super fast. Use it if available.
  (ar/vsetq projectile-project-root-files-functions
            '(projectile-root-local
              projectile-root-bottom-up))
  (when (executable-find "fd")
    (let ((fd-command "fd . --print0"))
      (ar/vsetq projectile-hg-command fd-command)
      (ar/vsetq projectile-git-command fd-command)
      (ar/vsetq projectile-fossil-command fd-command)
      (ar/vsetq projectile-bzr-command fd-command)
      (ar/vsetq projectile-darcs-command fd-command)
      (ar/vsetq projectile-svn-command fd-command)
      (ar/vsetq projectile-generic-command fd-command)))
  (projectile-mode))

(use-package dired
  :hook (dired-mode . dired-hide-details-mode)
  :bind (:map global-map
              ("C-l" . dired-jump)
              :map dired-mode-map
              ("j" . dired-next-line)
              ("k" . dired-previous-line)
              ;; Go to parent directory.
              ("^" . ar/file-find-parent-dir)
              ("RET" . dired-find-file)
              ("P" . peep-dired)
              ("i" . dired-hide-details-mode)
              ("C-l". dired-jump)
              ("s" . hydra-dired-sort/body)
              ("M" . ar/dired-mark-all))
  :commands (dired-mode
             ar/find-all-dired-current-dir
             ar/dired-mark-all
             ar/file-find-alternate-parent-dir)
  :validate-custom
  (dired-recursive-copies 'always)
  :init
  (defun ar/file-find-parent-dir ()
    "Open parent dir."
    (interactive)
    (find-file ".."))

  (defun ar/dired-mark-all ()
    (interactive)
    (dired-mark-files-regexp ""))

  (defun ar/find-all-dired-current-dir ()
    "Invokes `find-dired' for current dir."
    (interactive)
    (let ((dir (if buffer-file-name
                   (file-name-directory buffer-file-name)
                 ".")))
      (find-dired dir "'(' -name .svn -o -name .git ')' -prune -o -type f")))

  ;; https://oremacs.com/2015/02/15/sudo-stuffs/
  (defun ar/sudired ()
    (interactive)
    (require 'tramp)
    (let ((dir (expand-file-name default-directory)))
      (if (string-match "^/sudo:" dir)
          (user-error "Already in sudo")
        (dired (concat "/sudo::" dir)))))

  :config
  (use-package wdired
    :validate-custom
    (wdired-allow-to-change-permissions t)
    (wdired-create-parent-directories t))

  ;; For dired-jump.
  (use-package dired-x)

  (use-package peep-dired
    :ensure t
    :bind (:map dired-mode-map
                ("P" . peep-dired)))

  ;; Flaten display of nested directories with no other content.
  (use-package dired-collapse
    :ensure t)

  (use-package dired-subtree :ensure t
    :bind (:map dired-mode-map
                ("<tab>" . dired-subtree-toggle)
                ("<backtab>" . dired-subtree-cycle)))

  ;; Adding human readable units and sorted by date.
  ;; -A List all entries except for "." and "..".
  ;; -l List in long format.
  ;; -h Use unites (ie. Byte, Kilobyte, Megabyte).
  ;; -t Sort by time
  ;; -c Use last file status changed time for sorting (show me last download).
  (ar/vsetq dired-listing-switches "-Alhtc")

  ;; Try to guess the target directory for operations.
  (ar/vsetq dired-dwim-target t)

  ;; Enable since disabled by default.
  (put 'dired-find-alternate-file 'disabled nil)

  ;; Automatically refresh dired buffers when contents changes.
  (ar/vsetq dired-auto-revert-buffer t)

  ;; Hide some files
  (setq dired-omit-files "^\\..*$\\|^\\.\\.$")
  (setq dired-omit-mode t)

  (defun ar/dired-sort-by-size()
    "Sort dired buffer by size."
    (interactive)
    (dired-sort-other "-AlhS")
    (beginning-of-buffer)
    (next-line))

  (defun ar/dired-sort-by-date ()
    "Sort dired buffer by date."
    (interactive)
    (dired-sort-other "-Alhtc")
    (beginning-of-buffer)
    (next-line))

  (defun ar/dired-sort-by-name ()
    "Sort dired buffer by name."
    (interactive)
    (dired-sort-other "-Alh")
    (beginning-of-buffer)
    (next-line))

  (defun ar/dired-du-size-of-selection ()
    "Print size of selected dired files or directories."
    (interactive)
    (let ((files (dired-get-marked-files)))
      (with-temp-buffer
        (apply 'call-process "du" nil t nil "-csh" files)
        (message "Size of all marked files: %s"
                 (progn
                   (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
                   (match-string 1))))))

  ;; Predownloaded to ~/.emacs.d/downloads
  (use-package tmtxt-dired-async
    :config
    (use-package tmtxt-async-tasks))

  (use-package dired-git-info
    :ensure t
    :bind (:map dired-mode-map
                (")" . dired-git-info-mode)))

  ;; Colourful entries.
  (use-package diredfl
    :ensure t
    :config
    (diredfl-global-mode 1))

  (use-package dired-filter
    :ensure t
    :bind (:map dired-filter-map
                ("v" . dired-filter-by-videos))
    :config
    (dired-filter-define videos
        "Toggle current view to video files."
      (:description "videos")
      (string-match-p "\\.\\(mp4\\|264\\|dav\\|exo\\|h264\\|n3r\\|wve\\|tscproj\\|swf\\|avc\\|ts\\|hkv\\|g64x\\|avi\\|vproj\\|mepx\\|dvr\\|vob\\|bup\\|mkv\\|trec\\|encm\\|v264\\|webm\\|rec\\|pz\\|kux\\|dv4\\|bu\\|yify\\|arf\\|ifv\\|nfv\\|pds\\|dv5\\|h260\\|mov\\|lrv\\|dash\\|es3\\|flv\\|etrg\\|wlmp\\|fbr\\|ezp\\|dvt\\|prproj\\|tvs\\|vcr\\|m4s\\|strg\\|gifv\\|rf\\|trp\\|bvr\\|hbox\\|dxa\\|um4\\|veg\\|dcf\\|dc8\\|mk3d\\|dat\\|wmv\\|vvf\\|bik\\|dxr\\|lrec\\|mks\\|vpj\\|vep\\|vse\\|mts\\|mp41\\|dfxp\\|mxf\\|m2t\\|m4f\\|usm\\|bnk\\|h265\\|demo\\|mpg\\|dir\\|3gp\\|dtcp-ip\\|stx\\|vcd\\|vp6\\|sdr2\\|vgz\\|viv\\|mod\\|xvid\\|avh\\|hevc\\|ogv\\|vf\\|evo\\|dce\\|m4v\\|doink-gs\\|asf\\|rdt\\|vghd\\|cine\\|eti\\|moff\\|sol\\|mjpeg\\|vfo\\|3gp_128x96\\|ssif\\|m2p\\|ub1\\|ravi\\|irf\\|mts1\\|hmt\\|avf\\|dmx\\|asx\\|camproj\\|nmm\\|ismv\\|tp\\|nvc\\|vro\\|stu\\|tod\\|fcpxml\\|ivf\\|vp3\\|tts\\|xba\\|mjpg\\|3g2\\|sdv\\|hav\\|rec_part0\\|h3r\\|vid\\|amv\\|eztv\\|h64\\|dmsd\\|crec\\|265\\|amc\\|camrec\\|epm\\|hup\\|mtv\\|mps\\|p2\\|djanimations\\|mpg4\\|swi\\|mpeg\\|m2ts\\|rdg\\|flux\\|cct\\|vs4\\|rmvb\\|flm\\|vmlf\\|rt4\\|mj2\\|ts4\\|awlive\\|m65\\|ifv\\|3gpp\\|r3d\\|rm\\|g2m\\|mpv\\|prel\\|otrkey\\|mpgv\\|urc\\|wsve\\|mmv\\|dmsm\\|rpl\\|fsv\\|cpvc\\|jyk\\|rargb\\|mvc\\|ppj\\|dvr-ms\\|mpgx\\|swc\\|f4v\\|avd\\|rv\\|gmm\\|vp9\\|qt\\|w32\\|eye\\|str\\|ml20\\|60d\\|bdav\\|jmf\\|cpk\\|bdmv\\|dscf\\|fm2\\|tsp\\|dvm\\|mxv\\|mp4\\|lsf\\|tmi\\|eva\\|w3d\\|m1v\\|rcproject\\|fmv\\|xvw\\|aut\\|ilm\\|dlx\\|fcp\\|mpeg2\\|dv\\|swz\\|ty\\|mv\\|mpcpl\\|m2v\\|dtv\\|rax\\|g64\\|iva\\|epj\\|rec_part1\\|vcl\\|san\\|rtv\\|pgmx\\|vpg\\|m2s\\|xpv\\|ivm\\|movie\\|pxm\\|flexolibrary\\|divx\\|seq\\|ogm\\|mvd\\|mvy\\|slc\\|theater\\|mpeg4\\|cvc\\|y4m\\|kmv\\|rca\\|hlv\\|wtv\\|44\\|dmss\\|mjp\\|rvl\\|h263\\|ssm\\|lsproj\\|htd\\|gir\\|mv4\\|603\\|tms\\|vp8\\|mkv3D\\|hgd\\|s4ud\\|flc\\|vdr\\|cx3\\|hmv\\|vg2\\|hq\\|m-jpeg\\|gts\\|iis\\|mcv\\|mv2\\|tgv\\|nde\\|st4\\|bdtp\\|dvx\\|ratDVD\\|stj\\|wmv3\\|890\\|tp0\\|mpegps\\|film\\|pmf\\|mqv\\|smv\\|fli\\|vep4\\|nxv\\|scc\\|k3g\\|vs2\\|3mm\\|263\\|jmm\\|apz\\|s11\\|gvi\\|mbv\\|mjp\\|xmv\\|lsx\\|dvddata\\|ev2\\|fvt\\|$ts\\|dof\\|svcd\\|dgw\\|pyv\\|tvv\\|aqt\\|bsf\\|rcrec\\|bbv\\|xtodvd\\|gxf\\|mvv\\|vod\\|ivs\\|sqz\\|mjp2\\|l32\\|dsm\\|flh\\|ncor\\|imovietrailer\\|s2e\\|mio\\|261\\|m1s\\|flvat\\|800\\|l3\\|rtsp\\|splash\\|vid\\|imovieproj\\|jpv\\|am2\\|hnm\\|uvs\\|tivo\\|rec_part2\\|mgv\\|vg\\|vivo\\|vc1\\|vcm\\|dvdrip\\|dcr\\|demo4\\|fli_\\|qtc\\|qtm\\|wfsp\\|mpeg1\\|sbst\\|dcr\\|dwz\\|pxv\\|3gpp2\\|sbs\\|am7\\|zeg\\|ssw\\|wm3\\|rki\\|bub\\|pmp\\|h4v\\|axm\\|scm\\|bay\\|rec_part3\\|video\\|kava\\|roq\\|tridefmovie\\|htp\\|mep\\|hls\\|xas\\|lza\\|rvx\\|viewlet\\|svi\\|moov\\|nvl\\|tdt2\\|mvf\\|mpe\\|noa\\|3gp2\\|wm\\|ogx\\|4xm\\|moo\\|olproj\\|rum\\|c2r\\|flm\\|clk\\|vcpf\\|mpg2\\|qmx\\|crv\\|xmm\\|xlmv\\|hdmov\\|mp4v\\|arcut\\|nvavi\\|el8\\|d2v\\|buy\\|dvsd\\|mcf\\|bs4\\|siv\\|mvr\\|px\\|mhg\\|stk\\|ipr\\|tv\\|qvt\\|ml20\\|mmp\\|aec\\|mp7\\|qcif\\|vprj\\|jts\\|m21\\|vpd\\|biz\\|ppp\\|vp7\\|scn\\|vgx\\|vcv\\|dpg\\|pvr\\|mpj\\|osp\\|tab\\|dfv\\|mp2v\\|cak\\|vp5\\|ftvx\\|daproj\\|3ivx\\|pproj\\|avb\\|h261\\|zrb\\|dv-avi\\|cam\\|par\\|tvs\\|wpe\\|drc\\|grasp\\|dvdmedia\\|mnv\\|gfp\\|avr\\|asdvdcrtproj\\|hkm\\|orv\\|qtvr\\|cmmp\\|gdwx\\|ivr\\|mv1\\|ctd\\)$" file-name))))

(use-package openwith
  :ensure t
  :validate-custom
  (openwith-associations
   (cond
    ((string-equal system-type "darwin")
     '(("\\.\\(dmg\\|doc\\|docs\\|xls\\|xlsx\\)$"
        "open" (file))
       ("\\.\\(aiff\\|mp4\\|mp3\\|mkv\\|webm\\|avi\\|flv\\|mov\\)$"
        "open" ("-a" "mpv" file))))
    ((string-equal system-type "gnu/linux")
     '(("\\.\\(mp4\\|mp3\\|mkv\\|webm\\|avi\\|flv\\|mov\\)$"
        "xdg-open" (file))))))
  :config
  (openwith-mode +1))

(use-package tramp
  :validate-custom
  ;; Favor .ssh/config instead.
  (tramp-use-ssh-controlmaster-options nil)
  :config
  ;; Use remote PATH on tramp (handy for eshell).
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;; make sure vc stuff is not making tramp slower
  (setq vc-ignore-dir-regexp
	(format "%s\\|%s"
		vc-ignore-dir-regexp
		tramp-file-name-regexp)))
