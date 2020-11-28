;;; -*- lexical-binding: t; -*-
(require 'cl-lib)
(use-package ediff
  :validate-custom
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-split-window-function #'split-window-horizontally)
  :commands (ar/ediff-dir-content-file-sizes
             ediff-backup
             ediff-buffers
             ediff-buffers3
             ediff-current-file
             ediff-directories
             ediff-directory-revisions
             ediff-files
             ediff-files3
             ediff-merge-buffers
             ediff-merge-buffers-with-ancestor
             ediff-merge-directories
             ediff-merge-directories-with-ancestor
             ediff-merge-directory-revisions
             ediff-merge-directory-revisions-with-ancestor
             ediff-merge-files
             ediff-merge-files-with-ancestor
             ediff-merge-revisions
             ediff-merge-revisions-with-ancestor
             ediff-merge-with-ancestor
             ediff-patch-buffer
             ediff-patch-file
             ediff-regions-linewise
             ediff-regions-wordwise
             ediff-windows-linewise
             ediff-windows-wordwise
             edir-merge-revisions
             edirs-merge)
  ;; Automatically highlight first change.
  :hook ((ediff-startup . ediff-next-difference)
         (ediff-before-setup . ar/ediff-bsh)
         (ediff-after-setup-windows . ar/ediff-aswh);
         (ediff-quit . ar/ediff-qh))
  :init
  ;; ediff-revision cleanup.
  ;; From http://www.emacswiki.org/emacs/DavidBoon#toc8
  (defvar ar/ediff-bwin-config nil
    "Window configuration before ediff.")

  (defvar ar/ediff-bwin-reg ?b
    "Register to be set up to hold ar/ediff-bwin-config configuration.")

  (defun ar/ediff-bsh ()
    "Function to be called before any buffers or window setup for ediff."
    (remove-hook 'ediff-quit-hook #'ediff-cleanup-mess)
    (window-configuration-to-register ar/ediff-bwin-reg))

  (defun ar/ediff-aswh ()
    "Setup hook used to remove the `ediff-cleanup-mess' function.  It causes errors."
    (remove-hook 'ediff-quit-hook #'ediff-cleanup-mess))

  (defun ar/ediff-qh ()
    "Function to be called when ediff quits."
    (remove-hook 'ediff-quit-hook #'ediff-cleanup-mess)
    (ediff-cleanup-mess)
    (jump-to-register ar/ediff-bwin-reg))

  :config
  (use-package outline
    ;; Ensure ediff expands org files.
    :hook (ediff-prepare-buffer . outline-show-all))

  ;; Expand org files when ediffing.
  (add-hook 'ediff-prepare-buffer-hook
            (lambda ()
              (when (eq major-mode 'org-mode)
                (visible-mode +1)  ; default 0
                (setq-local truncate-lines nil)  ; no `org-startup-truncated' in hook
                (setq-local org-hide-leading-stars t))))

  ;; From https://stackoverflow.com/questions/9656311/conflict-resolution-with-emacs-ediff-how-can-i-take-the-changes-of-both-version
  (defun ar/ediff-copy-both-to-C ()
    "Ediff copy A and B to C."
    (interactive)
    (ediff-copy-diff ediff-current-difference nil 'C nil
                     (concat
                      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

  ;; From https://scripter.co/do-ediff-as-i-mean/
  (defun ar/ediff-dwim ()
    "Do ediff as I mean.

If a region is active, call `ediff-regions-wordwise'.
Else if the frame has 2 windows with identical major modes,
  - Do `ediff-files' if the buffers are associated to files and the buffers
    have not been modified.
  - Do `ediff-buffers' otherwise.
Else if the current is a file buffer with a VC backend, call `vc-ediff'
Else call `ediff-buffers'."
    (interactive)
    (let* ((num-win (safe-length (window-list)))
           (bufa (get-buffer (buffer-name)))
           (filea (buffer-file-name bufa))
           (modea (with-current-buffer bufa major-mode))
           bufb fileb modeb)
      (save-excursion
        (other-window 1)
        (setq bufb (get-buffer (buffer-name)))
        (setq fileb (buffer-file-name bufb))
        (setq modeb (with-current-buffer bufb major-mode)))
      (cond
       ;; If a region is selected
       ((region-active-p)
        (call-interactively #'ediff-regions-wordwise))
       ;; Else if 2 windows with same major modes
       ((and (= 2 num-win)
             (eq modea modeb))
        (if ;; If either of the buffers is not associated to a file,
            ;; or if either of the buffers is modified
            (or (null filea)
                (null fileb)
                (buffer-modified-p bufa)
                (buffer-modified-p bufb))
            (progn
              (message "Running (ediff-buffers \"%s\" \"%s\") .." bufa bufb)
              (ediff-buffers bufa bufb))
          (progn
            (message "Running (ediff-files \"%s\" \"%s\") .." filea fileb)
            (ediff-files filea fileb))))
       ;; Else if file in current buffer has a vc backend
       ((and filea
             (vc-registered filea))
        (call-interactively #'vc-ediff))
       ;; Else call `ediff-buffers'
       (t
        (call-interactively #'ediff-buffers)))))

  (defun ar/ediff-dir-content-file-sizes ()
    (interactive)
    "Diff two directories file sizes."
    (ar/ediff--dir-content-sizes-with-command "find . -type f -exec stat -f '%N %z' '{}' \\; | sort"))

  (defun ar/ediff-dir-content-dir-sizes ()
    "Diff two directories for subdirectory sizes."
    (interactive)
    (ar/ediff--dir-content-sizes-with-command "find . -type d | sort | du -h"))

  (defun ar/ediff-archive-content-file-sizes ()
    "Diff two archives, looking at file sizes."
    (interactive)
    (ar/ediff--archive-content-sizes-with-command "find . -type f -exec stat -f '%N %z' '{}' \\; | sort"))

  (defun ar/ediff-archive-content-dir-sizes ()
    "Diff two archives, looking at subdirectory sizes."
    (interactive)
    (ar/ediff--archive-content-sizes-with-command "find . -type d | sort | du -h"))

  (defun ar/ediff--dir-content-sizes-with-command (find-cmd)
    "Diff all subdirectories (sizes only) in two directories and list content using FIND-CMD."
    (let* ((dir1-path (read-directory-name "Dir 1: "))
           (dir2-path (read-directory-name "Dir 2: "))
           (buf1 (get-buffer-create (format "*Dir 1 (%s)*" (f-base dir1-path))))
           (buf2 (get-buffer-create (format "*Dir 2 (%s)*" (f-base dir2-path)))))
      (with-current-buffer buf1
        (read-only-mode -1)
        (erase-buffer)
        (shell-command (format "cd \"%s\"; %s" dir1-path find-cmd) buf1)
        (read-only-mode +1))
      (with-current-buffer buf2
        (read-only-mode -1)
        (erase-buffer)
        (shell-command (format "cd \"%s\"; %s" dir2-path find-cmd) buf2)
        (read-only-mode +1))
      (ediff-buffers buf1 buf2)))

  (defun ar/ediff--archive-content-sizes-with-command (find-cmd)
    "Diff all subdirectories (sizes only) in two archives and list content using FIND-CMD."
    (let* ((arch1-path (read-file-name "Archive 1: "))
           (arch2-path (read-file-name "Archive 2: "))
           (dir1-path (nth 0 (process-lines "mktemp"  "-d" "-t" "arch1.XXXXXX")))
           (dir2-path (nth 0 (process-lines "mktemp"  "-d" "-t" "arch2.XXXXXX")))
           (buf1 (get-buffer-create (format "*archive diff (%s)*" (f-filename arch1-path))))
           (buf2 (get-buffer-create (format "*archive diff (%s)*" (f-filename arch2-path))))
           (expand-cmd-fun (lambda (file)
                             (cond ((string-match-p ".*\.tar.bz2" file)
                                    "tar --strip-components=1 -x -z -f")
                                   ((string-match-p ".*\.tar.gz" file)
                                    "tar --strip-components=1 -x -z -f")
                                   ((string-match-p ".*\.bz2" file)
                                    "bunarch2")
                                   ((string-match-p ".*\.rar" file)
                                    "unrar x")
                                   ((string-match-p ".*\.gz" file)
                                    "gunzip")
                                   ((string-match-p ".*\.tar" file)
                                    "tar --strip-components=1 -x -f")
                                   ((string-match-p ".*\.tbz2" file)
                                    "tar --strip-components=1 -x -j -f")
                                   ((string-match-p ".*\.tgz" file)
                                    "tar --strip-components=1 -x -z -f")
                                   ((string-match-p ".*\.zip" file)
                                    "unzip")
                                   ((string-match-p ".*\.jar" file)
                                    "unzip")
                                   ((string-match-p ".*\.Z" file)
                                    "uncompress")
                                   (t
                                    (error "Don't know how to extract %s" file))))))
      (with-current-buffer buf1
        (read-only-mode -1)
        (erase-buffer)
        (cl-assert (eq 0 (shell-command (format "cd \"%s\"; %s \"%s\"" dir1-path (apply expand-cmd-fun
                                                                                     (list arch1-path)) arch1-path)
                                     "*archive diff*"))
                nil (format "Could not extract %s" arch1-path))
        (cl-assert (eq 0 (shell-command (format "cd \"%s\"; %s" dir1-path find-cmd) buf1))
                nil (format "Could not list files in %s" dir1-path))
        (read-only-mode +1))
      (with-current-buffer buf2
        (read-only-mode -1)
        (erase-buffer)
        (cl-assert (eq 0 (shell-command (format "cd \"%s\"; %s \"%s\"" dir2-path (apply expand-cmd-fun
                                                                                     (list arch2-path)) arch2-path)
                                     "*archive diff*"))
                nil (format "Could not extract %s" arch2-path))
        (cl-assert (eq 0 (shell-command (format "cd \"%s\"; %s" dir2-path find-cmd) buf2))
                nil (format "Could not list files in %s" dir2-path))
        (read-only-mode +1))
      (ediff-buffers buf1 buf2))))

;; Diff directories
(use-package ztree
  :ensure t
  :commands ztree-diff)
