(use-package ediff
  :commands (ediff-backup
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
  (csetq ediff-window-setup-function #'ediff-setup-windows-plain)
  (csetq ediff-split-window-function #'split-window-horizontally)

  (use-package outline
    :after outline
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
        (call-interactively #'ediff-buffers))))))
