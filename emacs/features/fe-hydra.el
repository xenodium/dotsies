;;; -*- lexical-binding: t; -*-

(use-package hydra
  :ensure t
  :hook (vc-git-log-edit-mode . hydra-vc-log-edit/body)
  :bind (("M-s" . hydra-search/body)
         ("C-c s" . hydra-search/body)
         ("C-c x" . hydra-quick-insert/body)
         ("C-c o" . ar/hydra-open-dwim)
         ("C-c g" . diff-hl/body)
         ("C-c 1" . hydra-profile/body))
  :validate-custom
  (hydra-is-helpful t)
  :config
  (defhydra hydra-profile (:color blue)
    "profiling"
    ("b" ar/profiler-start-cpu "begin")
    ("r" profiler-report "report")
    ("e" profiler-stop "end")
    ("q" nil "quit"))

  (defhydra hydra-search (:color blue)
    "search"
    ("d" ar/counsel-ag "search directory")
    ("a" ar/find-all-dired-current-dir "find all files")
    ("f" ar/counsel-find "find files")
    ("m" mu4e-headers-search "search email")
    ("q" nil "quit"))

  (defhydra hydra-quick-insert (:color blue)
    "
Quick insert: _w_eb bookmark or backlog"
    ("w" ar/ivy-org-add-bookmark-dwim nil)
    ("q" nil nil :color blue))

  (defun ar/hydra-open-dwim ()
    "Choose \"open\" hydra based on current mode."
    (interactive)
    (cond ((derived-mode-p 'c-mode) (hydra-open-prog-mode/body))
          ((derived-mode-p 'prog-mode) (hydra-open-prog-mode/body))
          ((derived-mode-p 'protobuf-mode) (hydra-open-prog-mode/body))
          (t (hydra-open/body))))

  (defhydra hydra-dired-sort (:color blue)
    "sort"
    ("d" ar/dired-sort-by-date "date")
    ("a" ar/dired-sort-by-name "alpha")
    ("s" ar/dired-sort-by-size "size")
    ("q" nil "cancel"))

  (defhydra hydra-open (:color blue)
    "
Open: _p_oint _e_xternally
"
    ("e" crux-open-with nil)
    ("p" ar/misc-open-file-at-point nil)
    ("q" nil "cancel"))

  (defun ar/open-closest-build-file-dwim ()
    (interactive)
    (if (locate-dominating-file default-directory "WORKSPACE")
        (call-interactively 'ar/bazel-jump-to-build-rule)
      (call-interactively 'ar/file-open-closest-build-file)))

  (defhydra hydra-open-prog-mode (:color blue)
    "open"
    ("o" ff-find-other-file "other")
    ("e" dwim-shell-commands-open-externally "externally")
    ("u" ar/misc-open-file-at-point "url at point")
    ("b" ar/open-closest-build-file-dwim "build file")
    ("q" nil "cancel"))

  (defhydra diff-hl (:body-pre (diff-hl-mode +1)
                               :hint nil)
    "
diff hl:
  _n_: next hunk        _k_ill hunk
  _p_: previous hunk    _d_iff hunk
  ^ ^
  _<_: first hunk       _q_uit
  _>_: last hunk
"
    ("n" diff-hl-next-hunk)
    ("p" diff-hl-previous-hunk)
    ("<" (progn (goto-char (point-min))
                (diff-hl-next-hunk 1)))
    (">" (progn (goto-char (point-min))
                (diff-hl-previous-hunk 1)))
    ("k" diff-hl-revert-hunk nil)
    ("d" diff-hl-diff-goto-hunk)
    ("q" nil :color blue))

  (defhydra hydra-vc-log-edit (:color blue :hint nil)
    "
_u_pdate _r_eview comments
_t_ypo
"
    ("u" (lambda ()
           (interactive)
           (insert "Updating")
           (log-edit-done)))
    ("t" (lambda ()
           (interactive)
           (insert "Fixing typo")
           (log-edit-done)))
    ("r" (lambda ()
           (interactive)
           (insert "Addressing review comments")
           (log-edit-done)))
    ("q" nil "quit")))
