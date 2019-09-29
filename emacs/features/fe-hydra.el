(require 'ar-vsetq)

(use-package hydra
  :ensure t
  :hook (vc-git-log-edit-mode . hydra-vc-log-edit/body)
  :bind (("C-c s" . hydra-search/body)
         ("C-c x" . hydra-quick-insert/body)
         ("C-c o" . ar/hydra-open-dwim)
         ("C-c g" . hydra-git-gutter/body)
         ("C-c 1" . hydra-profile/body))
  :config
  (ar/vsetq hydra-is-helpful t)

  (use-package vc-git)

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
    ("i" ar/helm-ag-insert "insert match")
    ("m" mu4e-headers-search "search email")
    ("q" nil "quit"))

  (defhydra hydra-quick-insert (:color blue)
    "
Quick insert: _w_eb bookmark or backlog
              _t_odo _d_one
"
    ("w" ar/ivy-org-add-bookmark-dwim nil)
    ("t" ar/org-add-todo nil)
    ("d" ar/org-add-done nil)
    ("q" nil nil :color blue))

  (defun ar/hydra-open-dwim ()
    "Choose \"open\" hydra based on current mode."
    (interactive)
    (cond ((derived-mode-p 'c-mode) (hydra-open-c-mode/body))
          ((derived-mode-p 'prog-mode) (hydra-open-prog-mode/body))
          ((derived-mode-p 'protobuf-mode) (hydra-open-prog-mode/body))
          (t (hydra-open/body))))

  (defhydra hydra-open-c-mode (:color blue)
    "open"
    ("o" ff-find-other-file "other")
    ("e" ar/platform-open-in-external-app "externally")
    ("u" ar/platform-open-file-at-point "url at point")
    ("b" ar/file-open-closest-build-file "build file")
    ("q" nil "cancel"))

  (defhydra hydra-open (:color blue)
    "
Open: _p_oint _e_xternally
"
    ("e" ar/platform-open-in-external-app nil)
    ("p" ar/platform-open-file-at-point nil)
    ("q" nil "cancel"))

  (defhydra hydra-open-prog-mode (:color blue)
    "open"
    ("o" ff-find-other-file "other")
    ("e" ar/platform-open-in-external-app "externally")
    ("u" ar/platform-open-file-at-point "url at point")
    ("b" ar/file-open-closest-build-file "build file")
    ("q" nil "cancel"))

  (defhydra hydra-git-gutter (:pre (git-gutter-mode +1))
    "
Git: _n_ext     _s_tage  _d_iff
     _p_revious _k_ill _q_uit
"
    ("n" git-gutter:next-hunk nil)
    ("p" git-gutter:previous-hunk nil)
    ("s" git-gutter:stage-hunk nil)
    ("k" (lambda ()
           (interactive)
           (git-gutter:revert-hunk)
           (call-interactively #'git-gutter:next-hunk)) nil)
    ("d" git-gutter:popup-hunk nil)
    ("q" nil nil :color blue))

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
    ("q" nil "quit"))

  ;; From http://oremacs.com/2015/03/07/hydra-org-templates
(defun ar/org-expand (str)
  "Expand org template STR."
  (insert str)
  (org-try-structure-completion)))
