;;; -*- lexical-binding: t; -*-


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
  ("e" ar/misc-open-in-external-app nil)
  ("p" ar/misc-open-file-at-point nil)
  ("q" nil "cancel"))

(defun ar/file-open-closest-build-file-dwim ()
  (interactive)
  (if (locate-dominating-file default-directory "WORKSPACE")
      (call-interactively 'ar/bazel-jump-to-build-rule)
    (call-interactively 'ar/file-open-closest-build-file)))

(defhydra hydra-open-prog-mode (:color blue)
  "open"
  ("o" ff-find-other-file "other")
  ("e" ar/misc-open-in-external-app "externally")
  ("u" ar/misc-open-file-at-point "url at point")
  ("b" ar/file-open-closest-build-file-dwim "build file")
  ("q" nil "cancel"))

;; https://github.com/abo-abo/hydra/wiki/Version-Control#git-gutter
(defhydra hydra-git-gutter (:body-pre (git-gutter-mode +1)
                                      :hint nil)
  "
Git gutter:
  _n_: next hunk        _s_tage hunk   _q_uit
  _p_: previous hunk    _k_ill hunk    _Q_uit and deactivate git-gutter
  ^ ^                   _d_iff hunk
  _<_: first hunk
  _>_: last hunk        set start _R_evision
"
  ("n" git-gutter:next-hunk)
  ("p" git-gutter:previous-hunk)
  ("<" (progn (goto-char (point-min))
              (git-gutter:next-hunk 1)))
  (">" (progn (goto-char (point-min))
              (git-gutter:previous-hunk 1)))
  ("s" git-gutter:stage-hunk)
  ("k" (lambda ()
         (interactive)
         (git-gutter:revert-hunk)
         (call-interactively #'git-gutter:next-hunk)) nil)
  ("d" git-gutter:popup-hunk)
  ("R" git-gutter:set-start-revision)
  ("q" nil :color blue)
  ("Q" (progn (git-gutter-mode -1)
              ;; git-gutter-fringe doesn't seem to
              ;; clear the markup right away
              (sit-for 0.1)
              (git-gutter:clear))
   :color blue))

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
