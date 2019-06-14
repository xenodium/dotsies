;; Only use ar/init--idle-load with string literal paths.
(if ar/init-debug-init
    (defmacro ar/init--idle-load (library)
      `(load ,library))
  (defmacro ar/init--idle-load (library)
    `(run-with-idle-timer 0.5 nil
                          (lambda ()
                            (load ,library)))))

;; Load before remaining. Useful for debugging init.el issues.
(ar/init--idle-load "~/.emacs.d/features/fe-maintenance")

;; Load all others on idle. Alphabetically listed.
(ar/init--idle-load "~/.emacs.d/features/fe-alert")
(ar/init--idle-load "~/.emacs.d/features/fe-bazel")
(ar/init--idle-load "~/.emacs.d/features/fe-buffers")
(ar/init--idle-load "~/.emacs.d/features/fe-cc")
(ar/init--idle-load "~/.emacs.d/features/fe-company")
(ar/init--idle-load "~/.emacs.d/features/fe-compile")
(ar/init--idle-load "~/.emacs.d/features/fe-crux")
(ar/init--idle-load "~/.emacs.d/features/fe-dev")
(ar/init--idle-load "~/.emacs.d/features/fe-dired")
(ar/init--idle-load "~/.emacs.d/features/fe-ediff")
(ar/init--idle-load "~/.emacs.d/features/fe-elfeed")
(ar/init--idle-load "~/.emacs.d/features/fe-elisp")
(ar/init--idle-load "~/.emacs.d/features/fe-eshell")
(ar/init--idle-load "~/.emacs.d/features/fe-file")
(ar/init--idle-load "~/.emacs.d/features/fe-files")
(ar/init--idle-load "~/.emacs.d/features/fe-flycheck")
(ar/init--idle-load "~/.emacs.d/features/fe-flyspell")
(ar/init--idle-load "~/.emacs.d/features/fe-git")
(ar/init--idle-load "~/.emacs.d/features/fe-golang")
(ar/init--idle-load "~/.emacs.d/features/fe-helm")
(ar/init--idle-load "~/.emacs.d/features/fe-help")
(ar/init--idle-load "~/.emacs.d/features/fe-hydra")
(ar/init--idle-load "~/.emacs.d/features/fe-images")
(ar/init--idle-load "~/.emacs.d/features/fe-info")
(ar/init--idle-load "~/.emacs.d/features/fe-ios")
(ar/init--idle-load "~/.emacs.d/features/fe-ivy")
(ar/init--idle-load "~/.emacs.d/features/fe-ledger")
(ar/init--idle-load "~/.emacs.d/features/fe-modal")
(ar/init--idle-load "~/.emacs.d/features/fe-navigation")
(ar/init--idle-load "~/.emacs.d/features/fe-org")
(ar/init--idle-load "~/.emacs.d/features/fe-paradox")
(ar/init--idle-load "~/.emacs.d/features/fe-platform")
(ar/init--idle-load "~/.emacs.d/features/fe-proced")
(ar/init--idle-load "~/.emacs.d/features/fe-prog")
(ar/init--idle-load "~/.emacs.d/features/fe-protobuf")
(ar/init--idle-load "~/.emacs.d/features/fe-python")
(ar/init--idle-load "~/.emacs.d/features/fe-swift")
(ar/init--idle-load "~/.emacs.d/features/fe-java")
(ar/init--idle-load "~/.emacs.d/features/fe-web")
(ar/init--idle-load "~/.emacs.d/features/fe-yasnippet")
(ar/init--idle-load "~/.emacs.d/features/fe-pdf")
(ar/init--idle-load "~/.emacs.d/features/fe-plantuml")
(ar/init--idle-load "~/.emacs.d/features/fe-lua")
(ar/init--idle-load "~/.emacs.d/features/fe-misc")
(ar/init--idle-load "~/.emacs.d/downloads/company-async-files")
(ar/init--idle-load "~/.emacs.d/features/fe-editing")
(ar/init--idle-load "~/.emacs.d/features/fe-javascript")
(ar/init--idle-load "~/.emacs.d/features/fe-tags")
;; Keep last. It enables view-only mode in prog modes,
;; which interferes with installing some packages that write to .el files.
(ar/init--idle-load "~/.emacs.d/features/fe-view")

(run-with-idle-timer
 0.5 nil
 (lambda ()
   ;; Load local elisp.
   (dolist (file (file-expand-wildcards "~/.emacs.d/local/*.el"))
     ;; Not using ar/init--idle-load explicit paths not known.
     (load file))

   ;; Load work elisp.
   (dolist (file (file-expand-wildcards "~/.emacs.d/work/*.el"))
     ;; Not using ar/init--idle-load explicit paths not known.
     (load file))))

;; Start Emacs server.
(ar/init--idle-load "~/.emacs.d/features/fe-server")
