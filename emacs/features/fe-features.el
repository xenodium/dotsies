;; Only use ar/init--idle-load with string literal paths.
(if ar/init-debug-init
    (defmacro ar/init--idle-load (library)
      `(load ,library))
  (defmacro ar/init--idle-load (library)
    `(run-with-idle-timer 0.5 nil
                          (lambda ()
                            (load ,library)))))

;; Load before remaining. Useful for debugging init.el issues.
(ar/init--idle-load "~/.emacs.d/features/fe-maintenance.el")

;; Load all others on idle. Alphabetically listed.
(ar/init--idle-load "~/.emacs.d/features/fe-alert.el")
(ar/init--idle-load "~/.emacs.d/features/fe-bazel.el")
(ar/init--idle-load "~/.emacs.d/features/fe-buffers.el")
(ar/init--idle-load "~/.emacs.d/features/fe-cc.el")
(ar/init--idle-load "~/.emacs.d/features/fe-company.el")
(ar/init--idle-load "~/.emacs.d/features/fe-compile.el")
(ar/init--idle-load "~/.emacs.d/features/fe-crux.el")
(ar/init--idle-load "~/.emacs.d/features/fe-dev.el")
(ar/init--idle-load "~/.emacs.d/features/fe-dired.el")
(ar/init--idle-load "~/.emacs.d/features/fe-ediff.el")
(ar/init--idle-load "~/.emacs.d/features/fe-elfeed.el")
(ar/init--idle-load "~/.emacs.d/features/fe-elisp.el")
(ar/init--idle-load "~/.emacs.d/features/fe-eshell.el")
(ar/init--idle-load "~/.emacs.d/features/fe-file.el")
(ar/init--idle-load "~/.emacs.d/features/fe-files.el")
(ar/init--idle-load "~/.emacs.d/features/fe-flycheck.el")
(ar/init--idle-load "~/.emacs.d/features/fe-flyspell.el")
(ar/init--idle-load "~/.emacs.d/features/fe-git.el")
(ar/init--idle-load "~/.emacs.d/features/fe-golang.el")
(ar/init--idle-load "~/.emacs.d/features/fe-helm.el")
(ar/init--idle-load "~/.emacs.d/features/fe-help.el")
(ar/init--idle-load "~/.emacs.d/features/fe-hydra.el")
(ar/init--idle-load "~/.emacs.d/features/fe-images.el")
(ar/init--idle-load "~/.emacs.d/features/fe-info.el")
(ar/init--idle-load "~/.emacs.d/features/fe-ios.el")
(ar/init--idle-load "~/.emacs.d/features/fe-ivy.el")
(ar/init--idle-load "~/.emacs.d/features/fe-ledger.el")
(ar/init--idle-load "~/.emacs.d/features/fe-modal.el")
(ar/init--idle-load "~/.emacs.d/features/fe-navigation.el")
(ar/init--idle-load "~/.emacs.d/features/fe-org.el")
(ar/init--idle-load "~/.emacs.d/features/fe-paradox.el")
(ar/init--idle-load "~/.emacs.d/features/fe-platform.el")
(ar/init--idle-load "~/.emacs.d/features/fe-proced.el")
(ar/init--idle-load "~/.emacs.d/features/fe-prog.el")
(ar/init--idle-load "~/.emacs.d/features/fe-protobuf.el")
(ar/init--idle-load "~/.emacs.d/features/fe-python.el")
(ar/init--idle-load "~/.emacs.d/features/fe-swift.el")
(ar/init--idle-load "~/.emacs.d/features/fe-java.el")
(ar/init--idle-load "~/.emacs.d/features/fe-web.el")
(ar/init--idle-load "~/.emacs.d/features/fe-yasnippet.el")
(ar/init--idle-load "~/.emacs.d/features/fe-pdf.el")
(ar/init--idle-load "~/.emacs.d/features/fe-plantuml.el")
(ar/init--idle-load "~/.emacs.d/features/fe-lua.el")
(ar/init--idle-load "~/.emacs.d/features/fe-misc.el")
(ar/init--idle-load "~/.emacs.d/downloads/company-async-files.el")
(ar/init--idle-load "~/.emacs.d/features/fe-editing.el")
(ar/init--idle-load "~/.emacs.d/features/fe-javascript.el")
(ar/init--idle-load "~/.emacs.d/features/fe-tags.el")
;; Keep last. It enables view-only mode in prog modes,
;; which interferes with installing some packages that write to .el files.
(ar/init--idle-load "~/.emacs.d/features/fe-view.el")

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
(ar/init--idle-load "~/.emacs.d/features/fe-server.el")
