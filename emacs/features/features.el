;; Only use ar/init--idle-load with string literal paths.
(if ar/init-debug-init
    (defmacro ar/init--idle-load (library)
      `(load ,library))
  (defmacro ar/init--idle-load (library)
    `(run-with-idle-timer 0.5 nil
                          (lambda ()
                            (load ,library)))))

;; Load before remaining. Useful for debugging init.el issues.
(ar/init--idle-load "~/.emacs.d/features/maintenance.el")

;; Load all others on idle. Alphabetically listed.
(ar/init--idle-load "~/.emacs.d/features/alert.el")
(ar/init--idle-load "~/.emacs.d/features/bazel.el")
(ar/init--idle-load "~/.emacs.d/features/buffers.el")
(ar/init--idle-load "~/.emacs.d/features/cc.el")
(ar/init--idle-load "~/.emacs.d/features/company.el")
(ar/init--idle-load "~/.emacs.d/features/compile.el")
(ar/init--idle-load "~/.emacs.d/features/crux.el")
(ar/init--idle-load "~/.emacs.d/features/dev.el")
(ar/init--idle-load "~/.emacs.d/features/dired.el")
(ar/init--idle-load "~/.emacs.d/features/ediff.el")
(ar/init--idle-load "~/.emacs.d/features/editing.el")
(ar/init--idle-load "~/.emacs.d/features/elfeed.el")
(ar/init--idle-load "~/.emacs.d/features/elisp.el")
(ar/init--idle-load "~/.emacs.d/features/eshell.el")
(ar/init--idle-load "~/.emacs.d/features/file.el")
(ar/init--idle-load "~/.emacs.d/features/files.el")
(ar/init--idle-load "~/.emacs.d/features/flycheck.el")
(ar/init--idle-load "~/.emacs.d/features/flyspell.el")
(ar/init--idle-load "~/.emacs.d/features/git.el")
(ar/init--idle-load "~/.emacs.d/features/golang.el")
(ar/init--idle-load "~/.emacs.d/features/helm.el")
(ar/init--idle-load "~/.emacs.d/features/help.el")
(ar/init--idle-load "~/.emacs.d/features/hydra.el")
(ar/init--idle-load "~/.emacs.d/features/images.el")
(ar/init--idle-load "~/.emacs.d/features/info.el")
(ar/init--idle-load "~/.emacs.d/features/ios.el")
(ar/init--idle-load "~/.emacs.d/features/ivy.el")
(ar/init--idle-load "~/.emacs.d/features/ledger.el")
(ar/init--idle-load "~/.emacs.d/features/modal.el")
(ar/init--idle-load "~/.emacs.d/features/navigation.el")
(ar/init--idle-load "~/.emacs.d/features/org.el")
(ar/init--idle-load "~/.emacs.d/features/paradox.el")
(ar/init--idle-load "~/.emacs.d/features/platform.el")
(ar/init--idle-load "~/.emacs.d/features/proced.el")
(ar/init--idle-load "~/.emacs.d/features/prog.el")
(ar/init--idle-load "~/.emacs.d/features/protobuf.el")
(ar/init--idle-load "~/.emacs.d/features/python.el")
(ar/init--idle-load "~/.emacs.d/features/swift.el")
(ar/init--idle-load "~/.emacs.d/features/web.el")
(ar/init--idle-load "~/.emacs.d/features/yasnippet.el")
(ar/init--idle-load "~/.emacs.d/features/pdf.el")
(ar/init--idle-load "~/.emacs.d/features/plantuml.el")
(ar/init--idle-load "~/.emacs.d/features/lua.el")
(ar/init--idle-load "~/.emacs.d/downloads/company-async-files/company-async-files.el")

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
(ar/init--idle-load "~/.emacs.d/features/server.el")
