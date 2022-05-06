;;; -*- lexical-binding: t; -*-

(use-package flyspell
  :defer
  :bind (:map
         flyspell-mode-map
         ("C-M-i" . flyspell-correct-wrapper))
  :config
  (use-package flyspell-correct
    :ensure t
    :config
    (use-package flyspell-correct-popup
      :ensure t
      :init
      (defun ar/flyspell-correct-popup-then-abbrev (candidates mispelled-word)
        (let ((selection (flyspell-correct-popup candidates mispelled-word)))
          (unless (consp selection)
            (define-abbrev global-abbrev-table mispelled-word selection))
          selection))
      :validate-custom
      (flyspell-correct-interface #'ar/flyspell-correct-popup-then-abbrev)))

  (use-package abbrev
    :validate-custom
    (abbrev-file-name "~/stuff/active/code/dots/emacs/abbrev_defs")
    (save-abbrevs 'silently)
    :config
    (setq-default abbrev-mode t))

  (use-package ispell
    :ensure-system-package aspell
    :config
    ;; http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
    (cond
     ;; if hunspell NOT installed, fallback to aspell
     ((executable-find "hunspell")
      ;; In addition to "brew install hunspell" download dicts to
      ;; ~/Library/Spelling/
      ;; https://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_GB.aff
      ;; https://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_GB.dic
      (setq ispell-program-name "hunspell")
      (setq ispell-local-dictionary "en_GB")
      (setq ispell-local-dictionary-alist
            '(("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_GB") nil utf-8))))
     ((executable-find "aspell")
      (setq ispell-program-name "aspell")
      (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_GB")))
     (t
      (error "No speller installed")))))

(use-package wucuo
  :ensure t
  :hook ((text-mode . wucuo-start)
         (prog-mode . wucuo-start)))

(use-package mw-thesaurus
  :ensure t
  :commands mw-thesaurus-lookup-at-point)

(use-package auto-dictionary
  :commands adict-change-dictionary
  :ensure t
  :hook (message-mode . auto-dictionary-mode))
