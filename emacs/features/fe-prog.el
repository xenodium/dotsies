;;; -*- lexical-binding: t; -*-

(use-package sh-script
  :after ivy
  :bind (:map
         sh-mode-map
         ("C-c C-c" . ar/compile)
         ("C-c C-r" . ivy-resume)))

(use-package lisp
  :bind (:map
         prog-mode-map
         ("C-<" . beginning-of-defun)
         ("C->" . end-of-defun)))

(use-package newcomment
  :bind (:map
         prog-mode-map
         ;; Better comment functionality
         ;; https://emacsredux.com/blog/2020/06/10/comment-commands-redux/
         ("M-;" . 'comment-line)
         ("C-M-;" . 'comment-line)))

;; Highlight TODO, FIXME....
(use-package hl-todo :ensure t
  :hook ((prog-mode . hl-todo-mode)))

;; prog-mode is loaded super early during launch/initialization.
(use-package prog-mode
  :bind (:map
         prog-mode-map
         ("C-x C-q" . view-mode))
  :hook ((prog-mode . company-mode)
         ;; Trying to go without.
         ;; (prog-mode . centered-cursor-mode)
         (prog-mode . rainbow-mode)
         (prog-mode . goto-address-prog-mode))
  :config
  ;; Trying without (for performance).
  ;;  (require 'flycheck)

  ;; Highlight hex strings in respective color.
  (use-package rainbow-mode
    :ensure t
    :config
    ;; Enable more color highlighting cases in prog modes.
    (mapc (lambda (mode)
            (add-to-list 'rainbow-x-colors-major-mode-list mode)
            (add-to-list 'rainbow-html-colors-major-mode-list mode))
          '(objc-mode
            swift-mode)))

  (defvar ar/unique-log-word "Yay")

  (defun ar/insert-unique-log-word (prefix)
    "Inserts `ar/unique-log-word' incrementing counter.

With PREFIX, change `ar/unique-log-word'."
    (interactive "P")
    (let* ((word (cond (prefix
                        (setq ar/unique-log-word
                              (read-string "Log word: ")))
                       ((region-active-p)
                        (setq ar/unique-log-word
                              (buffer-substring (region-beginning)
                                                (region-end))))
                       (ar/unique-log-word
                        ar/unique-log-word)
                       (t
                        "Reached")))
           (config
            (cond
             ((equal major-mode 'emacs-lisp-mode)
              (cons (format "(message \"%s: \\([0-9]+\\)\")" word)
                    (format "(message \"%s: %%s\")" word)))
             ((equal major-mode 'swift-mode)
              (cons (format "print(\"%s: \\([0-9]+\\)\")" word)
                    (format "print(\"%s: %%s\")" word)))
             ((equal major-mode 'ada-mode)
              (cons (format "Ada.Text_Io.Put_Line (\"%s: \\([0-9]+\\)\");" word)
                    (format "Ada.Text_Io.Put_Line (\"%s: %%s\");" word)))
             ((equal major-mode 'c++-mode)
              (cons (format "std::cout << \"%s: \\([0-9]+\\)\" << std::endl;" word)
                    (format "std::cout << \"%s: %%s\" << std::endl;" word)))
             (t
              (error "%s not supported" major-mode))))
           (match-regexp (car config))
           (format-string (cdr config))
           (max-num 0)
           (case-fold-search nil))
      (when ar/unique-log-word
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward match-regexp nil t)
            (when (> (string-to-number (match-string 1)) max-num)
              (setq max-num (string-to-number (match-string 1))))))
        (setq max-num (1+ max-num)))
      (unless (looking-at-p "^ *$")
        (end-of-line))
      (insert (concat
               (if (looking-at-p "^ *$") "" "\n")
               (format format-string
                       (if ar/unique-log-word
                           (number-to-string (1+ max-num))
                         (string-trim
                          (shell-command-to-string
                           "grep -E '^[a-z]{6}$' /usr/share/dict/words | shuf -n 1"))))))
      (call-interactively 'indent-for-tab-command))))

(use-package reformatter
  :defer
  :ensure t)

(use-package re-builder
  :commands (re-builder
             regexp-builder)
  :validate-custom
  (reb-re-syntax 'string))
