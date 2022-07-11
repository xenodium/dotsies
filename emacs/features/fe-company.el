;;; -*- lexical-binding: t; -*-
(use-package company
  :ensure t
  :commands (company-mode
             global-company-mode company-complete
             company-complete-common
             company-manual-begin
             company-grab-line)
  :validate-custom
  ;; Disable all company backends by default.
  (company-backends '())
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.1)
  (company-tooltip-align-annotations t)
  :bind (:map global-map
              ("<backtab>" . company-complete)
              :map company-search-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              :map company-active-map
              ("C-l" . company-show-location)
              ("C-s" . company-filter-candidates)
              ("C-d" . company-show-doc-buffer)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :config
  (use-package company-dabbrev
    :validate-custom
    (company-dabbrev-downcase nil)
    (company-dabbrev-ignore-case nil))

  (use-package company-dabbrev-code
    :validate-custom
    (company-dabbrev-code-ignore-case nil))

  (use-package company-rfiles)
  (use-package company-projectile-cd)

  (when (display-graphic-p)
    (use-package company-box
      :ensure t
      :hook (company-mode . company-box-mode)
      :validate-custom
      ;; Make all backends same color.
      ;; Didn't like yasnippets colored differently.
      (company-box-backends-colors nil)
      :init
      ;; Needed to avoid error:
      ;; Eager macro-expansion failure: (void-function all-the-icons-faicon)
      (use-package all-the-icons
        :ensure t)

      ;; Based on https://github.com/hlissner/doom-emacs/commit/6f273ffc253e5e2f863fe1c7dd1684238a21d03f#diff-b7bf3ccb35ec6e4125e00a39cfef9174
      (defun adviced:company-box--update-scrollbar (orig-fn &rest args)
        "Hides scrollbar"
        (cl-letf (((symbol-function #'display-buffer-in-side-window)
                   (symbol-function #'ignore)))
          (apply orig-fn args)))

      (advice-add #'company-box--update-scrollbar
                  :around
                  #'adviced:company-box--update-scrollbar))))
