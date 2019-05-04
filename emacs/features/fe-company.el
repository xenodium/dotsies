(require 'ar-vsetq)
(require 'ar-csetq)

(use-package company
  :ensure t
  :commands (company-mode
             global-company-mode company-complete
             company-complete-common
             company-manual-begin
             company-grab-line)
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
    :config
    (ar/vsetq company-dabbrev-downcase nil)
    (ar/vsetq company-dabbrev-ignore-case nil))

  (use-package company-dabbrev-code
    :config
    (ar/vsetq company-dabbrev-code-ignore-case nil))

  (use-package company-grep)
  (use-package company-rfiles)
  (use-package company-projectile-cd)

  (when (display-graphic-p)
    (use-package company-box
      :ensure t
      :hook (company-mode . company-box-mode)
      :init
      ;; Needed to avoid error:
      ;; Eager macro-expansion failure: (void-function all-the-icons-faicon)
      (use-package all-the-icons
        :ensure t)
      :config
      (ar/csetq company-box-enable-icon nil)))

  (ar/vsetq company-idle-delay 0.2)
  (ar/vsetq company-show-numbers t)
  (ar/vsetq company-minimum-prefix-length 2)
  (ar/vsetq company-tooltip-align-annotations t)

  ;; Disable all company backends by default.
  (ar/csetq company-backends '()))
