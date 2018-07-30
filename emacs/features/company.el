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
    (vsetq company-dabbrev-downcase nil)
    (vsetq company-dabbrev-ignore-case nil))

  (use-package company-dabbrev-code
    :config
    (vsetq company-dabbrev-code-ignore-case nil))

  (vsetq company-idle-delay 0.2)
  (vsetq company-show-numbers t)
  (vsetq company-minimum-prefix-length 2)
  (vsetq company-tooltip-align-annotations t)

  ;; Disable all company backends by default.
  (csetq company-backends '()))
