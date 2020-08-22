;;; -*- lexical-binding: t; -*-

(when (memq window-system '(mac ns))
  ;; No icon on window.
  (setq ns-use-proxy-icon nil)

  ;; Fixes mode line separator issues on macOS.
  (setq ns-use-srgb-colorspace nil)

  ;; Make ⌘ meta modifier.
  (setq mac-command-modifier 'meta)

  ;; Transparent titlebar on macOS.
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))

  (use-package ns-win
    ;; Easily insert # on macOS/UK keyboard.
    ;; https://coffeeandcode.neocities.org/emacs-keyboard-config-on-mac.html
    :bind ("M-3" . ar/macos-insert-hash)
    :config
    (defun ar/macos-insert-hash ()
      (interactive)
      (insert "#"))

    (defun ar/macos-hardware-overview ()
      "View macOS hardware overview."
      (interactive)
      (shell-command "system_profiler SPHardwareDataType")))

  ;; Want menu bar on macOS.
  (use-package menu-bar
    :defer 20
    :config
    (menu-bar-mode 1))

  ;; macOS color picker.
  (use-package color-picker
    :commands color-picker)

  ;; Convert binary plists to xml using host utilities.
  (use-package ar-osx
    :commands ar/osx-convert-plist-to-xml))
