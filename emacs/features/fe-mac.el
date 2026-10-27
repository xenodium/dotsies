;;; -*- lexical-binding: t; -*-

(when (memq window-system '(mac ns))
  ;; No icon on window.
  (setq ns-use-proxy-icon nil)

  ;; Make ⌘ meta modifier.
  (setq mac-command-modifier 'meta)
  ;; Don't use ⌥ for any binding.
  (setq mac-option-modifier 'none)

  ;; Use existing frame when opening files.
  (setq ns-pop-up-frames nil)

  ;; Transparent titlebar on macOS.
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))

  (setq delete-by-moving-to-trash t)

  ;; Overrides Emacs's built-in `system-move-file-to-trash'.
  ;; Only used while `trash-directory' is nil.
  (defun system-move-file-to-trash (file)
    "Use Homebrew's trash to move FILE to the system trash via Finder."
    (let ((trash "/opt/homebrew/opt/trash/bin/trash"))
      (unless (file-executable-p trash)
        (error "%s not found. Needs \"brew install trash\"" trash))
      (with-temp-buffer
        (unless (zerop (call-process trash nil t nil
                                     "-F" (expand-file-name file)))
          (error "trash failed: %s" (string-trim (buffer-string)))))))

  (use-package ns-win
    ;; Easily insert # on macOS/UK keyboard.
    ;; https://coffeeandcode.neocities.org/emacs-keyboard-config-on-mac.html
    :bind ("M-3" . ar/macos-insert-hash)
    :config
    (defun ar/macos-insert-hash ()
      (interactive)
      (insert "#")))

  ;; Want menu bar on macOS.
  (use-package menu-bar
    :defer 20
    :config
    (menu-bar-mode 1))

  ;; macOS color picker.
  (use-package color-picker
    :commands color-picker)


  (add-to-list 'load-path "/Users/alvaro/stuff/active/code/ready-player")

  (use-package ready-player
    :validate-custom
    (ready-player-my-media-collection-location "~/Music/Music/Media.localized/Music")
    :config
    (ready-player-macos-use-sf-symbols)
    (ready-player-mode +1))

  (add-to-list 'load-path "/Users/alvaro/stuff/active/code/EmacsMacOSModule")

  (use-package macos
    :commands (macos-reveal-in-finder
               macos-share
               macos-load-module)
    :config
    (macos-load-module))

  (defun ar/ivy-bluetooth-connect ()
    "Connect to paired bluetooth device."
    (interactive)
    (require 'cl-lib)
    (require 'ivy)
    (cl-assert (string-equal system-type "darwin")
               nil "macOS only. Sorry :/")
    (cl-assert (executable-find "BluetoothConnector")
               nil "Install BluetoothConnector from https://github.com/lapfelix/BluetoothConnector")
    (ivy-read "(Dis)connect: "
              (seq-map
               (lambda (item)
                 (let* ((device (split-string item " - "))
                        (mac (nth 0 device))
                        (name (nth 1 device)))
                   (propertize name
                               'mac mac)))
               (seq-filter
                (lambda (line)
                  ;; Keep lines like: af-8c-3b-b1-99-af - Device name
                  (string-match-p "^[0-9a-f]\\{2\\}" line))
                (with-current-buffer (get-buffer-create "*BluetoothConnector*")
                  (erase-buffer)
                  ;; BluetoothConnector exits with 64 if no param is given.
                  ;; Invoke with no params to get a list of devices.
                  (unless (eq 64 (call-process "BluetoothConnector" nil (current-buffer)))
                    (error (buffer-string)))
                  (split-string (buffer-string) "\n"))))
              :require-match t
              :preselect (when (boundp 'ar/misc-bluetooth-connect--history)
                           (nth 0 ar/misc-bluetooth-connect--history))
              :history 'ar/misc-bluetooth-connect--history
              :caller 'ar/toggle-bluetooth-connection
              :action (lambda (device)
                        (start-process "BluetoothConnector"
                                       (get-buffer-create "*BluetoothConnector*")
                                       "BluetoothConnector" (get-text-property 0 'mac device) "--notify")))))
