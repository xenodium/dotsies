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
    :commands ar/osx-convert-plist-to-xml)

  (use-package musica
    :bind (("C-c m r" . musica-search)
           ("C-c m i" . musica-info)
           ("C-c m n" . musica-play-next)
           ("C-c m p" . musica-play-previous)
           ("C-c m SPC" . musica-play-pause)
           ("C-c m r" . musica-play-next-random)))

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
                                       "BluetoothConnector" (get-text-property 0 'mac device) "--notify"))))

  (defun ar/display-toggle-rotation ()
    (interactive)
    (require 'cl-lib)
    (cl-assert (executable-find "fb-rotate") nil
               "Install fb-rotate from https://github.com/CdLbB/fb-rotate")
    ;; #  Display_ID    Resolution  ____Display_Bounds____  Rotation
    ;; 2  0x2b347692    1440x2560      0     0  1440  2560    270    [main]
    ;; From fb-rotate output, get the `current-rotation' from Column 7, row 1 zero-based.
    (let ((current-rotation (nth 7 (split-string (nth 1 (process-lines "fb-rotate" "-i"))))))
      (call-process-shell-command (format "fb-rotate -d 1 -r %s"
                                          (if (equal current-rotation "270")
                                              "0"
                                            "270"))))))
