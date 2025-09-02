;;; diverted.el --- Identify temporary diversions and automatically -*- lexical-binding: t; -*-
;;; move point back to original location.

;; Copyright (C) 2025 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; Package-Requires: ((emacs "28.1"))
;; URL: https://github.com/xenodium/diverted

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Automatically come back to a original location prior to diversion.

;; Setup:
;;
;;  1. Load package
;;
;;    (require 'diverted)
;;
;;  2. Identify diversions and provide a breadcrumb to get back.
;;
;;    (add-to-list 'diverted-events
;;      (make-diverted-event :from 'er/expand-region
;;                                 :to 'indent-for-tab-command
;;                                 :breadcrumb (lambda ()
;;                                                  (diverted--pop-to-mark-command 2))))
;;
;;  3. Enable `diverted-mode'
;;
;;    (diverted-mode +1)

;;; Code:

(require 'cl-lib)
(require 'seq)

(cl-defstruct diverted-event
  from ;; Initial function (eg. 'mark-defun)
  to ;; Follow-up function (eg. 'indent-for-tab-command)
  breadcrumb)

(defgroup diverted nil
  "Detect temporary diversions and restore point location."
  :group 'convenience)

(defcustom diverted-events
  (list
   (make-diverted-event :from 'mark-defun
                        :to 'indent-for-tab-command
                        :breadcrumb (lambda ()
                                      (diverted--pop-to-mark-command 2)))
   (make-diverted-event :from 'mark-whole-buffer
                        :to 'indent-for-tab-command
                        :breadcrumb (lambda ()
                                      (diverted--pop-to-mark-command 2))))
  "Diversion events to look for.

For example:

  (add-to-list \\='diverted-events
    (make-diverted-event
      :from \\='er/expand-region
      :to \\='indent-for-tab-command
      :breadcrumb (lambda ()
                    (diverted--pop-to-mark-command 2))))"
  :type '(repeat sexp)
  :group 'diverted)

(defun diverted--resolve (symbol)
  "Resolve SYMBOL to event."
  (seq-find (lambda (event)
              (equal symbol (diverted-event-from event)))
            diverted-events))

(defun diverted--pop-to-mark-command (n)
  "Invoke `pop-to-mark-command' N number of times."
  (dotimes (_ n)
    (pop-to-mark-command)))

(defun diverted--advice-fun (_orig-fun &rest _r)
  "Get back to location prior to diversion.

Use advice around `diverted-events' (ORIG-FUN and R)."
  (let ((recognized-event (diverted--resolve last-command)))
    (when recognized-event
      (funcall (diverted-event-breadcrumb recognized-event))
      (message "Breadcrumbed prior to `%s'"
               (diverted-event-from recognized-event)))))

(defun diverted-mode-enable ()
  "Enable `diverted-mode'."
  (interactive)
  (diverted-mode-disable)
  (mapc (lambda (event)
          (advice-add (diverted-event-to event)
                      :after
                      'diverted--advice-fun)
          (message "Looking for `%s' after `%s' diversions."
                   (diverted-event-to event)
                   (diverted-event-from event)))
        diverted-events)
  (message "diverted-mode enabled"))

(defun diverted-mode-disable ()
  "Disable `diverted-mode'."
  (interactive)
  (mapc (lambda (event)
          (advice-remove (diverted-event-to event)
                         'diverted--advice-fun))
        diverted-events)
  (message "diverted-mode disabled"))

(define-minor-mode diverted-mode
  "Detect temporary diversions and restore point location."
  :init-value nil
  :lighter " diverted"
  :group 'diverted
  :global t
  (if diverted-mode
      (diverted-mode-enable)
    (diverted-mode-disable)))

(provide 'diverted)

;;; diverted.el ends here
