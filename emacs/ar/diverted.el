;;; diverted.el --- Identify temporary diversions and automatically
;;; move point back to original location.

;;; Commentary:
;; Automatically come back to a original location prior to diversion.


;;; Code:

(require 's)
(require 'dash)

(defvar diverted--mode-enabled nil)

(defstruct diverted-event
  from ;; Initial function (eg. 'mark-defun)
  to ;; Follow-up function (eg. 'indent-for-tab-command)
  breadcrumb)

(defvar diverted-events
  (list
   (make-diverted-event :from 'er/expand-region
                        :to 'indent-for-tab-command
                        :breadcrumb (lambda ()
                                      (pop-to-mark-command)
                                      (pop-to-mark-command)))
   (make-diverted-event :from 'mark-defun
                        :to 'indent-for-tab-command
                        :breadcrumb (lambda ()
                                      (pop-to-mark-command)
                                      (pop-to-mark-command)))
   (make-diverted-event :from 'mark-whole-buffer
                        :to 'indent-for-tab-command
                        :breadcrumb (lambda ()
                                      (pop-to-mark-command)
                                      (pop-to-mark-command))))
  "docstring")

(defun diverted--resolve (sym)
  (-find (lambda (event)
           (equal sym
                  (diverted-event-from event)))
         diverted-events))

(defun diverted--advice-fun (orig-fun &rest r)
  "Get back to location prior to diversion using advice around `diverted-events' (ORIG-FUN and R)."
  (let ((recognized-event (diverted--resolve last-command)))
    (when recognized-event
      (funcall (diverted-event-breadcrumb recognized-event))
      (message "Breadcrumbed prior to `%s'"
               (diverted-event-from recognized-event)))))

(defun diverted-mode ()
  "Toggle diverted-mode."
  (interactive)
  (if diverted--mode-enabled
      (diverted-mode-disable)
    (diverted-mode-enable)))

(defun diverted-mode-enable ()
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
  (setq diverted--mode-enabled t)
  (message "diverted-mode enabled"))

(defun diverted-mode-disable ()
  (interactive)
  (mapc (lambda (event)
          (advice-remove (diverted-event-to event)
                         'diverted--advice-fun)
          (message "Ignoring `%s' after `%s' diversions."
                   (diverted-event-to event)
                   (diverted-event-from event)))
        diverted-events)
  (setq diverted--mode-enabled nil)
  (message "diverted-mode disabled"))

(provide 'diverted)

;;; diverted.el ends here
