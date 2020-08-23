;;; diverted.el --- Identify temporary diversions and automatically
;;; move point back to original location.

;;; Commentary:
;; Automatically come back to a original location prior to diversion.


;;; Code:

(require 'cl-lib)
(require 'seq)

(cl-defstruct diverted-event
  from ;; Initial function (eg. 'mark-defun)
  to ;; Follow-up function (eg. 'indent-for-tab-command)
  breadcrumb)


(defvar diverted-events
  (list
   (make-diverted-event :from 'mark-defun
                        :to 'indent-for-tab-command
                        :breadcrumb (lambda ()
                                      (diverted--pop-to-mark-command 2)))
   (make-diverted-event :from 'mark-whole-buffer
                        :to 'indent-for-tab-command
                        :breadcrumb (lambda ()
                                      (diverted--pop-to-mark-command 2))))
  "Diversion events to look for.")

(defun diverted--resolve (symbol)
  "Resolve SYMBOL to event."
  (seq-find (lambda (event)
              (equal symbol
                     (diverted-event-from event)))
            diverted-events))

(defun diverted--pop-to-mark-command (n)
  "Invoke `pop-to-mark-command' N number of times."
  (dotimes (_ n)
    (pop-to-mark-command)))

(defun diverted--advice-fun (orig-fun &rest r)
  "Get back to location prior to diversion using advice around `diverted-events' (ORIG-FUN and R)."
  (let ((recognized-event (diverted--resolve last-command)))
    (when recognized-event
      (funcall (diverted-event-breadcrumb recognized-event))
      (message "Breadcrumbed prior to `%s'"
               (diverted-event-from recognized-event)))))

(defun diverted-mode-enable ()
  "Enable diverted-mode."
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
  "Disable diverted-mode."
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
  :global t
  (if diverted-mode
      (diverted-mode-enable)
    (diverted-mode-disable)))

(provide 'diverted)

;;; diverted.el ends here
