;;; ar-helm-org.el --- Helm org support.

;;; Commentary:
;; Helm org helpers.


;;; Code:


(require 'ar-org)
(require 'cl)
(require 'helm)
(require 'helm-org)
(require 'org)
(require 'org-cliplink)
(require 'org-element)

(defvar ar/helm-org-source-my-todos
  `((name . "TODOS")
    (candidates . ar/helm-org-todo-candidates)
    (action . ,(helm-make-actions "goto" (lambda (marker)
                                           (org-goto-marker-or-bmk marker)
                                           (org-show-siblings))
                                  "mark DONE" (lambda (marker)
                                                (with-current-buffer (marker-buffer marker)
                                                  (save-excursion
                                                    (goto-char (marker-position marker))
                                                    (ar/org-move-to-current-week-as-done))))))))

(defun ar/helm-org-todos ()
  "Current TODOS."
  (interactive)
  (helm :sources '(ar/helm-org-source-my-todos)))

(defun ar/helm-org-todo-candidates ()
  "Get this week's TODOS helm candidates."
  (ar/org-entry-child-headings
   "~/stuff/active/non-public/daily/daily.org"
   "backlog"))

(provide 'ar-helm-org)

;;; ar-helm-org.el ends here
