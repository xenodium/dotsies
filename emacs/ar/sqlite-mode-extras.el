;;; sqlite-mode-extras.el --- Extensions for sqlite-mode

;; Copyright (C) 2023 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; Version: 0.1

;;; Commentary:
;; Helper additions `sqlite-mode'.


;; Suggested setup:
;;
;; (use-package sqlite-mode-extras
;;   :bind (:map
;;          sqlite-mode-map
;;          ("n" . next-line)
;;          ("p" . previous-line)
;;          ("b" . sqlite-mode-extras-backtab-dwim)
;;          ("f" . sqlite-mode-extras-tab-dwim)
;;          ("<backtab>" . sqlite-mode-extras-backtab-dwim)
;;          ("<tab>" . sqlite-mode-extras-tab-dwim)
;;          ("RET" . sqlite-mode-extras-ret-dwim)))


;;; Code:

(require 'sqlite-mode)

(defun sqlite-mode-extras-edit-row-field ()
  "Edit current row's field."
  (interactive)
  (when-let* ((table (get-text-property (point) 'sqlite--type))
              (row (get-text-property (point) 'sqlite--row))
              (column (sqlite-mode-extras--resolve-table-column))
              (value-at-point (sqlite-mode-extras--row-field-value-at-point))
              (value (if (numberp value-at-point)
                         (read-number (format "Update '%s': " column)
                                      value-at-point)
                       (read-string (format "Update '%s': " column)
                                    value-at-point)))
              (current-line (line-number-at-pos))
              (current-column (current-column)))
    (sqlite-execute
     sqlite--db
     (format "UPDATE %s SET %s = ? where %s"
             (cdr table)
             column
             (string-join
              (mapcar (lambda (column)
                        (format "%s = ?" (car (split-string column " "))))
                      (cons "rowid" (sqlite-mode--column-names (cdr table))))
              " and "))
     (append (list value)
             row))
    (save-restriction
      (goto-char (sqlite-mode-extras--table-header-pos))
      (forward-line -1)
      ;; First time collapses table
      (sqlite-mode-list-data)
      ;; Second time expands table (ie. refreshes it).
      (sqlite-mode-list-data))
    (forward-line (- current-line (line-number-at-pos)))
    (move-to-column current-column)))

(defun sqlite-mode-extras-ret-dwim ()
  "DWIM binding for RET.

If on table toggle expansion.  If on row, edit it."
  (interactive)
  (if (and (eq (get-text-property (point) 'sqlite--type) 'table)
           (get-text-property (point) 'sqlite--row))
      (sqlite-mode-list-data)
    (sqlite-mode-extras-edit-row-field)))

(defun sqlite-mode-extras-tab-dwim (&optional backward)
  "DWIM binding for TAB.

If on table toggle expansion.  If on row, navigate to next field.

When BACKWARD is set, navigate to previous field."
  (interactive)
  (let ((max (point-max)))
    (if (and (eq (get-text-property (point) 'sqlite--type) 'table)
             (get-text-property (point) 'sqlite--row))
        (sqlite-mode-list-data)
      (sqlite-mode-extras-next-column backward))))

(defun sqlite-mode-extras-backtab-dwim ()
  "Like `sqlite-mode-extras-tab-dwim' but backwards."
  (interactive)
  (sqlite-mode-extras-tab-dwim t))

(defun sqlite-mode-extras-next-column (&optional backward)
  "Navigate to next column.

When BACKWARD is set, navigate to previous column."
  (interactive)
  (let* ((columns (sqlite-mode-extras--table-header-column-details
                   (sqlite-mode-extras--table-header-line)))
         (next-column (seq-find (lambda (column)
                                  (let ((start (car (nth 1 column)))
                                        (end (cdr (nth 1 column))))
                                    (> start (current-column))))
                                columns))
         (prev-column-index (if next-column
                                (- (seq-position columns
                                                 next-column) 2)
                              (- (seq-position columns
                                               (car (last columns))) 1))))
    (if backward
        (if (>= prev-column-index 0)
            (goto-char (+ (line-beginning-position)
                          (car (nth 1 (nth prev-column-index columns)))))
          (forward-line -1)
          (goto-char (+ (line-beginning-position)
                        (car (nth 1 (car (last columns)))))))
      (if next-column
          (goto-char (+ (line-beginning-position)
                        (car (nth 1 next-column))))
        (forward-line)))))

(defun sqlite-mode-extras--row-field-value-at-point ()
  "Get current row's field value."
  (when-let* ((columns (sqlite-mode-extras--table-header-column-details
                        (sqlite-mode-extras--table-header-line)))
              (column (seq-find (lambda (column)
                                  (let ((start (car (nth 1 column)))
                                        (end (cdr (nth 1 column))))
                                    (and (<= start (current-column))
                                         (<= (current-column) end))))
                                columns))
              (column-pos (seq-position columns column))
              (row (get-text-property (point) 'sqlite--row)))
    (seq-elt row column-pos)))

(defun sqlite-mode-extras--resolve-table-column ()
  "Resolve point to relevant table column."
  (when-let* ((columns (sqlite-mode-extras--table-header-column-details
                        (sqlite-mode-extras--table-header-line)))
              (column (seq-find (lambda (column)
                                  (let ((start (car (nth 1 column)))
                                        (end (cdr (nth 1 column))))
                                    (and (<= start (current-column))
                                         (<= (current-column) end))))
                                columns)))
    (seq-first column)))

(defun sqlite-mode-extras--assert-on-row ()
  "Ensure point is on a table row."
  (let ((table (get-text-property (point) 'sqlite--type))
        (row (get-text-property (point) 'sqlite--row)))
    (when (or (not (consp table))
              (not (eq (car table) 'row)))
      (user-error "No row under point"))))

(defun sqlite-mode-extras--table-header-column-details (header)
  "Return column details list for HEADER string."
  (sqlite-mode-extras--assert-on-row)
  (let ((leading-space (when (string-match "^ *" header)
                         (match-string 0 header)))
        (len 0)
        (start 0)
        columns)
    (with-temp-buffer
      (insert header)
      (goto-char 0)
      (while (re-search-forward "\\([^ ]+  *\\)" (point-max) t)
        (setq columns (append columns (list (match-string 0)))))
      (when (seq-first columns)
        (setcar columns
                (concat leading-space
                        (seq-first columns))))
      (mapcar (lambda (column)
                (let* ((next-start (+ start (length column)))
                       (item (list (string-trim column)
                                   (cons start (1- next-start)))))
                  (setq start next-start)
                  item))
              columns))))

(defun sqlite-mode-extras--table-header-line ()
  "Look for line above with \='header-line\= face."
  (save-excursion
    (goto-char (sqlite-mode-extras--table-header-pos))
    (thing-at-point 'line t)))

(defun sqlite-mode-extras--table-header-pos ()
  "Look for line above with \='header-line\= face."
  (let ((pos))
    (save-excursion
      (while (and (not pos) (not (bobp)))
        (when (and (thing-at-point 'line)
                   (eq 'header-line
                       (get-text-property 0 'face
                                          (replace-regexp-in-string
                                           "\\s-" "" (thing-at-point 'line)))))
          (setq pos (line-beginning-position)))
        (forward-line -1)))
    (unless pos
      (user-error "No table header found"))
    pos))

(provide 'sqlite-mode-extras)
;;; sqlite-mode-extras.el ends here
