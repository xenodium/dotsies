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

(defun sqlite-mode-extras-execute ()
  (interactive)
  (sqlite-execute
   sqlite--db
   (read-string "Execute query: "))
  (sqlite-mode-extras-refresh))

(defun sqlite-mode-extras-edit-row-field ()
  "Edit current row's field."
  (interactive)
  (when-let* ((table (get-text-property (point) 'sqlite--type))
              (row (get-text-property (point) 'sqlite--row))
              (column (sqlite-mode-extras--resolve-table-column))
              (value (if (numberp (sqlite-mode-extras--row-field-value-at-point))
                         (read-number (format "Update '%s': " column)
                                      (sqlite-mode-extras--row-field-value-at-point))
                       (read-string (format "Update '%s': " column)
                                    (sqlite-mode-extras--row-field-value-at-point))))
              (current-line (line-number-at-pos))
              (current-column (current-column)))
    (sqlite-execute
     sqlite--db
     (format "UPDATE %s SET %s = ? WHERE rowid = ?"
             (cdr table)
             column)
     (list value (car row)))
    (save-restriction
      (goto-char (sqlite-mode-extras--table-header-pos))
      (forward-line -1)
      ;; First time collapses table
      (sqlite-mode-list-data)
      ;; Second time expands table (ie. refreshes it).
      (sqlite-mode-list-data))
    (forward-line (- current-line (line-number-at-pos)))
    (move-to-column current-column)))

(defun sqlite-mode-extras-add-row ()
  "Add a row to current table."
  (interactive)
  (let* ((type (get-text-property (point) 'sqlite--type))
         (row (get-text-property (point) 'sqlite--row))
         (table-name (cond ((and (consp type)
                                 (eq (car type) 'row))
                            (cdr type))
                           ((eq type 'table)
                            (car row))))
         (text-before (buffer-string)))
    (unless table-name
      (user-error "No table at point"))
    (sqlite-execute
     sqlite--db
     (format "INSERT INTO %s DEFAULT VALUES;" table-name))
    (sqlite-mode-extras-refresh)
    (sqlite-mode-extras--end-of-table)
    (beginning-of-line)
    (sqlite-mode-extras-next-column)
    (sqlite-mode-extras-next-column)))

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
  (unless (sqlite-mode-extras--on-row-p)
    (user-error "No row under point")))

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

(defun sqlite-mode-extras--end-of-table ()
  (while (and (sqlite-mode-extras--on-row-p)
              (not (eobp)))
    (forward-line))
  (forward-line -1))

(defun sqlite-mode-extras-refresh ()
  (interactive)
  (let ((expanded-tables (sqlite-mode-extras--expanded-tables))
        (current-line (line-number-at-pos))
        (current-column (current-column))
        (select-queries (sqlite-mode-extras--get-select-lines)))
    (save-excursion
      (sqlite-mode-list-tables)
      (goto-char (point-min))
      (while (not (eobp))
        (when-let ((table (sqlite-mode-extras--table-name))
                   (_ (seq-contains-p expanded-tables table)))
          (sqlite-mode-list-data))
        (forward-line))
      (mapc (lambda (query)
              (sqlite-mode-extras-execute-select-query query))
            select-queries))
    (forward-line (- current-line (line-number-at-pos)))
    (move-to-column current-column)))

(defun sqlite-mode-extras--expanded-tables ()
  (save-excursion
    (let ((tables))
      (goto-char (point-min))
      (while (not (eobp))
        (when-let ((table (sqlite-mode-extras--table-name))
                   (_ (sqlite-mode-extras--table-expanded-p)))
          (add-to-list 'tables table))
        (forward-line))
      tables)))

(defun sqlite-mode-extras--table-name ()
  (when (eq (get-text-property (point) 'sqlite--type) 'table)
    (car (get-text-property (point) 'sqlite--row))))

(defun sqlite-mode-extras--table-expanded-p ()
  (save-excursion
    (forward-line)
    (eq 'header-line
        (get-text-property 0 'face
                           (replace-regexp-in-string
                            "\\s-" "" (thing-at-point 'line))))))

(defun sqlite-mode-extras--on-table-p ()
  (eq (get-text-property (point) 'sqlite--type) 'table))

(defun sqlite-mode-extras--on-row-p ()
  "Look for line above with \='header-line\= face."
  (when (consp (get-text-property (point) 'sqlite--type))
    (eq (car (get-text-property (point) 'sqlite--type)) 'row)))

(defun sqlite-mode-extras--table-header-line ()
  "Look for line above with \='header-line\= face."
  (save-excursion
    (goto-char (sqlite-mode-extras--table-header-pos))
    (thing-at-point 'line t)))

(defun sqlite-mode-extras--table-pos ()
  ""
  (save-excursion
    (goto-char (sqlite-mode-extras--table-header-pos))
    (forward-line -1)
    (point)))

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

(defun sqlite-mode-extras--diff-position (s1 s2)
  (or
   (let ((pos (cl-position-if-not
               'identity
               (cl-mapcar
                'equal
                (append s1 nil) (append s2 nil)))))
     (and pos (1+ pos)))
   (if (/= (length s1) (length s2))
       (1+ (min (length s1) (length s2)))
     nil)))

(defun sqlite-mode-extras--table-name-in-query (query)
  "Extract table name from sqlite SELECT query."
  ;; Ensure it's a SELECT statement
  (unless (string-match-p (rx bol (0+ space) "SELECT" (1+ space)) (downcase query))
    (error "Provided Query is not a SELECT statement."))
  (let* ((words (split-string query))
         (from-index (cl-position "from" words :test #'string= :key #'downcase)))
    (when from-index
      (when-let ((table-name (nth (1+ from-index) words)))
        (replace-regexp-in-string "[^a-zA-Z0-9_]" "" table-name)))))

(defun sqlite-mode-extras-execute-select-query (&optional query)
  (interactive)
  (let* ((query (or query (read-string "Query: " "SELECT * from ")))
         (table (sqlite-mode-extras--table-name-in-query query))
         (rowid 0)
         (inhibit-read-only t)
         stmt)
    (unwind-protect
        (progn
          (setq stmt
                (sqlite-select
                 sqlite--db
                 query
                 nil
                 'set))
          (goto-char (point-max))
          (insert (propertize (format "\n%s\n\n" (string-trim  query)) 'face 'font-lock-doc-face))
          (sqlite-mode--tablify (sqlite-columns stmt)
                                (cl-loop for i from 0 upto 1000
                                         for row = (sqlite-next stmt)
                                         while row
                                         do (setq rowid (car row))
                                         collect row)
                                (cons 'row table)
                                "  ")
          (when (sqlite-more-p stmt)
            (insert (buttonize "  More data...\n" #'sqlite-mode--more-data
                               (list table rowid)))))
      (when stmt
        (sqlite-finalize stmt)))))

(defun sqlite-mode-extras--get-select-lines ()
  "Get all lines starting with SELECT or select from the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((regexp (rx bol (0+ space) (or "SELECT" "select") (one-or-more any)))
          (matches))
      (while (re-search-forward regexp nil t)
        (add-to-list 'matches (string-trim (match-string 0))))
      (reverse matches))))

(provide 'sqlite-mode-extras)
;;; sqlite-mode-extras.el ends here
