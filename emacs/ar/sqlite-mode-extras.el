;;; sqlite-mode-extras.el --- Extensions for sqlite-mode. -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; Version: 0.7

;;; Commentary:
;; Helper additions for `sqlite-mode'.


;; Suggested key bindings:
;;
;; (use-package sqlite-mode-extras
;;   :bind (:map
;;          sqlite-mode-map
;;          ("n" . next-line)
;;          ("p" . previous-line)
;;          ("b" . sqlite-mode-extras-backtab-dwim)
;;          ("f" . sqlite-mode-extras-tab-dwim)
;;          ("+" . sqlite-mode-extras-add-row)
;;          ("D" . sqlite-mode-extras-delete-row-dwim)
;;          ("C" . sqlite-mode-extras-compose-and-execute)
;;          ("E" . sqlite-mode-extras-execute)
;;          ("S" . sqlite-mode-extras-execute-and-display-select-query)
;;          ("DEL" . sqlite-mode-extras-delete-row-dwim)
;;          ("g" . sqlite-mode-extras-refresh)
;;          ("<backtab>" . sqlite-mode-extras-backtab-dwim)
;;          ("<tab>" . sqlite-mode-extras-tab-dwim)
;;          ("RET" . sqlite-mode-extras-ret-dwim)))


;;; Code:

(require 'sqlite-mode)

(defun sqlite-mode-extras-execute ()
  "Execute a query."
  (interactive)
  (let* ((query (read-string "Execute query: ")))
    (if (sqlite-mode-extras--selected-table-name-in-query query)
        (sqlite-mode-extras-execute-and-display-select-query query)
      (mapc (lambda (query)
              (setq query (string-trim query))
              (unless (string-empty-p query)
                (sqlite-execute sqlite--db query)))
            (string-split query ";"))
      (sqlite-mode-extras-refresh))))

(defun sqlite-mode-extras-compose-and-execute ()
  "Compose and execute a query."
  (interactive)
  (let* ((db-buffer (current-buffer))
         (buffer-name "*SQLite compose query*")
         (buffer (get-buffer-create buffer-name))
         (instructions (concat "Type "
                               (propertize "C-c C-c" 'face 'help-key-binding)
                               " to execute query. "
                               (propertize "C-c C-k" 'face 'help-key-binding)
                               " to cancel and exit. "))
         (query))
    (add-to-list 'display-buffer-alist
                 (cons buffer
                       '((display-buffer-below-selected)
                         (split-window-sensibly))))
    (with-current-buffer buffer
      (visual-line-mode +1)
      (when view-mode
        (view-mode -1)
        (erase-buffer))
      (local-set-key (kbd "C-c C-k")
                     (lambda () (interactive)
                       (quit-window t (get-buffer-window buffer))
                       (message "exit")))
      (local-set-key (kbd "C-c C-c")
                     (lambda ()
                       (interactive)
                       (when (string-empty-p
                              (string-trim
                               (buffer-substring-no-properties
                                (point-min) (point-max))))
                         (erase-buffer)
                         (user-error "Nothing to execute"))
                       (if view-mode
                           (progn
                             (view-mode -1)
                             (erase-buffer)
                             (message instructions))
                         (setq query
                               (string-trim
                                (buffer-substring-no-properties
                                 (point-min) (point-max))))
                         (view-mode +1)
                         (setq view-exit-action 'kill-buffer)
                         (with-current-buffer db-buffer
                           (if (sqlite-mode-extras--selected-table-name-in-query query)
                               (sqlite-mode-extras-execute-and-display-select-query query)
                             (mapc (lambda (query)
                                     (setq query (string-trim query))
                                     (unless (string-empty-p query)
                                       (sqlite-execute sqlite--db query)))
                                   (string-split query ";"))
                             (sqlite-mode-extras-refresh)))
                         (quit-window t (get-buffer-window buffer)))))
      (message instructions))
    (pop-to-buffer buffer-name)))

(defun sqlite-mode-extras-edit-row-field ()
  "Edit current row's field."
  (interactive)
  (when-let* ((table (sqlite-mode-extras--type-property-at-point))
              (row (sqlite-mode-extras--row-property-at-point))
              (columns (sqlite-mode-extras--table-header-column-details
                        (sqlite-mode-extras--table-header-line)))
              (column (sqlite-mode-extras--resolve-table-column))
              (value (if (numberp (sqlite-mode-extras--row-field-value-at-point))
                         (read-number (format "%s: " column)
                                      (sqlite-mode-extras--row-field-value-at-point))
                       (read-string (format "%s: " column)
                                    (sqlite-mode-extras--row-field-value-at-point)))))
    (unless (string-equal (car (seq-first columns)) "id")
      (error "First row must be 'id'"))
    (sqlite-execute
     sqlite--db
     (format "UPDATE %s SET %s = ? WHERE rowid = ?"
             (cdr table)
             column)
     (list value (car row)))
    (sqlite-mode-extras-refresh)))

(defun sqlite-mode-extras-delete-row-dwim ()
  "Delete current row or rows in region."
  (interactive)
  (cond ((sqlite-mode-extras--on-select-query-p)
         (sqlite-mode-extras--toggle-query-results-display t))
        ((sqlite-mode-extras--on-row-p)
         (when-let* ((table (sqlite-mode-extras--type-property-at-point))
                     (pos (point))
                     (rows (if (region-active-p)
                               (let* ((start (region-beginning))
                                      (end (region-end))
                                      (rows))
                                 (setq pos (min start end))
                                 (save-excursion
                                   (goto-char start)
                                   (while (and (< (point) end) (not (eobp)))
                                     (when-let ((row (sqlite-mode-extras--row-property-at-point)))
                                       (setq rows (cons row rows)))
                                     (forward-line 1)))
                                 rows)
                             (list (sqlite-mode-extras--row-property-at-point))))
                     (rowids (mapconcat (lambda (item) (number-to-string (car item))) rows ", "))
                     (columns (sqlite-mode-extras--table-header-column-details
                               (sqlite-mode-extras--table-header-line))))
           (unless (string-equal (car (seq-first columns)) "id")
             (error "First row must be 'id'"))
           (unless (yes-or-no-p (format "Delete from '%s' rowid = %s?" (cdr table) rowids))
             (user-error "Aborted"))
           (sqlite-execute
            sqlite--db
            (format "DELETE FROM  %s WHERE rowid IN (%s);" (cdr table) rowids))
           (sqlite-mode-extras-refresh)))))

(defun sqlite-mode-extras--point-at-last-column-p ()
  "Return t if point is on a table's last column."
  (when-let ((last-column (car (car (last (sqlite-mode-extras--table-header-column-details
                                           (sqlite-mode-extras--table-header-line)))))))
    (equal (sqlite-mode-extras--resolve-table-column) last-column)))

(defun sqlite-mode-extras-add-row ()
  "Add a row to current table."
  (interactive)
  (let* ((type (sqlite-mode-extras--type-property-at-point))
         (row (sqlite-mode-extras--row-property-at-point))
         (table-name (cond ((and (consp type)
                                 (eq (car type) 'row))
                            (cdr type))
                           ((eq type 'table)
                            (car row))))
         (last-column (car (car (last (sqlite-mode-extras--table-header-column-details
                                       (sqlite-mode-extras--table-header-line)))))))
    (unless table-name
      (user-error "No table at point"))
    (sqlite-execute
     sqlite--db
     (format "INSERT INTO %s DEFAULT VALUES;" table-name))
    (sqlite-mode-extras-refresh)
    (sqlite-mode-extras--end-of-table)
    (beginning-of-line)
    ;; Move to first non-id column.
    (while (and (not (sqlite-mode-extras--point-at-last-column-p))
                (not (equal (sqlite-mode-extras--resolve-table-column)
                            last-column))
                (equal (sqlite-mode-extras--resolve-table-column) "id"))
      (sqlite-mode-extras-next-column))
    (sqlite-mode-extras-edit-row-field)))

(defun sqlite-mode-extras--type-property-at-point ()
  "Get `sqlite--type property' at point."
  (save-excursion
    (if (and (eolp) (not (bolp)))
        (get-text-property (1- (point)) 'sqlite--type)
      (when (sqlite-mode-extras--on-table-header-p)
        (forward-line -1))
      (get-text-property (point) 'sqlite--type))))

(defun sqlite-mode-extras--row-property-at-point ()
  "Get `sqlite--row property' at point."
  (if (and (eolp) (not (bolp)))
      (get-text-property (1- (point)) 'sqlite--row)
    (get-text-property (point) 'sqlite--row)))

(defun sqlite-mode-extras-ret-dwim ()
  "DWIM binding for RET.

If on table toggle expansion.  If on row, edit it."
  (interactive)
  (if (and (eq (sqlite-mode-extras--type-property-at-point) 'table)
           (sqlite-mode-extras--row-property-at-point))
      (sqlite-mode-list-data)
    (sqlite-mode-extras-edit-row-field)))

(defun sqlite-mode-extras-tab-dwim (&optional backward)
  "DWIM binding for TAB.

If on table toggle expansion.  If on row, navigate to next field.

When BACKWARD is set, navigate to previous field."
  (interactive)
  (cond ((and (eq (sqlite-mode-extras--type-property-at-point) 'table)
              (sqlite-mode-extras--row-property-at-point))
         (sqlite-mode-list-data))
        ((sqlite-mode-extras--on-select-query-p)
         (sqlite-mode-extras--toggle-query-results-display))
        (t
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
                                  (let ((start (car (nth 1 column))))
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
              (row (sqlite-mode-extras--row-property-at-point)))
    (seq-elt row column-pos)))

(defun sqlite-mode-extras--resolve-table-column ()
  "Resolve point to relevant table column."
  (let* ((columns (sqlite-mode-extras--table-header-column-details
                   (sqlite-mode-extras--table-header-line)))
         (column (seq-find (lambda (column)
                             (let ((start (car (nth 1 column)))
                                   (end (cdr (nth 1 column))))
                               (and (<= start (current-column))
                                    (<= (current-column) end))))
                           columns)))
    (or (seq-first column)
        ;; If at end of line, assume last column.
        (and (eolp) (not (bolp)) (car (car (last columns)))))))

(defun sqlite-mode-extras--assert-on-row ()
  "Ensure point is on a table row."
  (unless (sqlite-mode-extras--on-row-p)
    (user-error "No row under point")))

(defun sqlite-mode-extras--table-header-column-details (header)
  "Return column details list for HEADER string."
  (let ((leading-space (when (string-match "^ *" header)
                         (match-string 0 header)))
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
  "Go to end of current table."
  (when (eq (sqlite-mode-extras--type-property-at-point) 'table)
    (unless (sqlite-mode-extras--table-expanded-p)
      (user-error "Table must be expanded"))
    (forward-line 2))
  (while (and (sqlite-mode-extras--on-row-p)
              (not (eobp)))
    (forward-line))
  (forward-line -1))

(defmacro sqlite-mode-extras--save-excursion (&rest body)
  "Like `save-excursion' executing BODY, but line column based."
  (declare (indent 0) (debug t))
  `(let ((current-line (if (region-active-p)
                           (min (line-number-at-pos (region-beginning))
                                (line-number-at-pos (region-end)))
                         (line-number-at-pos)))
         (current-line-column (current-column)))
     (unwind-protect
         (save-excursion ,@body)
       (forward-line (- current-line (line-number-at-pos)))
       (move-to-column current-line-column))))

(defun sqlite-mode-extras-refresh ()
  "Refresh all listings and table queries."
  (interactive)
  (let ((expanded-tables (sqlite-mode-extras--expanded-tables))
        (select-queries (sqlite-mode-extras--get-select-lines)))
    (sqlite-mode-extras--save-excursion
      (sqlite-mode-list-tables)
      (goto-char (point-min))
      (while (not (eobp))
        (when-let ((table (sqlite-mode-extras--table-name))
                   (_ (seq-contains-p expanded-tables table)))
          (sqlite-mode-list-data))
        (forward-line))
      (mapc (lambda (query)
              (sqlite-mode-extras-execute-and-display-select-query query))
            select-queries))))

(defun sqlite-mode-extras--expanded-tables ()
  "Collect all the tables names that are expanded."
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
  "Return table name at point."
  (when (eq (sqlite-mode-extras--type-property-at-point) 'table)
    (car (sqlite-mode-extras--row-property-at-point))))

(defun sqlite-mode-extras--table-expanded-p ()
  "Return t if table at point is expanded."
  (save-excursion
    (forward-line)
    (sqlite-mode-extras--on-table-header-p)))

(defun sqlite-mode-extras--on-table-p ()
  "Return t if point is on table."
  (eq (sqlite-mode-extras--type-property-at-point) 'table))

(defun sqlite-mode-extras--toggle-query-results-display (&optional remove)
  "Toggle query results display.

Set REMOVE to remove query and results."
  (unless (sqlite-mode-extras--on-select-query-p)
    (error "Not on a select query"))
  (let ((query)
        (inhibit-read-only t))
    (save-excursion
      (forward-line 2)
      (if (looking-at " ")
          ;; Delete results + newline.
          (progn
            (delete-region (point) (if (re-search-forward "^[^ ]" nil t)
                                       (match-beginning 0)
                                     (point-max)))
            (forward-line -1)
            (delete-line)
            (when remove
              (forward-line -1)
              (delete-line)))
        (setq query (thing-at-point 'line t))
        (forward-line -1)
        (delete-line)
        (unless remove
          (sqlite-mode-extras-execute-and-display-select-query query t))))))

(defun sqlite-mode-extras--on-select-query-p ()
  "Return t if on SELECT statement."
  (save-excursion
    (beginning-of-line)
    (looking-at (rx bol (or "SELECT" "select") (1+ space)))))

(defun sqlite-mode-extras--on-row-p ()
  "Look for line above with \='header-line\= face."
  (when (consp (sqlite-mode-extras--type-property-at-point))
    (eq (car (sqlite-mode-extras--type-property-at-point)) 'row)))


(defun sqlite-mode-extras--on-table-header-p ()
  "Return t if on table header."
  (eq 'header-line
      (get-text-property 0 'face
                         (replace-regexp-in-string
                          "\\s-" "" (thing-at-point 'line)))))

(defun sqlite-mode-extras--table-header-line ()
  "Look for line above with \='header-line\= face."
  (save-excursion
    (goto-char (sqlite-mode-extras--table-header-pos))
    (thing-at-point 'line t)))

(defun sqlite-mode-extras--table-header-pos ()
  "Look for line above with \='header-line\= face."
  (let ((pos))
    (if (and (eq (sqlite-mode-extras--type-property-at-point) 'table)
             (sqlite-mode-extras--table-expanded-p))
        (save-excursion
          (forward-line)
          (setq pos (line-beginning-position)))
        (save-excursion
          (while (and (not pos) (not (bobp)))
            (when (and (thing-at-point 'line)
                       (eq 'header-line
                           (get-text-property 0 'face
                                              (replace-regexp-in-string
                                               "\\s-" "" (thing-at-point 'line)))))
              (setq pos (line-beginning-position)))
            (forward-line -1))))
    (unless pos
      (user-error "No table header found"))
    pos))

(defun sqlite-mode-extras--selected-table-name-in-query (query)
  "Extract table name from sqlite SELECT QUERY."
  (when (string-match-p (rx bol (or "SELECT" "select") (1+ space)) (downcase query))
    (let* ((words (split-string query))
           (from-index (cl-position "from" words :test #'string= :key #'downcase)))
      (when from-index
        (when-let ((table-name (nth (1+ from-index) words)))
          (replace-regexp-in-string "[^a-zA-Z0-9_]" "" table-name))))))

(defun sqlite-mode-extras-execute-and-display-select-query (&optional query insert-at-point)
  "Execute a SELECT QUERY.

Set INSERT-AT-POINT to insert all results at point (instead of (point-max))"
  (interactive)
  (let* ((query (or query (read-string "Query: " "SELECT * from ")))
         (table (sqlite-mode-extras--selected-table-name-in-query query))
         (rowid 0)
         (inhibit-read-only t)
         stmt)
    (unless table
      (user-error "No table name found in SELECT query"))
    (unwind-protect
        (progn
          (setq stmt
                (sqlite-select
                 sqlite--db
                 query
                 nil
                 'set))
          (unless insert-at-point
            (goto-char (point-max))
            (insert "\n"))
          (save-excursion
            (insert (propertize (format "%s\n\n" (string-trim  query)) 'face 'font-lock-doc-face))
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
                                 (list table rowid))))))
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
