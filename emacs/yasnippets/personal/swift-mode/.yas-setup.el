;;; -*- lexical-binding: t; -*-

;; Swift helpers adjusted from:
;; https://github.com/jorgenschaefer/elpy/blob/060a4eb78ec8eba9c8fe3466c40a414d84b3dc81/snippets/python-mode/.yas-setup.el

(defun swift-snippet-init-assignments (arg-string)
  "Return init assignments for arguments."
  (let ((indentation (make-string (save-excursion
                                    (goto-char start-point)
                                    (current-indentation))
                                  ?\s)))
    (string-trim (mapconcat (lambda (arg)
                              (if (string-match "^\\*" arg)
                                  ""
                                (format "self.%s = %s\n%s"
                                        arg arg indentation)))
                            (swift-snippet-split-args arg-string)
                            ""))))

(defun swift-snippet-split-args (arg-string)
  "Split a python argument string into ((name, default)..) tuples"
  (mapcar (lambda (x)
            (if (and x (string-match "\\([[:alnum:]]*\\):" x))
                (match-string-no-properties 1 x)
              x))
          (split-string arg-string "[[:blank:]]*,[[:blank:]]*" t)))

(defun swift-class-or-struct-vars-at-point ()
  "Return a list of class or struct vars in the form '(((name . \"foo\") (type . \"Foo\")))."
  (cl-assert (seq-contains local-minor-modes 'tree-sitter-mode) "tree-sitter-mode not enabled")
  (let* ((node (or (tree-sitter-node-at-point 'struct_declaration)
                   (tree-sitter-node-at-point 'class_declaration)))
         (vars)
         (var))
    (unless node
      (error "Neither in class nor struct"))
    (mapc
     (lambda (item)
       (cond ((eq 'identifier
                  (tsc-node-type (cdr item)))
              (when var
                (setq vars (append vars (list var))))
              (setq var (list (cons 'name (tsc-node-text
                                           (cdr item))))))
             ((eq 'type
                  (tsc-node-type (cdr item)))
              (setq var (map-insert var 'type (tsc-node-text
                                               (cdr item)))))
             ((eq 'string
                  (tsc-node-type (cdr item)))
              (setq var (map-insert var 'type "String")))
             ((eq 'number
                  (tsc-node-type (cdr item)))
              (setq var (map-insert var 'type "Int")))
             (t (message "%s" (tsc-node-type (cdr item))))))
     (tsc-query-captures
      (tsc-make-query tree-sitter-language
                      "(struct_declaration (variable_declaration (identifier) @name (type) @type))
                       (struct_declaration (variable_declaration (identifier) @name (string) @value))
                       (struct_declaration (variable_declaration (identifier) @name (number) @value))
                       (struct_declaration (constant_declaration (identifier) @name (type) @value))
                       (struct_declaration (constant_declaration (identifier) @name (string) @value))
                       (struct_declaration (constant_declaration (identifier) @name (number) @value))
                       (class_declaration (variable_declaration (identifier) @name (type) @type))
                       (class_declaration (variable_declaration (identifier) @name (string) @value))
                       (class_declaration (variable_declaration (identifier) @name (number) @value))
                       (class_declaration (constant_declaration (identifier) @name (type) @type))
                       (class_declaration (constant_declaration (identifier) @name (string) @value))
                       (class_declaration (constant_declaration (identifier) @name (number) @value))")
      node nil))
    (when var
      (setq vars (append vars (list var))))
    vars))

(defun swift-class-or-struct-initializer-text (vars)
  (cl-assert (seq-contains local-minor-modes 'tree-sitter-mode) "tree-sitter-mode not enabled")
  (format
   (string-trim
    "
init(%s) {
  %s
}")
   (seq-reduce (lambda (reduced var)
                 (format "%s%s%s: %s"
                         reduced
                         (if (string-empty-p reduced)
                             "" ", ")
                         (map-elt var 'name)
                         (map-elt var 'type)))
               vars "")
   (string-join
    (mapcar (lambda (var)
              (format "self.%s = %s"
                      (map-elt var 'name)
                      (map-elt var 'name)))
            vars)
    "\n  ")))
