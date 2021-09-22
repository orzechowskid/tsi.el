(require 'tsi-engine)

(defcustom tsi-typescript-indent-offset 2
  "Default indent level."
  :type 'number)

(defvar tsi-typescript--always-indent
  '(jsx_attribute)
  "Node types for which indentation always applies.")

(defvar tsi-typescript--always-indent-children
  '(formal_parameters
    if_statement
    ;; jsx_element is a parent of jsx_closing_element, which we do not want to indent
    ;; jsx_element
    object
    object_pattern
    object_type
    parenthesized_expression
    statement_block)
  "Node types for which indentation of children always applies.")

(defun tsi-typescript--node-start-line (node)
  "Returns the number of the line containing the first byte of NODE."
  (if node
      (line-number-at-pos (tsc-node-start-position node))
    1))

(defun tsi-typescript--indent-line ()
  "Calculate indentation for the current line."
  (tsi-engine-walk
   (lambda (node parent is-empty-line)
     (let ((node-type
            (if node (tsc-node-type node) nil))
           (parent-type
            (if parent (tsc-node-type parent) nil)))
       (message "node-type %s parent-type %s is-empty-line %s" node-type parent-type is-empty-line)
       (when is-empty-line
         (message "indenting empty line"))
       (cond
        ;; if this line is empty and the parent's children are always indented
        ((and
          is-empty-line
          (member parent-type tsi-typescript--always-indent-children))
         tsi-typescript-indent-offset)
        ;; if this node and the parent node start on the same line
        ((eq
          (tsi-typescript--node-start-line node)
          (tsi-typescript--node-start-line parent))
         (message
          "no change; parent and child both begin on line %d"
          (tsi-typescript--node-start-line node))
         nil)
        ;; if node is a member of --always-indent
        ((member node-type tsi-typescript--always-indent)
         tsi-typescript-indent-offset)
        ;; if parent node is a member of --always-indent-children
        ((member parent-type tsi-typescript--always-indent-children)
         tsi-typescript-indent-offset)
        ;; type FooBar = "foo" | "bar" in Typescript resolves to the following AST:
        ;; (type_alias_declaration (union_type (union_type (...))))
        ;; , i.e., the tokens are parent/child instead of siblings
        ((eq
          node-type
          'union_type)
         
        ;; handle jsx_element
        ((eq
          parent-type
          'jsx_element)
         (if (eq
              node-type
              'jsx_closing_element)
             nil
           tsi-typescript-indent-offset))
        ;; fallback: do nothing
        (t nil))))))

(defun tsi-typescript--outdent-line ()
  "Outdents by `tsi-typescript-indent-offset`."
  (interactive)
  (let* ((current-indentation
          (progn
            (back-to-indentation)
            (current-column)))
         (new-indentation
          (max
           0
           (- current-indentation tsi-typescript-indent-offset))))
    (delete-region
     (progn (beginning-of-line) (point))
     (progn (back-to-indentation) (point)))
    (indent-to-column new-indentation)
    (back-to-indentation)))

;;;###autoload
(define-minor-mode tsi-typescript-mode
  "Use tree-sitter to calculate indentation for Typescript buffers."
  nil nil
  (make-sparse-keymap)
  (cond
   (tsi-typescript-mode
    ;; enabling mode
    ;; ensure tree-sitter is loaded
    (unless tree-sitter-mode
      (tree-sitter-mode))
    ;; update indent-line-function
    (setq-local
     indent-line-function
     #'tsi-typescript--indent-line)
    ;; add an outdent function
    (define-key tsi-typescript-mode-map (kbd "<S-iso-lefttab>") #'tsi-typescript--outdent-line)
    t)
   (t
    ;; disabling mode
    (setq-local
     indent-line-function
     (default-value 'indent-line-function)))))

(provide 'tsi-typescript)
;;; tsi-typescript.el ends here
