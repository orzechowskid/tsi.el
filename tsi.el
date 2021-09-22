;;; tsi.el

(require 'tree-sitter)

(defun tsi-node-start-line (node)
  "Returns the number of the line containing the first byte of NODE."
  (if node
      (line-number-at-pos (tsc-node-start-position node))
    1))

(defun tsi--get-column (node indent-info-fn)
  "Queries INDENT-INFO-FN to calculate the indentation for the path ending at NODE.

Returns a column number, or nil if no action should be taken."
  (message "< <go> >")
  (let* ((current-node
          node)
         (parent-node
          (if current-node (tsc-get-parent current-node) nil))
         (empty-line
          (save-excursion
            (end-of-line)
            (skip-chars-backward " \t")
            (bolp)))
         (indent-ops
          '()))
    (message "empty line? %s" empty-line)
    ;; indentation may apply when:
    ;; - the line being indented contains no non-whitespace characters
    (when empty-line
      (message "indenting empty (or only-whitespace) line")
      (push
       (funcall indent-info-fn nil current-node t)
       indent-ops))
    (while
        current-node
      (let ((parent-node (tsc-get-parent current-node)))
        ;; debug
        (when parent-node
          (message
           "parent is (%s) %s (line %d), child is (%s) %s (line %d)"
           (if (tsc-node-named-p parent-node) "named" "anonymous")
           (tsc-node-type parent-node)
           (tsi-node-start-line parent-node)
           (if (tsc-node-named-p current-node) "named" "anonymous")
           (tsc-node-type current-node)
           (tsi-node-start-line current-node)))
        ;; indentation may apply when:
        ;; - there exists a parent node to the current node
        (when
            parent-node
          (message "getting indent op")
          (push
           (funcall indent-info-fn current-node parent-node nil)
           indent-ops)
          ;; debug
          (message "result: %s" (car indent-ops)))
        (setq current-node parent-node)))
    (message "ops: %s" indent-ops)
    (seq-reduce
     (lambda (accum elt)
       (cond
        ((numberp elt)
         (+ accum elt))
        (t accum)))
     indent-ops
     0)))

(defun tsi-walk (indent-info)
  "Indents the current line using the tree-sitter AST.

INDENT-INFO is a function which takes three arguments - current-node,  parent-node, and is-empty-line - and returns one of the following indent operations:

- NIL (a no-op); or
- a number (to increase, or decrease, the amount of indentation on this line)"

  ;; credit for some of this comes from https://codeberg.org/FelipeLema/tree-sitter-indent.el
  (let*
      ((original-position
        (point))
       (first-non-blank-pos
        (save-excursion
          (back-to-indentation)
          (point)))
       (should-save-excursion ;; true if point is after any leading whitespace
        (< first-non-blank-pos original-position))
       (node
        (save-excursion
          (back-to-indentation)
          (tree-sitter-node-at-point)))
       (indent-to-column (tsi--get-column node indent-info)))
    (if should-save-excursion
        (save-excursion
          (indent-line-to indent-to-column))
      (indent-line-to indent-to-column))))

(provide 'tsi)
;;; tsi.el ends here
