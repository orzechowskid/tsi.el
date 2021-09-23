;;; tsi.el --- tree-sitter indentation -*- lexical-binding: t; -*-
;;; Summary:
;;; Commentary:
;;; Code:

(require 'tsi)

(defcustom tsi-typescript-indent-offset 2
  "Default indent level."
  :type 'number)

(defvar tsi-typescript--always-indent
  '(jsx_attribute
    literal_type)
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

(defun tsi-typescript--get-indent-info (node parent is-empty-line)
  "Calculate relative indentation for the current NODE and its PARENT."
  (let ((node-type
         (if node (tsc-node-type node) nil))
        (parent-type
         (if parent (tsc-node-type parent) nil)))
    (when is-empty-line
      (message "indenting empty line"))
    ;; TODO: (if is-empty-line (cond ...) (cond...)) maybe?
    ;; TODO: when is-empty-line is t, examine the last token on the previous line
    ;; TODO: handle ERROR nodes
    (cond
     ;; if this line is empty and the parent's children are always indented
     ((and
       is-empty-line
       (member parent-type tsi-typescript--always-indent-children))
      (message "+ is empty line and parent's children are always indented")
      tsi-typescript-indent-offset)
     ;; if node is a member of --always-indent
     ((member node-type tsi-typescript--always-indent)
      (message "+ this node is always indented")
      tsi-typescript-indent-offset)
     ;; if parent node is a member of --always-indent-children
     ((member parent-type tsi-typescript--always-indent-children)
      (message "+ parent's children are always indented")
      tsi-typescript-indent-offset)
     ;; handle union_type
     ((eq
       parent-type
       'union_type)
      nil)
     ;; ((eq
     ;;   node-type
     ;;   'union_type)
     ;;  (if (eq
     ;;       parent-type
     ;;       'union_type)
          
     ;;      ;; debug
     ;;      (progn (message "using parent's indentation for this union_type")
     ;;             (message "next node will be %s ; next parent will be %s" (tsc-node-type parent) (tsc-node-type (tsc-get-parent parent)))
     ;;             (tsi-typescript--get-indent-info parent (tsc-get-parent parent) is-empty-line))
     ;;    tsi-typescript-indent-offset))
     ;; indent all children of jsx_element except for jsx_closing_element
     ((and
       (eq
        parent-type
        'jsx_element)
       (not (eq
             node-type
             'jsx_closing_element)))
      (mesage "+ parent is jsx_element and child is not jsx_closing_element")
      tsi-typescript-indent-offset)
     ;; if this node and the parent node start on the same line
     ((eq
       (tsi-typescript--node-start-line node)
       (tsi-typescript--node-start-line parent))
      (message
       "no change; parent and child both begin on line %d"
       (tsi-typescript--node-start-line node))
      nil)
     ;; fallback: do nothing
     ;; debug
     (t (progn (message "n/a") nil)))))

;; exposed for testing purposes
;;;###autoload
(defun tsi-typescript--indent-line ()
  "Internal function.  Calculate indentation for the current line."
  (tsi-walk #'tsi-typescript--get-indent-info))

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
