;;; tsi-typescript.el --- tree-sitter indentation for Javascript/Typescript -*- lexical-binding: t; -*-

;;; Version: 1.5.2

;;; Author: Dan Orzechowski

;;; URL: https://github.com/orzechowskid/tsi.el

;;; Package-Requires: ((tsi "1.0.0"))

;;; SPDX-License-Identifier: GPLv3

;;; Commentary:
;; a file containing indentation rules for Javascript/Typescript, including JSX.  Enable
;; it via `M-x tsi-typescript-mode`, or configure as part of your major-mode hook:
;;
;; (add-hook 'typescript-mode-hook  (lambda () (tsi-typescript-mode t)))
;;
;; Enabling `tsi-typescript-mode` will set the current buffer's `indent-line-function`,
;; as well as register an outdent function (<S-iso-lefttab>, aka <backtab>).  I should
;; make that configurable at some point.
;;
;; Indentation amount is controlled by the value of `tsi-typescript-indent-offset`.

;;; Code:

(require 'tsi)

(defgroup tsi-typescript nil
  "Minor mode for indenting JS[X]/TS[X] using the tree-sitter CST."
  :group 'programming
  :prefix "tsi-typescript-")

(defcustom tsi-typescript-indent-offset 2
  "Default indent level."
  :type 'number
  :group 'tsi-typescript)

(defun tsi-typescript--get-indent-for (current-node parent-node)
  "Returns an indentation operation for the given CURRENT-NODE and PARENT-NODE."
  (let* ((current-type
          (tsc-node-type current-node))
         (parent-type
          (tsc-node-type parent-node))
         (child-indentation
          (cond
           ((or
             (eq
              current-type
              'statement_block)
             (eq
              current-type
              'object_type)
             (eq
              current-type
              'enum_body)
             (eq
              current-type
              'import_clause))
            (if (and
                 (> (line-number-at-pos) (car (tsc-node-start-point current-node)))
                 (< (line-number-at-pos) (car (tsc-node-end-point current-node))))
                tsi-typescript-indent-offset
              nil))

           (t nil)))
         (parent-indentation
          (cond
           ((and (eq
                  parent-type
                  'intersection_type))
            tsi-typescript-indent-offset)
           
           ((and (eq
                  parent-type
                  'arrow_function)
                 (not (eq
                       current-type
                       'statement_block)))
            tsi-typescript-indent-offset)

           ((eq
             parent-type
             'assignment_expression)
            (when (or
                   ;; this list is going to grow kind of big
                   (eq current-type 'template_string)
                   (eq current-type 'number))
              tsi-typescript-indent-offset))

           ((eq
             parent-type
             'conditional_type)
            tsi-typescript-indent-offset)

           ((eq
             parent-type
             'arguments)
            (if (tsc-node-named-p current-node)
                tsi-typescript-indent-offset
              nil))

           ((eq
             parent-type
             'type_parameters)
            (if (tsc-node-named-p current-node)
                tsi-typescript-indent-offset
              nil))

           ((eq
             parent-type
             'array)
            (if (tsc-node-named-p current-node)
                tsi-typescript-indent-offset
              nil))

           ((eq
             parent-type
             'class_body)
            (if (tsc-node-named-p current-node)
                tsi-typescript-indent-offset
              nil))

           ((eq
             parent-type
             'enum_body)
            (if (or (eq current-type 'enum_assignment)
                    (eq current-type 'property_identifier))
                tsi-typescript-indent-offset
              nil))

           ((eq
             parent-type
             'export_clause)
            (if (tsc-node-named-p current-node)
                tsi-typescript-indent-offset
              nil))

           ((eq
             parent-type
             'export_statement)
            (if (tsc-node-named-p current-node)
                tsi-typescript-indent-offset
              nil))

           ((eq
             parent-type
             'formal_parameters)
            (if (tsc-node-named-p current-node)
                tsi-typescript-indent-offset
              nil))

           ((eq
             parent-type
             'if_statement)
            (if (eq
                 current-type
                 'expression_statement)
                tsi-typescript-indent-offset
              nil))

           ((eq
             parent-type
             'jsx_element)
            (if (and
                 (tsc-node-named-p current-node)
                 (not (eq
                       current-type
                       'jsx_closing_element)))
                tsi-typescript-indent-offset
              nil))

           ((eq
             parent-type
             'jsx_fragment)
            (if (tsc-node-named-p current-node)
                tsi-typescript-indent-offset
              nil))

           ((eq
             parent-type
             'jsx_opening_element)
            (if (and
                 (tsc-node-named-p current-node)
                 (not (eq
                       current-type
                       'jsx_closing_element)))
                tsi-typescript-indent-offset
              nil))

           ((eq
             parent-type
             'jsx_self_closing_element)
            (if (tsc-node-named-p current-node)
                tsi-typescript-indent-offset
              nil))

           ((eq
             parent-type
             'lexical_declaration)
            (if (tsc-node-named-p current-node)
                tsi-typescript-indent-offset
              nil))

           ((eq
             parent-type
             'member_expression)
            (if (equal
                 (tsc-node-text current-node)
                 ".")
                tsi-typescript-indent-offset
              nil))

           ((eq
             parent-type
             'named_imports)
            (if (tsc-node-named-p current-node)
                tsi-typescript-indent-offset
              nil))

           ((eq
             parent-type
             'object)
            (if (tsc-node-named-p current-node)
                tsi-typescript-indent-offset
              nil))

           ((eq
             parent-type
             'object_pattern)
            (if (tsc-node-named-p current-node)
                tsi-typescript-indent-offset
              nil))

           ((eq
             parent-type
             'object_type)
            (if (tsc-node-named-p current-node)
                tsi-typescript-indent-offset
              nil))

           ((eq
             parent-type
             'pair)
            (if (tsc-node-named-p current-node)
                tsi-typescript-indent-offset
              nil))

           ((eq
             parent-type
             'parenthesized_expression)
            (if (tsc-node-named-p current-node)
                tsi-typescript-indent-offset
              nil))

           ((eq
             parent-type
             'statement_block)
            (if (and
                 (> (line-number-at-pos) (car (tsc-node-start-point parent-node)))
                 (< (line-number-at-pos) (car (tsc-node-end-point parent-node))))
                tsi-typescript-indent-offset
              nil))

           ((eq
             parent-type
             'switch_body)
            (if (tsc-node-named-p current-node)
                tsi-typescript-indent-offset
              nil))

           ((eq
             parent-type
             'switch_case)
            (if (tsc-node-named-p current-node)
                tsi-typescript-indent-offset
              nil))

           ((eq
             parent-type
             'switch_default)
            (if (tsc-node-named-p current-node)
                tsi-typescript-indent-offset
              nil))

           ((eq
             parent-type
             'type_alias_declaration)
            (if (and (tsc-node-named-p current-node) (not (eq current-type 'object_type)))
                tsi-typescript-indent-offset
              nil))

           ((eq
             parent-type
             'type_annotation)
            (if (tsc-node-named-p current-node)
                tsi-typescript-indent-offset
              nil))

           ((eq
             parent-type
             'type_arguments)
            (if (tsc-node-named-p current-node)
                tsi-typescript-indent-offset
              nil))

           ((eq
             parent-type
             'ternary_expression)
            (if (or
                 (equal
                  (tsc-node-text current-node)
                  "?")
                 (equal
                  (tsc-node-text current-node)
                  ":"))
                tsi-typescript-indent-offset
              nil))

           ((eq
             parent-type
             'union_type)
            (cond
             ((tsc-node-named-p current-node)
              tsi-typescript-indent-offset)
             ((not (eq
                    (tsc-node-type (tsi--highest-node-on-same-line-as parent-node))
                    'union_type))
              ;; type FooBar =
              ;;   | Foo
              ;;   | Bar;
              tsi-typescript-indent-offset)
             (t nil)))

           ((eq
             parent-type
             'variable_declarator)
            (if (tsc-node-named-p current-node)
                tsi-typescript-indent-offset
              nil))

           (t nil)))
         (comment-indentation
          (if (and
               (or
                (eq
                 current-type
                 'comment)
                (eq
                 (tsc-node-type
                  (tsi--highest-node-at current-node))
                 'program))
               (save-excursion
                 (beginning-of-line)
                 (back-to-indentation)
                 (looking-at-p "\\*")))
              1
            nil)))
    (tsi--debug "child indentation: %s parent indentation: %s comment indentation: %s" child-indentation parent-indentation comment-indentation)
    (+
     (or parent-indentation 0)
     (or child-indentation 0)
     (or comment-indentation 0))))

(defun tsi--current-line-empty-p ()
  (string-match-p "\\`[[:space:]]*$" (thing-at-point 'line)))

(defun tsi-typescript--get-indent-for-current-line ()
  (when-let* ((node-at-point (tree-sitter-node-at-point))
              (current-type (tsc-node-type node-at-point))
              (parent (tsc-get-parent node-at-point))
              (parent-type (tsc-node-type parent)))
    (cond
     ((and
       (tsi--current-line-empty-p)
       (or
        (eq current-type 'jsx_opening_element)
        (eq current-type 'jsx_self_closing_element)
        (eq current-type 'parenthesized_expression)
        (eq current-type 'type_parameters)
        (eq current-type 'type_arguments)
        (and
         (eq current-type 'object)
         (or (eq parent-type 'return_statement)
             (eq parent-type 'array)
             (eq parent-type 'assignment_expression)))))
      (progn (tsi--debug "indent for current line: %s" tsi-typescript-indent-offset) tsi-typescript-indent-offset))
     (t 0))))

;; exposed for testing purposes
;;;###autoload
(defun tsi-typescript--indent-line ()
  "Internal function.

  Calculate indentation for the current line."
  (tsi-indent-line #'tsi-typescript--get-indent-for #'tsi-typescript--get-indent-for-current-line))

(defun tsi-typescript--outdent-line ()
  "Outdents by `tsi-typescript-indent-offset`."
  (interactive)
  (let* ((current-indentation
          (save-excursion
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
  :group 'tsi-typescript
  :keymap (make-sparse-keymap)

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

