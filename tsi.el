;;; tsi.el --- tree-sitter indentation -*- lexical-binding: t; -*-
;;; Summary:
;;; Commentary:
;;; Code:


(require 'tree-sitter)


(defvar tsi-debug
  nil
  "Debug boolean for tsi-mode.")


(defun tsi--debug (&rest args)
  "Internal function.

Print messages only when `tsi-debug` is `t`."
  (when tsi-debug
    (apply 'message args)))


(defun tsi--node-start-line (node)
  "Internal function.

Returns the number of the line containing the first byte of NODE."
  (if node
      (save-excursion
        (line-number-at-pos (tsc-node-start-position node)))
    1))


(defun tsi--highest-node-at (node)
  "Internal function.

Returns the uppermost tree node sharing a start position with NODE."
  (let* ((parent-node
          (tsc-get-parent node)))
    (while
        (and
         parent-node
         (eq
          (tsc-node-start-position parent-node)
          (tsc-node-start-position node)))
      (setq node parent-node)
      (setq parent-node (tsc-get-parent parent-node)))
    node))


(defun tsi--highest-node-on-same-line-as (node)
  "Internal function.

Returns the uppermost tree node sharing the same line as NODE."
  (let* ((line-number
          (tsi--node-start-line node))
         (node-at-new-point
          (save-excursion
            (forward-line
             (- line-number (line-number-at-pos)))
            (back-to-indentation)
            (tree-sitter-node-at-point))))
    (tsi--highest-node-at node-at-new-point)))


;;;###autoload
(defun tsi-walk (indent-info-fn)
  "Indents the current line using information provided by INDENT-INFO-FN.

INDENT-INFO-FN is a function taking two arguments: (current-node parent-node)."

  (let*
      ((original-position
        (point))
       (first-non-blank-pos
        (save-excursion
          (back-to-indentation)
          (point)))
       (empty-line
        (save-excursion
          (end-of-line)
          (skip-chars-backward " \t")
          (bolp)))
       (should-save-excursion ;; true if point is after any leading whitespace
        (< first-non-blank-pos original-position))
       (current-node
        (tsi--highest-node-at
         (save-excursion
           (back-to-indentation)
           (tree-sitter-node-at-point))))
       (parent-node
        (tsc-get-parent current-node))
       (indent-ops
        '()))
    (while parent-node
      (tsi--debug "parent: %s line %d, child: %s line %d" (tsc-node-type parent-node) (tsi--node-start-line parent-node) (tsc-node-type current-node) (tsi--node-start-line current-node))
      (push
       (funcall
        indent-info-fn
        current-node
        (if empty-line
            current-node
          parent-node))
       indent-ops)
      (tsi--debug "op: %s" (car indent-ops))
      ;; get outermost node on the line where parent starts
      (setq
       current-node
       (tsi--highest-node-on-same-line-as parent-node))
      (setq
       parent-node
       (tsc-get-parent current-node)))
    (tsi--debug "ops: %s" indent-ops)
    (let ((column
           (seq-reduce
            (lambda (accum elt)
              (cond
               ((numberp elt)
                (+ accum elt))
               (t accum)))
            indent-ops
            0)))
      (tsi--debug "indenting to column %d" column)
      (if should-save-excursion
          (save-excursion
            (indent-line-to column))
        (indent-line-to column)))))

(provide 'tsi)
;;; tsi.el ends here
