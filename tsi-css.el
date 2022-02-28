;;; tsi-css.el --- tree-sitter indentation for CSS and friends -*- lexical-binding: t; -*-

;;; Version: 1.0.2

;;; Author: Dan Orzechowski

;;; URL: https://github.com/orzechowskid/tsi.el

;;; Package-Requires: ((tsi "1.0.0"))

;;; SPDX-License-Identifier: GPLv3

;;; Commentary:
;; a file containing indentation rules for CSS.  Enable
;; it via `M-x tsi-css-mode`, or configure as part of your major-mode hook:
;;
;; (add-hook 'css-mode-hook  (lambda () (tsi-css-mode t)))
;;
;; Enabling `tsi-css-mode` will set the current buffer's `indent-line-function`,
;; as well as register an outdent function (<S-iso-lefttab>, aka <backtab>).  I should
;; make that configurable at some point.
;;
;; Indentation amount is controlled by the value of `tsi-css-indent-offset`.

;;; Code:

(require 'tsi)

(defgroup tsi-css nil
  "Minor mode for indenting CSS using the tree-sitter CST."
  :group 'programming
  :prefix "tsi-css-")

(defcustom tsi-css-indent-offset 2
  "Default indent level."
  :type 'number
  :group 'tsi-css)

(defun tsi-css--get-indent-for (current-node parent-node)
  "Returns an indentation operation for the given CURRENT-NODE and PARENT-NODE."
  (let* ((current-type
          (tsc-node-type current-node))
         (parent-type
          (tsc-node-type parent-node))
         (parent-indentation
          (cond
            ((eq
              parent-type
              'stylesheet)
             nil)
            ((eq
              parent-type
              'declaration)
             (if (eq
                  current-type
                  'plain_value)
                 tsi-css-indent-offset
               nil))
            ((eq
              parent-type
              'block)
             (if (tsc-node-named-p current-node)
                 tsi-css-indent-offset
               nil))
            ((eq
              parent-type
              'arguments)
             (if (tsc-node-named-p current-node)
                 tsi-css-indent-offset
               nil))))
         (child-indentation
          nil)
         (comment-indentation
          (if (and
               (or
                (eq
                 current-type
                 'comment)
                (eq
                 (tsc-node-type
                  (tsi--highest-node-at current-node))
                 'stylesheet))
               (save-excursion
                 (beginning-of-line)
                 (back-to-indentation)
                 (looking-at-p "\\*")))
              1
            nil)))
    (tsi--debug "child indentation: %s parent indentation: %s" child-indentation parent-indentation)
    (+
     (or parent-indentation 0)
     (or child-indentation 0)
     (or comment-indentation 0))))


;; exposed for testing purposes
;;;###autoload
(defun tsi-css--indent-line ()
  "Internal function.

  Calculate indentation for the current line."
  (tsi-indent-line #'tsi-css--get-indent-for))

(defun tsi-css--outdent-line ()
  "Outdents by `tsi-css-indent-offset`."
  (interactive)
  (let* ((current-indentation
          (save-excursion
            (back-to-indentation)
            (current-column)))
         (new-indentation
          (max
           0
           (- current-indentation tsi-css-indent-offset))))
    (delete-region
     (progn (beginning-of-line) (point))
     (progn (back-to-indentation) (point)))
    (indent-to-column new-indentation)
    (back-to-indentation)))

;;;###autoload
(define-minor-mode tsi-css-mode
    "Use tree-sitter to calculate indentation for CSS buffers."
  :group 'tsi-css
  :keymap (make-sparse-keymap)

  (cond
   (tsi-css-mode
    ;; enabling mode
    ;; ensure tree-sitter is loaded
    (unless tree-sitter-mode
      (tree-sitter-mode))
    ;; update indent-line-function
    (setq-local
     indent-line-function
     #'tsi-css--indent-line)
    ;; add an outdent function
    (define-key tsi-css-mode-map (kbd "<S-iso-lefttab>") #'tsi-css--outdent-line)
    t)
   (t
    ;; disabling mode
    (setq-local
     indent-line-function
     (default-value 'indent-line-function)))))

(provide 'tsi-css)
