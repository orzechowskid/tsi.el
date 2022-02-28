;;; tsi-json.el --- tree-sitter indentation for JSON -*- lexical-binding: t; -*-

;;; Version: 1.1.2

;;; Author: Dan Orzechowski

;;; URL: https://github.com/orzechowskid/tsi.el

;;; Package-Requires: ((tsi "1.0.0"))

;;; SPDX-License-Identifier: GPLv3

;;; Commentary:
;; a file containing indentation rules for JSON.  Enable
;; it via `M-x tsi-json-mode`, or configure as part of your major-mode hook:
;;
;; (add-hook 'json-mode-hook  (lambda () (tsi-json-mode t)))
;;
;; Enabling `tsi-json-mode` will set the current buffer's `indent-line-function`,
;; as well as register an outdent function (<S-iso-lefttab>, aka <backtab>).  I should
;; make that configurable at some point.
;;
;; Indentation amount is controlled by the value of `tsi-json-indent-offset`.

;;; Code:

(require 'tsi)

(defgroup tsi-json nil
  "Minor mode for indenting JSON using the tree-sitter CST."
  :group 'programming
  :prefix "tsi-json-")

(defcustom tsi-json-indent-offset 2
  "Default indent level."
  :type 'number
  :group 'tsi-json)

(defun tsi-json--get-indent-for (current-node parent-node)
  "Returns an indentation operation for the given CURRENT-NODE and PARENT-NODE."
  (let* ((current-type
          (tsc-node-type current-node))
         (parent-type
          (tsc-node-type parent-node)))
    (cond
     ((eq
       parent-type
       'array)
      (if (tsc-node-named-p current-node)
          tsi-json-indent-offset
        nil))

     ((eq
       parent-type
       'object)
      (if (tsc-node-named-p current-node)
          tsi-json-indent-offset
        nil))

     ((eq
       parent-type
       'pair)
      tsi-json-indent-offset)

     (t nil))))

;; exposed for testing purposes
;;;###autoload
(defun tsi-json--indent-line ()
  "Internal function.

  Calculate indentation for the current line."
  (tsi-indent-line #'tsi-json--get-indent-for))

(defun tsi-json--outdent-line ()
  "Outdents by `tsi-json-indent-offset`."
  (interactive)
  (let* ((current-indentation
          (save-excursion
            (back-to-indentation)
            (current-column)))
         (new-indentation
          (max
           0
           (- current-indentation tsi-json-indent-offset))))
    (delete-region
     (progn (beginning-of-line) (point))
     (progn (back-to-indentation) (point)))
    (indent-to-column new-indentation)
    (back-to-indentation)))

;;;###autoload
(define-minor-mode tsi-json-mode
  "Use tree-sitter to calculate indentation for JSON buffers."
  :group 'tsi-json
  :keymap (make-sparse-keymap)

  (cond
   (tsi-json-mode
    ;; enabling mode
    ;; ensure tree-sitter is loaded
    (unless tree-sitter-mode
      (tree-sitter-mode))
    ;; update indent-line-function
    (setq-local
     indent-line-function
     #'tsi-json--indent-line)
    ;; add an outdent function
    (define-key tsi-json-mode-map (kbd "<S-iso-lefttab>") #'tsi-json--outdent-line)
    t)
   (t
    ;; disabling mode
    (setq-local
     indent-line-function
     (default-value 'indent-line-function)))))

(provide 'tsi-json)
