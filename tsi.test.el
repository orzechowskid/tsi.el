;;; tsi.test.el --- test utils for tsi.el
;;; Code

(defun tsi-test--indent (txt)
  "Internal function.  applies an indent function to each line of text in TXT."
  (with-temp-buffer
    (insert txt)
    (buffer-string)))
  
(provide 'tsi-test)
;;; tsi.test.el ends here
