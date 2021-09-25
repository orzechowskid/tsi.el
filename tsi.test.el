;;; tsi.test.el --- test setup for tsi.el
;;; Code

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 4))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'buttercup)

(buttercup-define-matcher :to-be-indented (tst-obj-fn)
  (let ((txt
         (funcall tst-obj-fn)))
    (with-temp-buffer
      (setq-local indent-tabs-mode nil)
      (typescript-mode)
      (tree-sitter-mode t)
      (beginning-of-buffer)
      (insert txt)
      (indent-rigidly (point-min) (point-max) -100) ;; this could be done better
      (beginning-of-buffer)
      (while (not (eobp))
        (beginning-of-line)
        (tsi-typescript--indent-line)
        (forward-line))
      (if (equal
           txt
           (buffer-string))
          t
        `(nil . ,(format "expected:\n<<%s<<\nreceived:\n>>%s>>\n\n" txt (buffer-string)))))))


;;; tsi.test.el ends here
