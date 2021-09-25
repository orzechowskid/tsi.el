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
(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)

(require 'buttercup)
(require 'tree-sitter)
(require 'tree-sitter-langs)

(defvar tsi--test-indent-fn
  nil)

(buttercup-define-matcher :to-be-indented (tst-obj-fn)
  (let ((txt
         (funcall tst-obj-fn)))
    (with-temp-buffer
      (setq-local indent-tabs-mode nil)
      ;; set this in a (before-all) form in your test file
      (setq-local indent-line-function #'tsi--test-indent-fn)
      (tree-sitter-mode t)
      (insert txt)
      (indent-rigidly (point-min) (point-max) -100) ;; this could be done better
      (while (not (bobp))
        (beginning-of-line)
        (funcall tsi--test-indent-fn)
        (forward-line -1))
      (if (equal
           txt
           (buffer-string))
          t
        `(nil . ,(format "\nexpected:\n<<%s<<\nreceived:\n>>%s>>\n\n" txt (buffer-string)))))))

(setq buttercup-stack-frame-style 'omit)

;;; tsi.test.el ends here
