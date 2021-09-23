(require 'buttercup)
(require 'typescript-mode)

(load "./tsi.el")
(load "./tsi-typescript.el")

(buttercup-define-matcher :to-be-indented (tst-obj-fn)
  (let ((txt
         (funcall tst-obj-fn)))
    (message "txt: >%s<" txt)
    (with-temp-buffer
      (setq-local indent-tabs-mode nil)
      (typescript-mode)
      (tree-sitter-mode t)
      (beginning-of-buffer)
      (insert txt)
      (indent-rigidly (point-min) (point-max) -100)
      (message "1buffer: >%s<" (buffer-string))
      (beginning-of-buffer)
      (while (not (eobp))
        (beginning-of-line)
        (tsi-typescript--indent-line)
        (forward-line))
      (message "2buffer: >%s<" (buffer-string))
      (if (eq
           txt
           (buffer-string))
          t
        `(nil . ,(format "expected:\n%s\nreceived:\n%s\n\n" txt (buffer-string)))))))

(describe "indenting union types"
  (xit "properly indents when the first option is not on a separate line"
    (expect
     "
type FooBar = 'foo'
       | 'bar';
" :to-be-indented))

  (it "properly indents when the first option is on a separate line"
    (expect
     "
type FooBar =
  | 'foo'
  | 'bar';
" :to-be-indented)))
