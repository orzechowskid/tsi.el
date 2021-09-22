(require 'buttercup)
(require 'tsi "./tsi.el")
(require 'tsi-typescript "./tsi-typescript.el")
(require 'tsi-test "./tsi.test.el")

(describe "union types"
  (it "properly indents union types where the first option is not on a separate line"
    (expect
     (tsi-test-indent "
type FooBar = 'foo'
| 'bar';
")
     :to-equal
     "
type FooBar = 'foo'
  | 'bar';
"))

  (it "properly indents union types where the first option is on a separate line"
    (expect
     (tsi-test--indent "
type FooBar =
| 'foo'
| 'bar';
")
     :to-equal
     "
type FooBar =
  | 'foo'
  | 'bar';
")))
