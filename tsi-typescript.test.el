(require 'tsi-typescript)

(add-to-list 'tree-sitter-major-mode-language-alist '(fundamental-mode . tsx))
(setq tsi--test-indent-fn (lambda () (tsi-typescript--indent-line)))

(describe "indenting imports"
  (it "properly indents when named imports are on a separate line"
    (expect
     "
import {
  useState
} from 'react';
"
     :to-be-indented))

  (it "properly indents when default imports and named imports are on separate lines"
    (expect
     "
import React, {
  useState
} from 'react';
"
     :to-be-indented)))

(describe "indenting exports"
  (it "properly indents named exports"
    (expect
     "
export {
  foo
};
"
     :to-be-indented))

  (it "properly indents default exports when the exported symbol is on a separate line"
    (expect
     "
export default
  foo;
"
     :to-be-indented)))

(describe "indenting variable declarations"
  (it "properly indents when identifiers are on a separate line"
    (expect
     "
const
  x = 1,
  y = 2;
"
     :to-be-indented))

  (it "properly indents when initial values are on a separate line"
    (expect
     "
const x =
  1;
"
     :to-be-indented)))

(describe "indenting interface and type defs"
  (it "properly indents when an interface is defined across multiple lines"
    (expect
     "
interface Foo {
  bar: string;
}
"
     :to-be-indented))

  (it "properly indents generic types defined across multiple lines"
    (expect
     "
type Foo = Omit<
  Bar,
  'baz'
>;
"
     :to-be-indented)))

(describe "indenting union types"
  (it "properly indents when the first option is not on a separate line"
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

(describe "indenting if/else statements"
  (it "properly indents bracketed if statements"
    (expect
     "
if (foo) {
  bar();
}
"
     :to-be-indented))

  (it "properly indents un-bracketed if statements"
    (expect
     "
if (foo)
  bar();
"
     :to-be-indented))

  (it "properly indents bracketed if/else statements"
    (expect
     "
if (foo) {
  bar();
}
else {
  baz();
}
"
     :to-be-indented)))

(describe "indenting switch statements"
  (it "properly indents case statements"
    (expect
     "
switch (foo) {
  case 1:
    bar();
}
"
     :to-be-indented)))

(describe "indenting array contents"
  (it "properly indents child identifiers"
    (expect
     "
const x = [
  ...foo,
  ...bar
];
"
     :to-be-indented))

  (it "properly indents object literals"
    (expect
     "
const x = [{
  foo: true
}];
"
     :to-be-indented)
    (expect
     "
const x = [{
  foo: true
}, {
  foo: false
}];
"
     :to-be-indented)))
