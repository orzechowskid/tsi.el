;; set up some test libraries/utils/variables
(load "tsi.test")

;; require our language-specific file
(require 'tsi-typescript)

;; our custom matcher doesn't set a major mode, so we associate fundamental-mode with
;; a tree-sitter grammar here
(add-to-list 'tree-sitter-major-mode-language-alist '(fundamental-mode . tsx))
;; tell tsi.el to use the tsi-typescript indent function
(setq tsi--test-indent-fn 'tsi-typescript--indent-line)

(describe
 "indenting imports"
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

(describe
 "indenting exports"
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

(describe
 "indenting variable declarations"
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

(describe
 "indenting interface and type defs"
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

(describe
 "indenting union types"
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

(describe
 "indenting if/else statements"
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

(describe
 "indenting switch statements"
 (it "properly indents case statements"
     (expect
      "
switch (foo) {
  case 1:
    bar();
}
"
      :to-be-indented)))

(describe
 "indenting object contents"
 (it "properly indents multi-line type declarations"
     (expect
      "
interface FooRecord {
  foo:
    Record<
      string,
      Foo
    >
};
"
      :to-be-indented)))

(describe
 "indenting array contents"
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

(describe
 "indenting function arguments"
 (it "properly indents function arguments"
     (expect
      "
function foo(
  a,
  b
) {
  return a + b;
}
"
      :to-be-indented))
 (it "properly indents anonymous function arguments"
     (expect
      "
const foo = (
  a,
  b
) => a + b;
"
      :to-be-indented)))

(describe
 "indenting ternaries"
 (it "properly indents consequents and alternates on their own lines"
     (expect
      "
const foo = bar
  ? 1
  : 2;
"
      :to-be-indented)))

(describe
 "indenting comments"
 (it "properly indents single-line comments"
     (expect
      "
function foo() {
  // hello
}
"
      :to-be-indented))

 (it "properly indents multi-line comments"
     (expect
      "
function foo() {
  /**
   * @type {number}
   */
  let bar;
}
"
      :to-be-indented))

 (it "properly indents block comments at the head of the file"
     (expect
      "
/**********
 * banner *
 **********/
let foo;
"
      :to-be-indented)))

(describe
 "indenting enums"
 (it "properly indents Typescript enums with explicit values"
     (expect
      "
enum FooBar {
  foo = 'foo',
  bar = 'bar'
}
"
      :to-be-indented))
 (it "properly indents Typescript enums with implicit values"
     (expect
      "
enum FooBar {
  foo,
  bar
}
"
      :to-be-indented)))

(describe
 "indenting JSX"
 (it "properly indents attributes of JSX elements"
     (expect
      "
<div
  id='foo'
  className='bar'
>
</div>
"
      :to-be-indented))
 
 (it "properly indents attributes of self-closing JSX elements"
     (expect
      "
<div
  id='foo'
  className='bar'
/>
"
      :to-be-indented))
 
 (it "properly indents child elements"
     (expect
      "
<div>
  <span />
</div>
"
      :to-be-indented))
 
 (it "properly indents JSX expressions"
     (expect
      "
<div>
  {foo}
</div>
"
      :to-be-indented)))

(buttercup-run-discover)
