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
 "indenting variable assignment"
 (it "properly indents when the value for an assignment is on a separate line"
     (expect
      "
x =
  123;
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
      :to-be-indented))

 (it "properly indents object types inside type annotations"
     (expect
      "
interface X {
  x: {
    
  };
}"
      :to-be-indented)) 
 )

(describe
 "indenting union types"
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
      :to-be-indented))

(it "properly indents default statements"
     (expect
      "
switch (foo) {
  default:
    bar();
}
"
      :to-be-indented))

(it "properly indents blank lines in switch case statements"
     (expect
      "
switch (x) {
  
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
 "indenting type definitions"
 (it "properly indents multi-line conditional types"
     (expect
      "
type A<T, U> = T extends U ?
  U : never;
"
      :to-be-indented))
 )
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
      :to-be-indented))

 (it "properly indents object arguments to functions on blank lines"
     (expect
      "
foo({
  
});
"
      :to-be-indented)))

(describe
 "indenting arrow functions"
 (it "properly indents arrow expression spanning two lines"
     (expect
      "
const x = () =>
  4 + 2;
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

(describe
 "indenting whitespace"
 ;; NB: you might have to make whitespace visible in order for some of these tests to make sense :)
 (it "properly indents lines inside of statement blocks"
     (expect
      "
function() {
  
}
"
      :to-be-indented))

 (it "properly indents whitepace in multi-line JSX opening elements"
     (expect
      "
<div
  
>
</div>
"
      :to-be-indented))

 (it "properly indents whitepace in multi-line JSX opening elements"
     (expect
      "
<div
  
>
</div>
"
      :to-be-indented))

 (it "properly indents whitepace in multi-line JSX self-closing elements"
     (expect
      "
<div
  
/>
"
      :to-be-indented))

 (it "properly indents whitespace in if statements multi-line parenthesis"
     (expect
      "
if (
  
) {
  
}
else if (
  
) {
  
}
"
      :to-be-indented))

 (it "properly indents whitespace in arrow functions with parenthesis"
     (expect
      "
const X = () => (
  3
  
);
"
      :to-be-indented))


 (it "properly indents multi-line objects in return statements"
     (expect
      "
function f() {
  return {
    
  };
}
"
      :to-be-indented))

 (it "properly indents multi-line formal parameters"
     (expect
      "
const x = (
  
) => {}
"
      :to-be-indented))

 (it "properly indents multi-line type parameters"
     (expect
      "
function f<
  
>() {}
"
      :to-be-indented))

 (it "properly indents multi-line type arguments"
     (expect
      "
type HasLength = Pick<
  
>;
"
      :to-be-indented))

 (it "properly indents multi-line objects in assignment expression"
     (expect
      "
x = {
  
}
"
      :to-be-indented))

 (it "properly indents multi-line intersection types"
     (expect
      "
type C = A &
  B;
"
      :to-be-indented))

 (it "properly indents multi-line objects inside arrays"
     (expect
      "
[
  {
    
  }
]
"
      :to-be-indented))

 (it "properly indents blank lines inside object literals"
     (expect
      "
{
  x: {
    
  }
}
"
      :to-be-indented))

 (it "properly indents type aliases"
     (expect
      "
type X = {
  
}
"
      :to-be-indented))

 (it "properly indents switch statement bodies"
     (expect
      "
switch (x) {
  
}
"
      :to-be-indented))
 (it "properly indents case statements"
     (expect
      "
switch (x) {
  case 1:
    
    3;
}
"
      :to-be-indented))

 (it "properly indents default statements"
     (expect
      "
switch (condition) {
  case expression:
    break;
  default:
    
    return 4;
}
"
      :to-be-indented))

 (it "properly indents blank lines inside multiline arrays"
     (expect
      "
[
  
]
"
      :to-be-indented))

 (it "properly blank lines inside nested multiline arrays with delimeters on same line"
     (expect
      "
[[
  
]]
"
      :to-be-indented))

 (it "properly blank lines inside object in an array with delimeters on same line"
     (expect
      "
[{
  
}]"
      :to-be-indented))

 (it "properly blank lines inside an array inside of an object inside of an array"
     (expect
      "
[{[
  
]}]"
      :to-be-indented))

 (it "properly blank lines inside an array inside of an object inside of an object"
     (expect
      "
[{{
  
}}]"
      :to-be-indented))
 
 (it "properly indents blank lines inside function arguments"
     (expect
      "
f(
  
)"
      :to-be-indented))
 
 (it "properly indents blank lines inside arrays inside parenthesized_expression"
     (expect
      "
if ([
  
]) {}"
      :to-be-indented)))

(describe
 "indents new scopes"
 (it "properly indents expression statement inside statement block "
     (expect
      "
{
  0
}
"
      :to-be-indented))

 (it "properly indents two expression statements inside statement block "
     (expect
      "
{
  0
  0
}
"
      :to-be-indented))

 (it "properly indents expression statement inside statement block with a preceding node"
     (expect
      "
0
{
  0
}
"
      :to-be-indented))

 (it "properly indents expression statement inside if block "
     (expect
      "
if (0)
{
  0
}
"
      :to-be-indented))

 (it "properly indents expression statement inside statement block preceded by expression statement"
     (expect
      "
0 {
  0
}
"
      :to-be-indented))
)

(describe
 "indenting parenthesis"

 (it "properly indents parenthesized types"
     (expect
      "
let a: (
  0
)
"
      :to-be-indented)))

(describe
 "multiline operators"

 (it "properly indents Operators split over multiple lines with operators at end of lines"
     (expect
      "
0 +
  0 +
  0
"
      :to-be-indented))
 
 (it "properly indents Operators split over multiple lines with operators at beginning of lines"
     (expect
      "
0 +
  + 0
  + 0"

      :to-be-indented)))

(describe 
 "indenting interpolated keys"

 (it "properly indents multiline interpolated variable declarations"
     (expect
      "
let a: {[
  0
]: 0}
"
      :to-be-indented))

 (it "properly indents multiline interpolated variable assignments"
     (expect
      "
let a = {[
  0
]: 0}
"
      :to-be-indented)))
 
(buttercup-run-discover)
