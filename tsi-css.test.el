;; set up some test libraries/utils/variables
(load "tsi.test")

;; require our language-specific file
(require 'tsi-css)

;; our custom matcher doesn't set a major mode, so we associate fundamental-mode with
;; a tree-sitter grammar here
(add-to-list 'tree-sitter-major-mode-language-alist '(fundamental-mode . css))
;; tell tsi.el to use the tsi-css indent function
(setq tsi--test-indent-fn 'tsi-css--indent-line)

(describe
 "indenting rules"
 (it "properly indents CSS rules inside selectors"
     (expect
      "
.foo {
  color: tomato;
}
"
      :to-be-indented))

 (it "properly indents CSS rule values"
     (expect
      "
.foo {
  color:
    tomato;
}
"
      :to-be-indented))

 (it "properly indents arguments to CSS functions"
     (expect
      "
.foo {
  color: rgb(
    0, 0, 0
  );
}
"
      :to-be-indented)
     (expect
      "
.foo {
  color: rgb(
    0,
    0,
    0
  );
}
"
      :to-be-indented)))

(describe
 "indenting selectors"
 (it "properly indents lists of selectors"
     (expect
      "
.foo,
.bar {
  color: tomato;
}
"
      :to-be-indented))

 (it "properly indents nested selectors"
     (expect
      "
.foo {
  .bar {
    color: tomato;
  }
}
"
      :to-be-indented)

     (expect
      "
.foo {
  .bar,
  .baz {
    color: tomato;
  }
}
"
      :to-be-indented)))

(describe
 "indenting comments"
 (it "properly indents comments at the head of the file"
     (expect
      "
/****
 * hello there
 ****/
.foo {
}
"
      :to-be-indented))

 (it "properly indents comments inside of a selector"
     (expect
      "
.foo {
  /*
   * hello there
   */
}
"
      :to-be-indented)))

(buttercup-run-discover)
