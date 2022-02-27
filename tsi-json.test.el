;; set up some test libraries/utils/variables
(load "tsi.test")

;; require our language-specific file
(require 'tsi-json)

;; our custom matcher doesn't set a major mode, so we associate fundamental-mode with
;; a tree-sitter grammar here
(add-to-list 'tree-sitter-major-mode-language-alist '(fundamental-mode . json))
;; tell tsi.el to use the tsi-json indent function
(setq tsi--test-indent-fn 'tsi-json--indent-line)

(describe
 "JSON indentation"
 (it "properly indents objects"
     (expect
      "
{
  \"foo\": {
    \"bar\": true
  }
}
"
      :to-be-indented))

 (it "properly indents arrays"
     (expect
      "
{
  \"foo\": [
    1,
    2,
    3
  ]
}
"
      :to-be-indented))
 (it "properly indents the right-side of a key/value pair"
     (expect
      "
{
  \"foo\":
    true
}
"
      :to-be-indented)))

(buttercup-run-discover)
