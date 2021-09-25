# tsi.el: tree-sitter indentation minor mode for Emacs

## Installation

0. Dependencies: make sure you have `[tree-sitter](https://emacs-tree-sitter.github.io/installation/)` installed already.
1. Install: download this package and place `tsi.el`, and the desired language-specific `tsi-<foo>.el` files,  inside a directory on your `load-path`
2. Require: `(require 'tsi-<foo>)`
3. Enable: `(tsi-<foo>-mode t)`

## Configuration

Useful variables are members of the relevant language-specific custom group and can be viewed and modified with the command `M-x customize-group [RET] tsi-<foo> [RET]`.

## Bugs

If a supported language is not indenting in the way you feel it should, then maybe it's a bug!  open an issue and we'll discuss it.

## Contributing

PRs for existing and new languages are most welcome.  Please make sure that any changes are accompanied with unit tests (see tsi-typescript.test.el for an example).  Unit tests use the super rad [buttercup](https://github.com/jorgenschaefer/emacs-buttercup) framework, and can be run via the following shell command:

`$ emacs -batch -q -L . -l tsi.test -l tsi-<foo>.test --traceback omit`

## License

GPLv3.  see LICENSE in the top level of this repository.
