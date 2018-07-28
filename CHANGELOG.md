## v0.3.8 (unreleased)

Elisp:

* Fixed a crash on inlining functions when ido-yes-or-no-p was not
  installed.
* Fixed an issue when extracting let bindings inside `ert-deftest`
  forms.
* Improved names of refactorings to clarify what they do.
* Fixed an issue with extract refactorings from functions with
  autoload cookies, where the cookie got moved.
* Fixed an issue where inlining let variables wasn't always offered.
* Added a new refactoring for toggling between `let` and `let*`.

## v0.3.7

General:

* Fixed an issue where calling `emr-show-refactor-menu` would say 'no
  refactorings available' when emr hadn't been used.

Elisp:

* Fixed an issue where `emr-el-extract-function` was confused by
quoted symbols.
* Fixed an issue where extract refactorings were not offered when they
  should be.

CSS: Added `emr-css-toggle-important`.

Ruby: Added `ruby-refactor-convert-post-conditional`.

C/C++: Added `emr-cc-format-region` and `emr-cc-format-buffer` to
format with clang-format according to `emr-clang-format-style`.

iedit: Added `emr-iedit-in-function`, `emr-iedit-in-region` and
`emr-iedit-in-region`.

## v0.3.6

Not documented.
