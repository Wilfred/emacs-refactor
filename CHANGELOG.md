## v0.4 (unreleased)

elisp:

* 'autoload' refactorings have been renamed to 'add autoload' and
  'autoload cookie' for clarity.
* 'add autoload' is no longer pointlessly offered for functions
  defined in the current file.
* Only offer one of 'rename in function' and 'rename in file', as
  appropriate.
* Only offer 'rename in function' when point is on symbols that are
  bound in this function.
* Don't offer to rename special forms.
* 'Find unused' now considers interactive functions to be used, and is
  also more robust.

iedit:

* Fixed an issue with 'rename globally' where it only renamed the
  current instance of a symbol.
* Changed 'rename globally' to 'rename in file' to reflect what it
  actually does.
* EMR no longer modifies iedit-mode-map.

## v0.3.8

Elisp:

* Extracting let bindings is now much more generic and produces
  correct, equivalent code in many more circumstances.
* Fixed a crash on inlining functions when ido-yes-or-no-p was not
  installed.
* Fixed an issue when extracting let bindings inside `ert-deftest`
  forms.
* Improved names of refactorings to clarify what they do.
* Fixed an issue with extract refactorings from functions with
  autoload cookies, where the cookie got moved.
* Fixed an issue where inlining let variables wasn't always offered.
* Added a new refactoring for toggling between `let` and `let*`.

Redshank is no longer a dependency: it was only used for extracting
let bindings, and it was unmaintained.

Lisp:

* Remove comment/uncomment refactorings. These cluttered the refactor
  menu, as they were offered in all situations. There are many tools
  for commenting already, such as: `comment-or-uncomment-region`,
  `paredit-comment-dwim`, `comment-dwim`, `lispy-comment`.

Please file a bug if you miss this feature.

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
