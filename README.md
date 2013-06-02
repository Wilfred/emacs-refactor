# emacs-refactor
<!-- Travis builds are disabled until cassou/emacs updates to Emacs 24.3 -->
<!-- [![Build Status](https://travis-ci.org/chrisbarrett/emacs-refactor.png?branch=master)](https://travis-ci.org/chrisbarrett/emacs-refactor) -->

EMR allows you to define language-specific refactoring commands in Emacs. It has
a simple declarative interface for easy extension.

To use EMR when editing, simply move point to an expression and invoke the refactor menu.

![Refactoring menu example](https://raw.github.com/chrisbarrett/emacs-refactor/master/assets/emr.png)

EMR ships with useful refactorings for the following languages:

* Elisp
* C (in progress)

More languages are forthcoming. See
[Extension](https://github.com/chrisbarrett/emacs-refactor#extension) for
details on extending EMR to other language modes. It's easy (honest!).

Tested on Emacs 24.3.

# Installation

`emr` is available on [MELPA](http://melpa.milkbox.net/). This is the easiest
way to install.

If you haven't set up MELPA, you'll need to add the following to your init.el

```lisp
;;; Initialize packages.

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
```

Once MELPA is configured:

1. `M-x package-install emr`

2. Configure your init.el:

  ```lisp
 (define-key prog-mode-map (kbd "M-RET") 'emr-show-refactor-menu)
 (eval-after-load "emr" '(emr-initialize))
   ```

# Development

You will need *carton*, *make* and *git* to build the project.

1. Install [Carton](https://github.com/rejeep/carton):

   ```
   curl -fsSkL https://raw.github.com/rejeep/carton/master/go | sh
   ```

2. Clone and install with `make`:

   ```
   cd
   git clone git@github.com:chrisbarrett/emacs-refactor.git
   cd emacs-refactor
   make
   ```

3. Configure your init.el:

  ```lisp
 (autoload 'emr-show-refactor-menu "emr"')
 (define-key prog-mode-map (kbd "M-RET") 'emr-show-refactor-menu)
 (eval-after-load "emr" '(emr-initialize))
   ```

## Dependencies

These will be installed automatically by Carton.

* [s](https://github.com/magnars/s.el)
* [dash](https://github.com/magnars/dash.el)
* [popup](https://github.com/auto-complete/popup-el)
* [cl-lib](https://github.com/emacsmirror/cl-lib)
* [list-utils](https://github.com/rolandwalker/list-utils)
* [redshank](https://github.com/emacsmirror/redshank)

Shout out to [@magnars](https://twitter.com/magnars) for his awesome libraries.

# Extension

Use the `emr-declare-action` macro to declare a refactoring action. The
action will automatically become available in the refactoring popup menu.

This macro supports predicate expressions, allowing the options displayed to be
context-sensitive.

As an example, here is the declaration for a refactoring command that ships with EMR:

```lisp
(emr-declare-action emr-el-extract-constant
  :title "constant"
  :description "defconst"
  :modes emacs-lisp-mode
  :predicate (not (or (emr-el:looking-at-definition?)
                      (emr-el:looking-at-let-binding-symbol?))))
```

This wires the `emr-el-extract-constant` function to be displayed in
`emacs-lisp-mode`, provided point is not looking at an Elisp definition or
let-binding form.

If your favourite language mode already offers refactoring commands, it is
simple to wire them up with EMR using this interface.

# Contributing

Pull requests are welcome. If appropriate, please add unit tests. See the tests for `emr-elisp` for examples.

## TODO

* C: More useful refactorings, eg, inline function/variable
* Support for other languages (esp. Clojure, Python, Ruby)

## Bug Reports

Please include backtraces with errors if applicable, as well as the simplest
code possible that will reproduce the issue.

# License

Licensed under the [GPL version 3](http://www.gnu.org/licenses/) or later.
