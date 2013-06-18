# emacs-refactor
<!-- Travis builds are disabled until cassou/emacs updates to Emacs 24.3 -->
<!-- [![Build Status](https://travis-ci.org/chrisbarrett/emacs-refactor.png?branch=master)](https://travis-ci.org/chrisbarrett/emacs-refactor) -->

Emacs Refactor (EMR) provides language-specific refactoring support for
Emacs. It has a simple declarative interface for easy extension.

To use EMR when editing, simply move point to an expression and invoke the refactor menu.

![Refactoring menu example](https://raw.github.com/chrisbarrett/emacs-refactor/master/assets/emr.png)

EMR ships with many refactoring commands, and pull requests for extensions
are welcome. See
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
 (add-hook 'prog-mode-hook 'emr-initialize)
   ```

# Language support

Most EMR commands are context-sensitive and are available through the
refactor menu. Some actions affect the whole buffer and are available in
the menu bar.

## General

These commands are available for all programming languages.

The following context-sensitive refactoring commands are available:

* *comment region*
* *uncomment region*

## C

The following context-sensitive refactoring commands are available:

* *tidy includes*

The following buffer-wide actions are available:

* *insert include*

Refactoring support for C is a work in progress. Contributions are welcome.

## Lisps

These commands are available to all Lisp dialects, including Clojure, Elisp
and Common Lisp.

The following context-sensitive refactoring commands are available:

* *comment form*
* *uncomment block*

## Elisp

The following context-sensitive refactoring commands are available:

* *inline variable*
* *eval and replace*
* *extract function*
* *implement function*
* *extract variable*
* *extract constant*
* *insert autoload directive*
* *tidy autoloads*
* *extract autoload*
* *delete unused let binding form*
* *extract to let*
* *inline let variable*
* *inline function*
* *delete unused definition*

The following buffer-wide actions are available:

* *find unused definitions*

## Scheme

The following refactoring commands are available:

* *extract function*
* *extract variable*

# Development

You will need *carton*, *make* and *git* to build the project.

1. Install [Carton](https://github.com/rejeep/carton):

   ```shell
   curl -fsSkL https://raw.github.com/rejeep/carton/master/go | sh
   ```

2. Clone and install with `make`:

   ```shell
   cd
   git clone git@github.com:chrisbarrett/emacs-refactor.git
   cd emacs-refactor
   make
   ```

3. Configure your init.el:

  ```lisp
 (autoload 'emr-show-refactor-menu "emr"')
 (define-key prog-mode-map (kbd "M-RET") 'emr-show-refactor-menu)
 (add-hook 'prog-mode-hook 'emr-initialize)
   ```

## Dependencies

These will be installed automatically by Carton.

* [cl-lib](https://github.com/emacsmirror/cl-lib)
* [dash](https://github.com/magnars/dash.el)
* [list-utils](https://github.com/rolandwalker/list-utils)
* [paredit](https://github.com/emacsmirror/paredit/blob/master/paredit.el)
* [popup](https://github.com/auto-complete/popup-el)
* [projectile](https://github.com/bbatsov/projectile)
* [redshank](https://github.com/emacsmirror/redshank)
* [s](https://github.com/magnars/s.el)

Shout out to [@magnars](https://twitter.com/magnars) for his awesome libraries.

# Extension

Use the `emr-declare-command` macro to declare a refactoring action. The
action will automatically become available in the refactoring popup menu.

This macro supports predicate expressions, allowing the options displayed to be
context-sensitive.

As an example, here is the declaration for a refactoring command that ships with EMR:

```lisp
(emr-declare-command emr-el-extract-constant
  :title "constant"
  :description "defconst"
  :modes emacs-lisp-mode
  :predicate (lambda ()
                        (not (or (emr-el:looking-at-definition?)
                          (emr-el:looking-at-let-binding-symbol?)))))
```

This wires the `emr-el-extract-constant` function to be displayed in
`emacs-lisp-mode`, provided point is not looking at an Elisp definition or
let-binding form.

If your favourite language mode already offers refactoring commands, it is
simple to wire them up with EMR using this interface.

# Contributing

Pull requests are welcome. If appropriate, please add unit tests. See the
tests for `emr-elisp` for examples.

## TODO

* Elisp:
    * Simplify let statements when inlining functions
    * Use destructuring-bind when inlining functions that use destructuring
      in their arglists.
* C: More useful refactorings, eg, inline function/variable
* Support for other languages (esp. Clojure, Python, Ruby)

## Bug Reports

Please include backtraces with errors if applicable, as well as the simplest
code possible that will reproduce the issue.

# License

Licensed under the [GPL version 3](http://www.gnu.org/licenses/) or later.
