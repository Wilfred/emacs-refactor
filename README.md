[![License GPL 3][badge-license]][copying]
[![Build Status][badge-travis]][travis]

# emacs-refactor

## Summary

Emacs Refactor (EMR) provides language-specific refactoring support for
Emacs. It has a simple declarative interface for easy extension.

To use EMR when editing, simply move point to an expression and invoke the refactor menu.

[![Example][example-pic]][pic]

EMR ships with many refactoring commands, and pull requests for extensions
are welcome. See
[Extension](https://github.com/chrisbarrett/emacs-refactor#extension) for
details on extending EMR to other language modes. It's easy (honest!).

Tested on Emacs 24.3.

## Installation

`emr` is available on [MELPA][]. This is the easiest way to install.

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

## Language support

Most EMR commands are context-sensitive and are available through the
refactor menu. Some actions affect the whole buffer and are available in
the menu bar.

### General

These commands are available for all programming languages.

The following context-sensitive refactoring commands are available:

* *comment region*
* *uncomment region*

### C

The following context-sensitive refactoring commands are available:

* *tidy includes*

The following buffer-wide actions are available:

* *insert include*

Refactoring support for C is a work in progress. Contributions are welcome.

### Lisps

These commands are available to all Lisp dialects, including Clojure, Elisp
and Common Lisp.

The following context-sensitive refactoring commands are available:

* *comment form*
* *uncomment block*

### Elisp

The following context-sensitive refactoring commands are available:

* *delete unused definition*
* *delete unused let binding form*
* *eval and replace*
* *extract autoload*
* *extract constant*
* *extract function*
* *extract to let*
* *extract variable*
* *implement function*
* *inline function*
* *inline let variable*
* *inline variable*
* *insert autoload directive*
* *tidy autoloads*

The following buffer-wide actions are available:

* *find unused definitions*

### Scheme

The following refactoring commands are available:

* *extract function*
* *extract variable*

## Development

You will need [Cask][], [make][] and [git][] to build the project.

1. Install Cask:

   ```shell
   curl -fsSkL https://raw.github.com/cask/cask/master/go | python
   ```

2. Clone and install with `make && make install`:

   ```shell
   cd
   git clone git@github.com:chrisbarrett/emacs-refactor.git
   cd emacs-refactor
   make && make install
   ```

3. Configure your init.el:

  ```lisp
 (autoload 'emr-show-refactor-menu "emr")
 (define-key prog-mode-map (kbd "M-RET") 'emr-show-refactor-menu)
 (add-hook 'prog-mode-hook 'emr-initialize)
   ```

### Dependencies

These will be installed automatically by Cask.

* [cl-lib](https://github.com/emacsmirror/cl-lib)
* [dash](https://github.com/magnars/dash.el)
* [list-utils](https://github.com/rolandwalker/list-utils)
* [paredit](https://github.com/emacsmirror/paredit/blob/master/paredit.el)
* [popup](https://github.com/auto-complete/popup-el)
* [projectile](https://github.com/bbatsov/projectile)
* [redshank](https://github.com/emacsmirror/redshank)
* [s](https://github.com/magnars/s.el)

Shout out to [@magnars][] for his awesome libraries.

## Extension

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

## Contributing

Yes, please do. See [CONTRIBUTING][] for guidelines.

### TODO

* Elisp:
    * Simplify let statements when inlining functions
    * Use destructuring-bind when inlining functions that use destructuring
      in their arglists.
* C: More useful refactorings, eg, inline function/variable
* Support for other languages (esp. Clojure, Python, Ruby)

## License

See [COPYING][]. Copyright (c) 2014 Chris Barrett.

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
[badge-travis]: https://travis-ci.org/chrisbarrett/emacs-refactor.png?branch=master
[example-pic]: https://raw.github.com/chrisbarrett/emacs-refactor/master/assets/emr.png
[@magnars]: https://twitter.com/magnars
[Cask]: https://github.com/cask/cask
[make]: http://www.gnu.org/software/make/
[git]: http://git-scm.com
[MELPA]: http://melpa.milkbox.net/
[CONTRIBUTING]: ./CONTRIBUTING.md
[COPYING]: ./COPYING
