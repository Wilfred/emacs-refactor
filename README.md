# emacs-refactor

[![Build Status](https://travis-ci.org/wilfred/emacs-refactor.svg)](https://travis-ci.org/wilfred/emacs-refactor)
[![Coverage Status](https://coveralls.io/repos/wilfred/emacs-refactor/badge.svg)](https://coveralls.io/r/wilfred/emacs-refactor)
[![MELPA](http://melpa.org/packages/emr-badge.svg)](http://melpa.org/#/emr)
[![MELPA stable](http://stable.melpa.org/packages/emr-badge.svg)](http://stable.melpa.org/#/emr)

Emacs Refactor (EMR) provides language-specific refactoring support for Emacs.
It has a simple declarative interface for easy extension.

- [Summary](#user-content-summary)
- [Installation](#user-content-installation)
- [Language support](#user-content-language-support)
    - [General](#user-content-general)
    - [C](#user-content-c)
    - [Lisps](#user-content-lisps)
    - [Elisp](#user-content-elisp)
    - [JavaScript](#user-content-javascript)
    - [Ruby](#user-content-ruby)
    - [Scheme](#user-content-scheme)
- [Extension](#user-content-extension)
- [Development](#user-content-development)
- [Contributing](#user-content-contributing)
    - [TODO](#user-content-todo)
- [License](#user-content-license)

## Summary

To use EMR when editing, simply move point to an expression and invoke the refactor menu.

![Example][example-pic]

EMR ships with many refactoring commands, and pull requests for extensions are
welcome. See [Extension](#user-content-extension) for details on extending EMR
to other language modes. It's easy (honest!).

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

### JavaScript

JavaScript refactoring support requires [js2 refactor][].

The following refactoring commands are available:

* *extract function*
* *extract method*
* *extract variable*
* *add parameter*
* *local variable to instance variable*
* *log region*

### Ruby

Ruby refactoring support requires [ruby refactor][].

The following refactoring commands are available:

* *extract function*
* *extract variable*
* *extract constant*
* *add parameter*
* *extract to let*

### Scheme

The following refactoring commands are available:

* *extract function*
* *extract variable*

## Extension

Use the `emr-declare-command` function to declare a refactoring action. The
action will automatically become available in the refactoring popup menu.

This function supports predicate expressions, allowing the options displayed to
be context-sensitive.

As an example, here is the declaration for a refactoring command that ships with
EMR:

```lisp
(emr-declare-command 'emr-el-extract-constant
  :title "constant"
  :description "defconst"
  :modes 'emacs-lisp-mode
  :predicate (lambda ()
               (not (or (emr-el:looking-at-definition?)
                        (emr-el:looking-at-let-binding-symbol?)))))
```

This wires the `emr-el-extract-constant` function to be displayed in
`emacs-lisp-mode`, provided point is not looking at an Elisp definition or
let-binding form.

If your favourite language mode already offers refactoring commands, it is
simple to wire them up with EMR using this interface.

## Development

You will need [Cask][], [make][] and [git][] to build the project.

1. Install Cask:

   ```shell
   curl -fsSkL https://raw.github.com/cask/cask/master/go | python
   ```

2. Clone and install with `make && make install`:

   ```shell
   cd
   git clone git@github.com:wilfred/emacs-refactor.git
   cd emacs-refactor
   make && make install
   ```

3. Configure your init.el:

  ```lisp
 (autoload 'emr-show-refactor-menu "emr")
 (define-key prog-mode-map (kbd "M-RET") 'emr-show-refactor-menu)
 (add-hook 'prog-mode-hook 'emr-initialize)
   ```

## Contributing

Yes, please do. See [CONTRIBUTING][] for guidelines.

Thanks to the following contributors:

- Sreenath Nannat ([@tacticiankerala][]) for adding Ruby and JavaScript bindings

### TODO

* Elisp:
    * Simplify let statements when inlining functions
    * Use destructuring-bind when inlining functions that use destructuring
      in their arglists.
* C: More useful refactorings, eg, inline function/variable

## License

See [COPYING][]. Copyright (c) 2014-2015 Chris Barrett.

[example-pic]: https://raw.github.com/wilfred/emacs-refactor/master/assets/emr.png
[Cask]: https://github.com/cask/cask
[make]: http://www.gnu.org/software/make/
[js2 refactor]: https://github.com/magnars/js2-refactor.el
[ruby refactor]: https://github.com/ajvargo/ruby-refactor
[git]: http://git-scm.com
[MELPA]: http://melpa.milkbox.net/
[CONTRIBUTING]: ./CONTRIBUTING.md
[COPYING]: ./COPYING
[@tacticiankerala]: https://github.com/tacticiankerala
