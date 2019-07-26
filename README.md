# emacs-refactor

[![Build Status](https://travis-ci.org/Wilfred/emacs-refactor.svg)](https://travis-ci.org/Wilfred/emacs-refactor)
[![Coverage Status](https://coveralls.io/repos/Wilfred/emacs-refactor/badge.svg)](https://coveralls.io/r/Wilfred/emacs-refactor)
[![MELPA](http://melpa.org/packages/emr-badge.svg)](http://melpa.org/#/emr)
[![MELPA stable](http://stable.melpa.org/packages/emr-badge.svg)](http://stable.melpa.org/#/emr)

Emacs Refactor (EMR) is a framework for providing language-specific
refactoring in Emacs. It includes refactoring commands for a variety
of languages, including elisp itself!

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

## Summary

To use EMR when editing, simply move point to an expression and 
`M-x emr-show-refactor-menu`.

![Example][example-pic]

EMR ships with many refactoring commands, and pull requests for extensions are
welcome. See [Extension](#user-content-extension) for details on extending EMR
to other language modes. It's easy (honest!).

## Installation

Install `emr` from [MELPA](http://www.melpa.org/), then configure your
init.el:

```emacs-lisp
(define-key prog-mode-map (kbd "M-RET") 'emr-show-refactor-menu)
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

[example-pic]: https://raw.github.com/Wilfred/emacs-refactor/master/assets/emr.png
[js2 refactor]: https://github.com/magnars/js2-refactor.el
[ruby refactor]: https://github.com/ajvargo/ruby-refactor
[git]: http://git-scm.com
