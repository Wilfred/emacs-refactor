# elisp-refactor

ELR allows you to define language-specific refactoring commands in Emacs. It has
a simple declarative interface for easy extension.

To use ELR when editing, simply move point to an expression and invoke the refactor menu.

![Refactoring menu example](https://raw.github.com/chrisbarrett/elisp-refactor/master/elr.png)

ELR ships with useful refactorings for Emacs Lisp, including:

* extract expression to function
* extract and inline variables
* extract autoloads for functions
* evalute expression and replace it with the result.

More languages are forthcoming. See *Extension* for details on extending ELR to
other language modes. It's easy (honest!).

## Dependencies

* [s](https://github.com/magnars/s.el)
* [dash](https://github.com/magnars/dash.el)
* [popup](https://github.com/magnars/dash.el)
* [cl-lib](https://github.com/emacsmirror/cl-lib)

Download them using [Melpa](http://melpa.milkbox.net/), or get them straight
from GitHub.

## Installation

Add this package to your load path and add an autoload for
`elr-show-refactor-menu`. Bind this command to something convenient.

```lisp
(add-to-list 'load-path "path/to/elr")
(autoload 'elr-show-refactor-menu "elr")

(add-hook 'prog-mode-hook
          (lambda () (local-set-key (kbd "M-RET") 'elr-show-refactor-menu)))
```

## Extension

Use the `elr-declare-action' macro to declare a refactoring action for a given
mode. The action will automatically become available in the refactoring popup
menu.

This macro supports predicate expressions, allowing the options displayed to be
context-sensitive.

As an example, here is the declaration for a refactoring that ships with ELR:

```lisp
;;; Extract constant
(elr-declare-action elr-extract-constant emacs-lisp-mode "constant"
  :predicate (not (elr--looking-at-definition?))
  :description "defconst")
```

This wires the `elr-extract-constant` function to be displayed in
`emacs-lisp-mode`, provided point is not looking at an Elisp definition form.

If your favourite language mode already offers refactoring commands, it is
simple to wire them up with ELR using this interface.

## TODO

* Support for other languages (esp. Clojure, Python, Ruby)

* Elisp:
   * inline function
   * remove PROGN on function extraction.

## Contributing

Pull requests are totally welcome. :D

## License

Licensed under the [GPL version 3](http://www.gnu.org/licenses/) or later.
