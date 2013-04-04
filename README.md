# elisp-refactor

Provides refactoring commands for Emacs Lisp. To use, move point to an expression and invoke the refactor menu.

![Refactoring menu example](https://raw.github.com/chrisbarrett/elisp-refactor/master/elr.png)

The following commands are currently available:

* extract expression to function
* extract expression to defvar
* extract expression to defconst
* extract autoload for function

## Dependencies

* [s](https://github.com/magnars/s.el)
* [dash](https://github.com/magnars/dash.el)
* [popup](https://github.com/magnars/dash.el)
* [cl-lib](https://github.com/emacsmirror/cl-lib)

## Installation

Add this package to your load path and add an autoload for
`elr/show-refactor-menu`. Bind this command to something convenient.

```lisp
(add-to-list 'load-path "path/to/elisp-refactor")
(autoload 'elr/show-refactor-menu "elisp-refactor")

(add-hook 'emacs-lisp-mode-hook
          (lambda () (local-set-key (kbd "M-RET") 'elr/show-refactor-menu)))
```

## TODO

* inline function
* inline variable

## Contributing

Pull requests are totally welcome. :D

## License

Licensed under the [GPL version 3](http://www.gnu.org/licenses/) or later.
