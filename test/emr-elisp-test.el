;;; emr-elisp-test --- Tests for emr-elisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for emr-elisp

;;; Code:

;;;; Function implementation.

(require 'emr-elisp)
(require 'dash)
(require 's)

(ert-deftest emr-elisp-symbol-names ()
  "elisp--uses symbol names when inferring arglists from callsites"
  (let ((fname (cl-gensym)))
    (should
     (equal
      '(x y)
      (emr-el:infer-arglist-for-usage `(,fname x y))))))

(ert-deftest emr-elisp-argn ()
  "elisp--uses argn for non-symbol names when inferring arglists from callsites"
  (should
   (equal
    '(arg1 arg2)
    (emr-el:infer-arglist-for-usage '(hello 9 8)))))

(ert-deftest emr-el:interactive-form-p ()
  (should
   (not
    (emr-el:interactive-form-p
     '(defun foo () 1))))
  (should
   (emr-el:interactive-form-p
    '(defun foo () (interactive) 1)))
  (should
   (emr-el:interactive-form-p
    '(defun foo () "docstring" (interactive) 1))))

(ert-deftest emr-el:looking-at-symbol-p ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "foo")

    (goto-char (point-min))
    (should (emr-el:looking-at-symbol-p))

    (goto-char (point-max))
    (should (emr-el:looking-at-symbol-p)))

  (dolist (src '("()" "[]" "123" "1.1" "-1" "1e2" "?x" "\"foo\""
                 ";; foo" ":foo" "'foo"))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert src)
      (goto-char (point-min))

      (should (not (emr-el:looking-at-symbol-p))))))

;;;; Bound variables

(ert-deftest emr-elisp-free-vars-let ()
  "elisp--finds free vars in let form"
  (should
   (equal
    '(a b c d)

    (emr-el:free-variables
     '(let (x (y a))
        b
        (let (z w)
          c
          (list d)))))))

(ert-deftest emr-elisp-quoted-vars ()
  "elisp--quoted vars are not free variables"
  (should
   (equal
    '(x)

    (emr-el:free-variables
     '(list x 'y)))))

(ert-deftest emr-elisp-free-vars-let* ()
  "elisp--finds free vars in let* form"
  (should
   (equal
    '(a b c d)

    (emr-el:free-variables
     '(let* (x (y a))
        b
        (let* (z w)
          c
          (list d)))))))

(ert-deftest emr-elisp-free-vars-lambda ()
  "elisp--finds free vars in lambda form"
  (should
   (equal
    '(a b c)

    (emr-el:free-variables
     '(lambda (x &rest y)
        a
        (list b)
        (lambda (z w)
          c))))))

(ert-deftest emr-elisp-free-vars-progn ()
  "elisp--finds free vars in progn form"
  (should
   (equal
    '(a b c)

    (emr-el:free-variables
     '(progn
        a
        (lambda (x &rest y) b)
        (let (z w) c))))))

(ert-deftest emr-elisp-free-vars-bind ()
  "elisp--finds free vars in cl-destructuring-bind"
  (should
   (equal
    '(a b c d)

    (emr-el:free-variables
     '(cl-destructuring-bind (x y) (list 1 2)
        a
        (list b)
        (cl-destructuring-bind (z . w) (list 3 4 5)
          (list c d)))))))

(ert-deftest emr-free-vars-defun ()
  "elisp--finds free vars in defun form"
  (should
   (equal
    '(a b)

    (emr-el:free-variables
     '(defun hello (x y)
        (list a x)
        (progn
          (list b y)))))))

(ert-deftest emr-elisp-free-vars-function-sym ()
  "elisp--survives function symbol followed by non-lambda term"
  (let ((fname (cl-gensym)))
    (should
     (equal
      `(,fname)

      (emr-el:free-variables
       `(function ,fname))))))

(ert-deftest emr-elisp-free-vars-outer-scope ()
  "elisp--checks outer scope for bindings that share names with functions"
  (should
   (equal
    '(message y)

    (emr-el:free-variables '(funcall message y)
                           '(let (message)
                              (funcall message y))))))

(ert-deftest emr-elisp-extract-with-autoload ()
  "Ensure we don't move autoload cookies when we extract functions."
  (with-temp-buffer
    (delay-mode-hooks (emacs-lisp-mode))

    (insert ";;;###autoload\n(defun foo ()\n  (+ 1 2))")
    (search-backward "+")

    (emr-el-extract-function "extracted" '())

    ;; Autoload cookie should still be before foo.
    (should
     (s-contains-p ";;;###autoload\n(defun foo"
                   (buffer-string)))))

;;;; Inspecting forms at point

(ert-deftest emr-el:looking-at-let-binding-symbol? ()
  ;; Vars with initial values.
  (with-temp-buffer
    (insert "(let* ((x 1)\n       (y 2))\n  1)")
    (goto-char (point-min))
    (search-forward "y")

    (should (emr-el:looking-at-let-binding-symbol?)))
  ;; Vars without initial values.
  (with-temp-buffer
    (insert "(let* (x y)\n  1)")
    (goto-char (point-min))
    (search-forward "y")

    (should (emr-el:looking-at-let-binding-symbol?)))
  ;; Not looking at a symbol.
  (with-temp-buffer
    (insert "(let* ((x 1)\n       (y 2))\n  1)")
    (goto-char (point-min))
    (search-forward "2")

    (should (not (emr-el:looking-at-let-binding-symbol?))))
  ;; A symbol, but in the let body.
  (with-temp-buffer
    (insert "(let* ((x 1)\n       (y 2))\n  y)")
    (goto-char (point-max))
    (search-backward "y")

    (should (not (emr-el:looking-at-let-binding-symbol?))))
  ;; A symbol, but used as an initial value for another symbol.
  (with-temp-buffer
    (insert "(let* ((x 1)\n       (y z))\n  y)")
    (goto-char (point-min))
    (search-forward "z")

    (should (not (emr-el:looking-at-let-binding-symbol?)))))

(ert-deftest emr-elisp-top-level-let ()
  "elisp--a top level let is not a definition"
  (with-temp-buffer
    (insert "(let ((x 1))\n  (message \"foo: %s %s\" x 'bar))")
    (goto-char (point-min))
    (search-forward "message")
    (should (not (emr-el:looking-at-definition?)))))

(ert-deftest emr-el-extract-to-let--body ()
  "Extracting a variable from the body."
  (with-temp-buffer
    (insert "(let ((x 1))\n  (+ 1 2))")
    (search-backward "(+")
    (emr-el-extract-to-let 'y)

    (let ((result-form (read (buffer-string))))
      (should
       (equal
        result-form
        '(let ((x 1)
               (y (+ 1 2)))
           y))))))

(ert-deftest emr-el-extract-to-let--let-var ()
  "Extracting a variable from another let-bound variable."
  (with-temp-buffer
    (insert "(let ((x (+ 1 2)))\n  x)")
    (search-backward "(+")
    (emr-el-extract-to-let 'z)

    (let ((result-form (read (buffer-string))))
      (should
       (equal
        result-form
        '(let* ((z (+ 1 2))
                (x z))
           x))))))

(ert-deftest emr-el-extract-to-let--numeric-literal ()
  "Extracting a literal value from another let-bound variable."
  (with-temp-buffer
    (insert "(let ((x 3))\n  x)")
    (search-backward "3")
    (transient-mark-mode t)
    (set-mark (1+ (point)))
    (emr-el-extract-to-let 'z)

    (let ((result-form (read (buffer-string))))
      (should
       (equal
        result-form
        '(let* ((z 3)
                (x z))
           x))))))

(ert-deftest emr-el-extract-to-let--no-let ()
  "Extracting a let variable when we don't a let form yet."
  (with-temp-buffer
    (insert "(defun foo ()\n  (+ 1 2))")
    (search-backward "(+")
    (emr-el-extract-to-let 'x)

    (let ((result-form (read (buffer-string))))
      (should
       (equal
        result-form
        '(defun foo ()
           (let ((x (+ 1 2)))
             x)))))))

;;;; Commands

(ert-deftest emr-el:find-unused-defs ()
  "Ensure we don't crash on quoted definitions."
  (with-temp-buffer
    (insert "(defmacro my-defvar (name val)\n  `(defvar ,name ,val))")

    (emr-el:find-unused-defs)))

(defun emr-el-test-example-docstring (str)
  "Extract the example invocation, before and after from a docstring."
  (let (start-pos example before after)
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))

      (search-forward-regexp (rx bol "EXAMPLE:\n"))
      (setq start-pos (point))

      (search-forward-regexp (rx bol "BEFORE:\n"))
      (setq example
            (buffer-substring start-pos (line-beginning-position -1)))

      (setq start-pos (point))
      (search-forward-regexp (rx bol "AFTER:\n"))
      (setq before
            (buffer-substring start-pos (line-beginning-position -1)))

      (setq after
            (buffer-substring (point) (point-max))))
    (list
     (emr-el-tests:unindent-rigidly example)
     (emr-el-tests:unindent-rigidly before)
     (emr-el-tests:unindent-rigidly after))))

(defun emr-el-tests:unindent-rigidly (string)
  "Given an indented STRING, unindent rigidly until
at least one line has no indent."
  (let* ((lines (s-lines string))
         (nonblanks (--remove (s-blank? it) lines))
         ;; Get the leading whitespace for each line.
         (indents (--map (car (s-match (rx bos (+ whitespace)) it))
                         nonblanks))
         (min-indent (-min (--map (length it) indents)))
         (unindented (--map
                      (if (s-blank? it)
                          it
                        (substring it min-indent))
                      lines)))
    (s-trim (s-join "\n" unindented))))

(defmacro gentest-from-docstring (fname)
  "Define an ERT test according to the spec in FNAME's docstring.
FNAME is a refactoring command with a docstring of the following style:

<General description>

EXAMPLE:
  <The command form that will be called.>

BEFORE:

  <Buffer state before executing command, where a pipe ('|') char
   signifies POINT. >

AFTER:

  <Buffer state after executing command>

"
  `(ert-deftest ,fname ()
     (let* ((text-quoting-style 'straight)
            (docstring (documentation ',fname)))

       ;; Basic sanity checks before running.
       (assert (not (s-blank? docstring)))
       (assert (s-contains? "EXAMPLE:" docstring))
       (assert (s-contains? "BEFORE:" docstring))
       (assert (s-contains? "AFTER:" docstring))
       (-let [(form before after) (emr-el-test-example-docstring docstring)]
         (with-temp-buffer
           ;; Insert the BEFORE into the buffer.
           (delay-mode-hooks (lisp-mode))
           (insert before)

           ;; Move to position.
           (goto-char (point-min))
           (search-forward "|")
           (delete-char -1)

           ;; Run the command we're testing.
           (eval (read form))

           (let ((result
                  (s-trim (buffer-substring-no-properties (point-min) (point-max)))))
             (should (equal result after))))))))

(gentest-from-docstring emr-el-inline-variable)
(gentest-from-docstring emr-el-inline-let-variable)
(gentest-from-docstring emr-el-toggle-let*)
(gentest-from-docstring emr-el-eval-and-replace)
(gentest-from-docstring emr-el-extract-function)
(gentest-from-docstring emr-el-extract-constant)
(gentest-from-docstring emr-el-extract-variable)
(gentest-from-docstring emr-el-insert-autoload-directive)

(provide 'emr-elisp-test)

;;; emr-elisp-test.el ends here
