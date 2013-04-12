;;; emr-elisp-tests --- Tests for emr-elisp

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

(require 'ert)
(require 'emr (expand-file-name "../emr.el"))
(require 'emr-elisp (expand-file-name "../emr-elisp.el"))

(defmacro check (desc &rest body)
  "Wrap `ert-deftest' with a simpler interface.
DESC is a string describing the test.
BODY lists the forms to be executed."
  (declare (indent 1))
  `(ert-deftest
       ,(intern (replace-regexp-in-string "[ .]" "_" desc)) ()
     ,@body))

(defun should= (x y)
  "Assert that objects X and Y are equal."
  (should (equal x y)))

(defun should-match (regex str)
  "Assert that string STR matches REGEX."
  (should (string-match-p regex str)))

;;; ----------------------------------------------------------------------------

;;; Read/Print
;;;
;;; EMR Elisp uses custom read/print commands that tokenise whitespace and comments.

(check "reading single-line form does not change input"
  (should= '(input) (emr--read "(input)")))

(check "can read and print Lisp forms on one line"
  (let ((line "(list 1 2 3 4)"))
    (should= (emr--print (emr--read line))
             line)))

(check "preserves line-endings"
  (let ((line "(list\n1)"))
    (should (string-match-p "\n" (emr--print (emr--read line))))))

(check "preserves whole-line comments"
  (let* ((expr    "(list \n ;;; Comment \n )")
         (output  (emr--print (emr--read expr)))
         (line    (nth 1 (split-string output "\n"))))
    (should-match (rx bol (* space) ";;; Comment" (* space) eol)
                  line)))

(check "preserves eol-line comments"
  (let* ((expr    "(list ;;; Comment \n )")
         (output  (emr--print (emr--read expr)))
         (line    (car (split-string output "\n"))))
    (should-match (rx  "(list "(* space) ";;; Comment" (* space) eol)
                  line)))

;;; Let-extraction

(check "let-extracted variables use let when bindings are not recursive"
  (should=
   '(let ((z w) :emr--newline
          (x y)) :emr--newline
          body)

   (emr--add-let-binding 'x 'y '(let ((z w)) body))))

(check "let-extracted variables use let* when bindings are recursive"
  (should=
   '(let* ((z w) :emr--newline
           (z y)) :emr--newline
           body)

   (emr--add-let-binding 'z 'y '(let ((z w)) body))))

(check "uses let when there is a single bound item"
  (should= '(let ((x x)))
           (emr--add-let-binding 'x 'x nil)))

(check "can let-extract atom where body is single form"
  (should= '(let ((x y)) :emr--newline body)
           (emr--add-let-binding 'x 'y 'body)))

(check "can let-extract single forms where body is list"
  (should= '(let ((x y)) :emr--newline (body))
           (emr--add-let-binding 'x 'y '(body))))

(check "can let-extract defun string in the docstring position"
  (should=
   '(defun fn (args) (let ((x y)) :emr--newline
                          "str"))

   (emr--add-let-binding 'x 'y '(defun fn (args) "str"))))

(check "can let-extract forms in defun body where no decls or docstring"
  (should=
   '(defun fn (args) (let ((x y)) :emr--newline
                          (body)))

   (emr--add-let-binding 'x 'y '(defun fn (args) (body)))))

(check "can let-extract forms in single-form defun body"
  (should=
   '(defun fn (args) (let ((x y)) :emr--newline
                          body))

   (emr--add-let-binding 'x 'y '(defun fn (args) body))))

(check "can let-extract forms in defun body where docstring exists"
  (should= '(defun fn (args) "docstring"
              (let ((x y)) :emr--newline
                   (body)))
           (emr--add-let-binding 'x 'y '(defun fn (args) "docstring" (body)))))

(check "can let-extract forms in defun body where decls exists"
  (should=
   '(defun fn (args) "docstring" (interactive) (declare cthulhu) (let ((x y)) :emr--newline
                                                                      (body)))
   (emr--add-let-binding
    'x 'y '(defun fn (args) "docstring" (interactive) (declare cthulhu) (body)))))

(check "can let-extract list form in defvar body"
  (should=
   '(defvar variable :emr--newline
      (let ((x y)) :emr--newline
           (body)))

   (emr--add-let-binding 'x 'y '(defvar variable (body)))))

(check "can let-extract atom in defvar body"
  (should=
   '(defvar variable :emr--newline
      (let ((x y)) :emr--newline
           (body)))

   (emr--add-let-binding 'x 'y '(defvar variable (body)))))

(check "can let-extract atom form in defvar body where docstring exists"
  (should=
   '(defvar variable :emr--newline
      (let ((x y)) :emr--newline
           body) :emr--newline
      "docstring")
   (emr--add-let-binding 'x 'y '(defvar variable body "docstring"))))

;;; Function implementation.

(check "uses symbol names when inferring arglists from callsites"
  (should=
   '(x y)
   (emr--infer-arglist-for-usage '(hello x y))))

(check "uses argn for non-symbol names when inferring arglists from callsites"
  (should=
   '(arg1 arg2)
   (emr--infer-arglist-for-usage '(hello 9 8))))

;;; Bound variables

(check "finds free vars in let form"
  (should=
   '(a b c d)

   (emr--free-variables
    '(let (x (y a))
       b
       (let (z w)
         c
         (list d))))))

(check "finds free vars in let* form"
  (should=
   '(a b c d)

   (emr--free-variables
    '(let* (x (y a))
       b
       (let* (z w)
         c
         (list d))))))

(check "finds free vars in lambda form"
  (should=
   '(a b c)

   (emr--free-variables
    '(lambda (x &rest y)
       a
       (list b)
       (lambda (z w)
         c)))))

(check "finds free vars in progn form"
  (should=
   '(a b c)

   (emr--free-variables
    '(progn
       a
       (lambda (x &rest y) b)
       (let (z w) c)))))

(check "finds free vars in destructuring-bind"
  (should=
   '(a b c d)

   (emr--free-variables
    '(destructuring-bind (x y) (list 1 2)
       a
       (list b)
       (cl-destructuring-bind (z . w) (list 3 4 5)
         (list c d))))))

(provide 'emr-elisp-tests)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; emr-elisp-tests.el ends here
