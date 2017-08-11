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

(check "elisp--uses symbol names when inferring arglists from callsites"
  (let ((fname (cl-gensym)))
    (should=
     '(x y)
     (emr-el:infer-arglist-for-usage `(,fname x y)))))

(check "elisp--uses argn for non-symbol names when inferring arglists from callsites"
  (should=
   '(arg1 arg2)
   (emr-el:infer-arglist-for-usage '(hello 9 8))))

;;;; Bound variables

(check "elisp--finds free vars in let form"
  (should=
   '(a b c d)

   (emr-el:free-variables
    '(let (x (y a))
       b
       (let (z w)
         c
         (list d))))))

(check "elisp--quoted vars are not free variables"
  (should=
   '(x)

   (emr-el:free-variables
    '(list x 'y))))

(check "elisp--finds free vars in let* form"
  (should=
   '(a b c d)

   (emr-el:free-variables
    '(let* (x (y a))
       b
       (let* (z w)
         c
         (list d))))))

(check "elisp--finds free vars in lambda form"
  (should=
   '(a b c)

   (emr-el:free-variables
    '(lambda (x &rest y)
       a
       (list b)
       (lambda (z w)
         c)))))

(check "elisp--finds free vars in progn form"
  (should=
   '(a b c)

   (emr-el:free-variables
    '(progn
       a
       (lambda (x &rest y) b)
       (let (z w) c)))))

(check "elisp--finds free vars in cl-destructuring-bind"
  (should=
   '(a b c d)

   (emr-el:free-variables
    '(cl-destructuring-bind (x y) (list 1 2)
       a
       (list b)
       (cl-destructuring-bind (z . w) (list 3 4 5)
         (list c d))))))

(check "elisp--finds free vars in defun form"
  (should=
   '(a b)

   (emr-el:free-variables
    '(defun hello (x y)
       (list a x)
       (progn
         (list b y))))))

(check "elisp--survives function symbol followed by non-lambda term"
  (let ((fname (cl-gensym)))
    (should=
     `(,fname)

     (emr-el:free-variables
      `(function ,fname)))))

(check "elisp--checks outer scope for bindings that share names with functions"
  (should=
   '(message y)

   (emr-el:free-variables '(funcall message y)
                        '(let (message)
                           (funcall message y)))))

;;;; Inspecting forms at point

(check "elisp--a top level let is not a definition"
  (with-temp-buffer
    (insert "(let ((x 1))\n  (message \"foo: %s %s\" x 'bar))")
    (goto-char (point-min))
    (search-forward "message")
    (should (not (emr-el:looking-at-definition?)))))

;;;; Commands

(defstruct emr-el-test-spec form before after)

(defun emr-el-test:example-call-from-docstring (str)
  "Extract the function usage form from a docstring test spec."
  (with-temp-buffer
    (insert str)
    (let ((beg (save-excursion (goto-char (point-min))
                               (search-forward-regexp (rx bol "EXAMPLE:"))))
          (end (save-excursion (goto-char (point-max))
                               (search-backward "BEFORE:"))))

      (s-trim (buffer-substring beg end)))))

(defun emr-el-test-spec-from-docstring (str)
  "Parse STR for a test spec.
Returns a cons where the car is the BEFORE state and the cdr is
the AFTER state."
  ;; Extract the test usage.
  (-if-let (form (emr-el-test:example-call-from-docstring str))
    ;; Extract the BEFORE and AFTER states to test.
    (cl-destructuring-bind (_ spec)
        (s-split (rx bol "BEFORE:") str)
      (cl-destructuring-bind (before after)
          (s-split (rx bol "AFTER:") spec)
        (make-emr-el-test-spec
         :form   (read form)
         :before (s-trim before)
         :after  (s-trim after))))

    (error "No test form in STR.")))

(defun emr-el-tests:remove-indentation (str)
  (->> (s-trim str) (s-split "\n") (-map 's-trim) (s-join "\n")))

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
  (let ((docstring (documentation fname)))
    ;; Perform some basic expansion-time checking.
    (assert (not (s-blank? docstring)))
    (assert (s-contains? "EXAMPLE:" docstring))
    (assert (s-contains? "BEFORE:" docstring))
    (assert (s-contains? "AFTER:" docstring))
    (assert (s-contains? "|" docstring)))

  `(check ,(format "elisp--%s" fname)
     ;; `documentation' returns the functions docstring concatenated with
     ;; its arglist. Remove the arglist.
     (let ((docstring (->> (documentation ',fname)
                        (s-trim)
                        (s-lines)
                        (reverse)
                        (-drop 1)
                        (reverse)
                        (s-join "\n"))))

       ;; Basic sanity checks before running.
       (assert (not (s-blank? docstring)))
       (assert (s-contains? "EXAMPLE:" docstring))
       (assert (s-contains? "BEFORE:" docstring))
       (assert (s-contains? "AFTER:" docstring))
       (assert (s-contains? "|" docstring))
       (let ((spec (emr-el-test-spec-from-docstring docstring)))
         (assert (s-contains? "|" (emr-el-test-spec-before spec)))

         (with-temp-buffer
           ;; Insert the BEFORE state from the spec into the buffer, then perform
           ;; the refactor command.
           (lisp-mode)
           (save-excursion
             (insert (s-trim (emr-el-test-spec-before spec)))
             (indent-region (point-min) (point-max)))
           ;; Move to position.
           (search-forward "|")
           (delete-char -1)
           (eval (emr-el-test-spec-form spec))

           ;; Remove leading indentation - the forms inside the docstrings are
           ;; probably indented for aesthetics.
           (let ((expected (eval `(rx ,(emr-el-tests:remove-indentation
                                        (emr-el-test-spec-after spec)))))
                 (result (emr-el-tests:remove-indentation (buffer-string))))
             ;; Remove text properties from result.
             (set-text-properties 0 (length result) nil result)
             (set-text-properties 0 (length expected) nil expected)

             (should (s-matches? expected result))))))))

(gentest-from-docstring emr-el-inline-variable)
(gentest-from-docstring emr-el-eval-and-replace)
(gentest-from-docstring emr-el-extract-function)
(gentest-from-docstring emr-el-extract-constant)
(gentest-from-docstring emr-el-extract-variable)
(gentest-from-docstring emr-el-insert-autoload-directive)

(provide 'emr-elisp-test)

;;; emr-elisp-test.el ends here
