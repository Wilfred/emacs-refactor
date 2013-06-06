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
(require 'test-utils (expand-file-name "./test-utils.el"))
(require 'emr (expand-file-name "../emr.el"))
(require 'emr-elisp (expand-file-name "../emr-elisp.el"))

;;;; Function implementation.

(check "uses symbol names when inferring arglists from callsites"
  (let ((fname (cl-gensym)))
    (should=
     '(x y)
     (emr-el:infer-arglist-for-usage `(,fname x y)))))

(check "uses argn for non-symbol names when inferring arglists from callsites"
  (should=
   '(arg1 arg2)
   (emr-el:infer-arglist-for-usage '(hello 9 8))))

;;;; Bound variables

(check "finds free vars in let form"
  (should=
   '(a b c d)

   (emr-el:free-variables
    '(let (x (y a))
       b
       (let (z w)
         c
         (list d))))))

(check "finds free vars in let* form"
  (should=
   '(a b c d)

   (emr-el:free-variables
    '(let* (x (y a))
       b
       (let* (z w)
         c
         (list d))))))

(check "finds free vars in lambda form"
  (should=
   '(a b c)

   (emr-el:free-variables
    '(lambda (x &rest y)
       a
       (list b)
       (lambda (z w)
         c)))))

(check "finds free vars in progn form"
  (should=
   '(a b c)

   (emr-el:free-variables
    '(progn
       a
       (lambda (x &rest y) b)
       (let (z w) c)))))

(check "finds free vars in destructuring-bind"
  (should=
   '(a b c d)

   (emr-el:free-variables
    '(destructuring-bind (x y) (list 1 2)
       a
       (list b)
       (cl-destructuring-bind (z . w) (list 3 4 5)
         (list c d))))))

(check "finds free vars in defun form"
  (should=
   '(a b)

   (emr-el:free-variables
    '(defun hello (x y)
       (list a x)
       (progn
         (list b y))))))

(check "survives function symbol followed by non-lambda term"
  (let ((fname (cl-gensym)))
    (should=
     `(,fname)

     (emr-el:free-variables
      `(function ,fname)))))

(check "checks outer scope for bindings that share names with functions"
  (should=
   '(message y)

   (emr-el:free-variables '(funcall message y)
                        '(let (message)
                           (funcall message y)))))

;;;; Commands

(defmacro* check-command (desc before command-form _-> after
                               &key (point-marker "|"))
  "Check that a given refactoring command has an expect result.

* BEFORE and AFTER are strings to compare.

* DESC is a description of the test.

* POINT-MARKER is the character that will represent the position
  of point in BEFORE and AFTER strings."
  (declare (indent 1))
  (assert (stringp desc))
  (assert (stringp before))
  (assert (listp command-form))
  (assert (stringp after))
  (assert (stringp point-marker))
  (assert (s-contains? point-marker before))
  `(check ,(concat "check command: " desc)
     (with-temp-buffer
       (lisp-mode)
       ;; Do all sorts of wacky string replacement. I could have just compared
       ;; the position of point against the pipe character, but comparing
       ;; strings gives you much better error feedback in ERT.
       (save-excursion (insert ,(s-trim before)))
       ;; delete the point marker in BEFORE
       (search-forward ,point-marker)
       (delete-char -1)
       ;; Perform the refactoring command.
       ,command-form

       ;; Remove text properties from result.
       (let ((expected (rx ,(s-trim after)))
             (result (s-trim (buffer-string))))
         (set-text-properties 0 (length result) nil result)
         (set-text-properties 0 (length expected) nil expected)

         ;; assert that the buffer now looks like AFTER.
         (should (s-matches? expected result))))))

(check-command "inline variable - defvar"
  "
  (defvar x| value)

  x

  (application x)"
  (emr-el-inline-variable) ->
  "
  value

  (application value)")

(check-command "inline variable - defconst"
  "
  (defconst x| value)

  x

  (application x)"
  (emr-el-inline-variable) ->
  "
  value

  (application value)")

(check-command "eval-and-replace at top level"
  "(+ 1 2)|"
  (emr-el-eval-and-replace) ->
  "3")

(check-command "eval-and-replace inside other forms"
  "(+ (+ 1 2)| 3)"
  (emr-el-eval-and-replace) ->
  "(+ 3 3)")

(check-command "extract function at top level"
  "
(defun orig (x)
  (application| x))"
  (emr-el-extract-function "extracted" '(x)) ->
  "
(defun extracted (x)
  (application x))

(defun orig (x)
  (extracted x))")

(provide 'emr-elisp-tests)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; emr-elisp-tests.el ends here
