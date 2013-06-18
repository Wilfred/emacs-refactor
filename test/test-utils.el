;;; test-utils.el --- Common utilities for emr tests

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

;; Common utilities for emr tests

;;; Code:

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

(provide 'test-utils)

;; Local Variables:
;; lexical-binding: t
;; no-byte-compile: t
;; End:

;;; test-utils.el ends here
