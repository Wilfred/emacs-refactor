;;; elr-elisp-tests --- Tests for elr-elisp

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

;; Tests for elr-elisp

;;; Code:

(require 'ert)
(require 'elr-elisp (expand-file-name "../elr-elisp.el"))

(defmacro check (desc &rest body)
  "Wrap `ert-deftest' with a simpler interface.
DESC is a string describing the test.
BODY lists the forms to be executed."
  (declare (indent 1))
  `(ert-deftest
       ,(intern (replace-regexp-in-string "[ .]" "_" desc)) ()
     ,@body))

(defvaralias 'nl 'elr--newline-token)

(defun should= (x y)
  "Assert X == Y."
  (should (equal x y)))

(check "reading single-line form does not change input"
  (should= '(input) (elr--read "(input)")))

(check "inserts newline tokens for multi-line forms"
  (should= `(line1 ,nl line2)
           (elr--read "(line1\nline2)")))

(check "printing form without newline tokens does not insert newlines"
  (should (not (string-match-p "\n" (elr--print '(single line form))))))

(check "printing form with newline tokens should insert newlines"
  (should= "(form\nwith\nnewlines)"
           (elr--print `(form ,nl with ,nl newlines))))

(provide 'elr-elisp-tests)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; elr-elisp-tests.el ends here
