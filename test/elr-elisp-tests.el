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
(require 'elr (expand-file-name "../elr.el"))
(require 'elr-elisp (expand-file-name "../elr-elisp.el"))

(defmacro check (desc &rest body)
  "Wrap `ert-deftest' with a simpler interface.
DESC is a string describing the test.
BODY lists the forms to be executed."
  (declare (indent 1))
  `(ert-deftest
       ,(intern (replace-regexp-in-string "[ .]" "_" desc)) ()
     ,@body))

(defun should= (x y)
  (should (equal x y)))

(defun should-match (regex str)
  (should (string-match-p regex str)))

;;; ----------------------------------------------------------------------------

(check "reading single-line form does not change input"
  (should= '(input) (elr--read "(input)")))

(check "can read and print Lisp forms on one line"
  (let ((line "(list 1 2 3 4)"))
    (should= (elr--print (elr--read line))
             line)))

(check "preserves line-endings"
  (let ((line "(list\n1)"))
    (should (string-match-p "\n" (elr--print (elr--read line))))))

(check "preserves whole-line comments"
  (let* ((expr    "(list \n ;;; Comment \n )")
         (output  (elr--print (elr--read expr)))
         (line    (nth 1 (split-string output "\n"))))
    (should-match (rx bol (* space) ";;; Comment" (* space) eol)
                  line)))

(check "preserves eol-line comments"
  (let* ((expr    "(list ;;; Comment \n )")
         (output  (elr--print (elr--read expr)))
         (line    (car (split-string output "\n"))))
    (should-match (rx  "(list "(* space) ";;; Comment" (* space) eol)
                  line)))

(provide 'elr-elisp-tests)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; elr-elisp-tests.el ends here
