;;; emr-c-tests.el --- Tests for emr-c

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

;; Tests for emr-c

;;; Code:

(require 'ert)
(require 'test-utils (expand-file-name "./test-utils.el"))
(require 'emr (expand-file-name "../emr.el"))
(require 'emr-c (expand-file-name "../emr-c.el"))

(defmacro* check-in-buffer
    (desc function before _-> after &key (point-marker-char "|"))
  "Check that a given motion moves POINT to an expected position.
* BEFORE and AFTER are strings to compare.
* DESC is a description of the test.
* POINT-MARKER-CHAR is the character that will represent the
  position of point in BEFORE and AFTER strings."
  (declare (indent 1))
  (cl-assert (equal (length before) (length after)))
  `(check ,desc
     (with-temp-buffer
       ;; Do all sorts of wacky string replacement. I could have just compared
       ;; the position of point against the pipe character, but comparing
       ;; strings gives you much better error feedback in ERT.
       (insert ,before)
       ;; delete the point marker in BEFORE
       (goto-char (1+ (s-index-of ,point-marker-char ,before)))
       (delete-char 1)

       ,function

       ;; put a marker where we are now.
       (insert ,point-marker-char)
       ;; assert that the buffer now looks like AFTER.
       (should (equal ,after (buffer-string))))))

(check-in-buffer "expression start bounded by = sign"
  (goto-char (emr-c:expr-start))
  "int x = 20;|" -> "int x = |20;")

(check-in-buffer "expression start bounded by semicolon"
  (goto-char (emr-c:expr-start))
  "int x = 20; foo()|;" ->
  "int x = 20; |foo();" )

(check-in-buffer "expression start takes whole line"
  (goto-char (emr-c:expr-start))
  "int x = 20;\n foo()|;" ->
  "int x = 20;\n |foo();" )


(provide 'emr-c-tests)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; emr-c-tests.el ends here
