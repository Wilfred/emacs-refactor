;;; test-runner --- Runs emr tests.

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

;; Runs emr tests.

;;; Code:

(require 'cl)

;;; ----------------------------------------------------------------------------
;;; Load path processing.

(defun directory-p (f)
  "Test whether F is a directory.  Return nil for '.' and '..'."
  (and (file-directory-p f) (not (string-match "/[.]+$" f))))

(defun directory-subfolders (path)
  "Return a flat list of all subfolders of PATH."
  (unless (string-match-p ".git" path)
    (flatten
     (mapcar
      (lambda (d) (cons d (directory-subfolders d)))
      (remove-if-not 'directory-p (directory-files path t))))))

(defun flatten (ls)
  (cond ((null ls) nil)
        ((atom ls) (list ls))
        (t (append (flatten (car ls)) (flatten (cdr ls))))))

(defun add-tree-to-load-path (dir)
  (let ((dir (expand-file-name dir)))
    (mapc (lambda (s) (add-to-list 'load-path s))
          (cons dir (directory-subfolders dir)))))

;;; ----------------------------------------------------------------------------

(defun run-tests ()
  "Set up the environment and run unit tests."
  (message "Configuring load path...")
  (add-tree-to-load-path "../")
  (add-tree-to-load-path "./")

  (message "Requiring features...")
  (require 'ert)
  (require 'emr)
  (require 'emr-elisp)

  (message "Loading tests...")
  (require 'emr-elisp-tests)

  (message "Running tests...")
  (ert-run-tests-batch-and-exit nil))

(provide 'test-runner)

;; Local Variables:
;; lexical-binding: t
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; test-runner.el ends here
