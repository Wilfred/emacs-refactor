;;; emr-scheme.el --- Refactoring commands for Scheme.  -*- lexical-binding: t; -*-

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

;; Refactoring commands for Scheme.

;;; Code:

(require 'emr)
(require 'emr-lisp)
(require 'emr-elisp)
(require 'dash)

(defun emr-scm:looking-at-def? ()
  "Non-nil if point is looking at a definition form."
  (emr-line-matches? (rx bol (* space) "(define" (+ space))))

(defun emr-scm:inside-def? ()
  "Non-nil if point is inside a definition form."
  (and
   (emr-lisp-find-upwards 'define)
   (not (emr-scm:looking-at-def?))))

;;;###autoload
(defun emr-scm-extract-function (name arglist)
  "Extract a function, using the current region or form at point as the body.
NAME is the name of the new function.
ARGLIST is its argument list."
  (interactive
   (list
    ;; Read a name for the function, ensuring it is not blank.
    (let ((x (read-string "Name: ")))
      (if (s-blank? x) (user-error "Name must not be blank") x))

    ;; Prompt user with default arglist.
    (read-string "Arglist: ")))

  (cl-assert (not (s-blank? name)) () "Name must not be blank")

  (emr-lisp-extraction-refactor (body) "Extracted to"
    (let ((hdr (cons (intern name)
                     (emr-el:safe-read (format "(%s)" arglist)))))

      ;; Insert usage at point.
      (insert (emr-el:print hdr))

      ;; Insert definition.
      (->> (format "(define %s\n  %s)" hdr body)
        (emr-el:format-defun)
        (emr-lisp-insert-above-defun)))))

;;;###autoload
(defun emr-scm-extract-variable (name)
  "Extract the current region or form at point to a special variable.
The variable will be called NAME."
  (interactive "*sName: ")
  (cl-assert (not (s-blank? name)) () "Name must not be blank")
  (emr-lisp-extraction-refactor (sexp) "Extracted to"
    ;; Insert usage.
    (insert (s-trim name))
    ;; Insert definition.
    (emr-lisp-insert-above-defun (format "(define %s %s)" name sexp))))

; ------------------

(emr-declare-command 'emr-scm-extract-function
  :title "function"
  :description "define"
  :modes 'scheme-mode
  :predicate (lambda ()
               (not (or (emr-scm:looking-at-def?)
                        (emr-el:looking-at-let-binding-symbol?)))))

(emr-declare-command 'emr-scm-extract-variable
  :title "variable"
  :description "define"
  :modes 'scheme-mode
  :predicate (lambda ()
               (not (or (emr-scm:looking-at-def?)
                        (emr-el:looking-at-let-binding-symbol?)))))

(provide 'emr-scheme)

;;; emr-scheme.el ends here
