;;; emr-js.el --- Refactoring commands for JavaScript.  -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Chris Barrett

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

;; Refactoring commands for JavaScript.  Requires `js2-refactor`.
;;
;;    https://github.com/magnars/js2-refactor.el
;;

;;; Code:

(require 'emr)

(emr-declare-command 'js2r-extract-function
  :title "extract to function"
  :description nil
  :modes 'js2-mode
  :predicate (lambda ()
               (and
                (require 'js2-refactor nil t)
                (use-region-p))))

(emr-declare-command 'js2r-extract-var
  :title "extract local variable"
  :description nil
  :modes 'js2-mode
  :predicate (lambda ()
               (and
                (require 'js2-refactor nil t)
                (use-region-p))))

(emr-declare-command 'js2r-var-to-this
  :title "local variable to instance variable"
  :description nil
  :modes 'js2-mode
  :predicate (lambda ()
               (and
                (require 'js2-refactor nil t)
                (use-region-p))))

(emr-declare-command 'js2r-log-this
  :title "log this"
  :description nil
  :modes 'js2-mode
  :predicate (lambda ()
               (and
                (require 'js2-refactor nil t)
                (or (use-region-p)
                    (ignore-errors (js2r--name-node-at-point))))))

(emr-declare-command 'js2r-introduce-parameter
  :title "add parameter"
  :description nil
  :modes 'js2-mode
  :predicate (lambda ()
               (and
                (require 'js2-refactor nil t)
                (use-region-p))))

(emr-declare-command 'js2r-extract-method
  :title "extract to method"
  :description nil
  :modes 'js2-mode
  :predicate (lambda ()
               (and
                (require 'js2-refactor nil t)
                (use-region-p))))

(provide 'emr-js)

;;; emr-js.el ends here
