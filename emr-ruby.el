;;; emr-ruby.el --- Refactoring support for Ruby.  -*- lexical-binding: t; -*-

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

;; Refactoring support for Ruby.  Requires `Ruby Refactor`.
;;
;;    https://github.com/ajvargo/ruby-refactor
;;

;;; Code:

(require 'emr)

(emr-declare-command 'ruby-refactor-extract-to-method
  :title "extract method"
  :description nil
  :modes '(enh-ruby-mode ruby-mode)
  :predicate (lambda ()
               (and (require 'ruby-refactor nil t)
                    (use-region-p))))

(emr-declare-command 'ruby-refactor-extract-local-variable
  :title "extract local variable"
  :description nil
  :modes '(enh-ruby-mode ruby-mode)
  :predicate (lambda ()
               (and (require 'ruby-refactor nil t)
                    (use-region-p))))

(emr-declare-command 'ruby-refactor-extract-constant
  :title "extract constant"
  :description nil
  :modes '(enh-ruby-mode ruby-mode)
  :predicate (lambda ()
               (and (require 'ruby-refactor nil t)
                    (use-region-p))))

(emr-declare-command 'ruby-refactor-add-parameter
  :title "add parameter"
  :description nil
  :modes '(enh-ruby-mode ruby-mode)
  :predicate (lambda ()
               (require 'ruby-refactor nil t)))

(emr-declare-command 'ruby-refactor-convert-post-conditional
  :title "convert post conditional"
  :description nil
  :modes '(enh-ruby-mode ruby-mode)
  :predicate (lambda ()
               (and (require 'ruby-refactor nil t)
                    (use-region-p))))

(emr-declare-command 'ruby-refactor-extract-to-let
  :title "extract to let"
  :description nil
  :modes '(enh-ruby-mode ruby-mode)
  :predicate (lambda ()
               (and (require 'ruby-refactor nil t)
                    (use-region-p))))

(provide 'emr-ruby)

;;; emr-ruby.el ends here
