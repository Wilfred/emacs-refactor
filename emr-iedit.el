;;; emr-iedit.el -- Use iedit for rename refactorings  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 EMR Project
;; Copyright (C) 2018 Wilfred Hughes

;; Author: YangYingchao <yangyingchao@gmail.com>

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
;; Rename variables/functions/macros with iedit.

;;; Code:

(require 'emr)
(require 's)
(require 'dash)
(require 'thingatpt)
(require 'which-func)
(require 'emr-elisp)

(require 'iedit)

(defconst emr-iedit:rx-iterator
  (rx (+ (or alnum "-" "_"))))

(defun emr-iedit:looking-at-iterator? ()
  (thing-at-point-looking-at emr-iedit:rx-iterator))

(defun emr-iedit-global ()
  "Rename a variable appears in current buffer.."
  (interactive)
  (iedit-mode))

(defun emr-iedit-in-function ()
  "Rename variable appears in current function."
  (interactive)
  (iedit-mode-toggle-on-function))


(emr-declare-command 'emr-iedit-in-function
  :title "rename"
  :description "in function"
  :modes '(prog-mode)
  :predicate (lambda ()
               (and (not (iedit-region-active))
                    (if (eq major-mode 'emacs-lisp-mode)
                        (emr-el:looking-at-local-var-p)
                      (and
                       (emr-iedit:looking-at-iterator?)
                       (which-function))))))

(emr-declare-command 'emr-iedit-global
  :title "rename"
  :description "in file"
  :modes '(prog-mode)
  :predicate (lambda ()
               (and (not (iedit-region-active))
                    (if (eq major-mode 'emacs-lisp-mode)
                        (and
                         (emr-el:looking-at-symbol-p)
                         (not (emr-el:looking-at-local-var-p)))
                      (emr-iedit:looking-at-iterator?)))))

(provide 'emr-iedit)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; emr-iedit.el ends here
