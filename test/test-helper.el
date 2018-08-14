;;; test-utils.el --- Common utilities for emr tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Chris Barrett
;; Copyright (C) 2018 Wilfred Hughes

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

(require 'ert)
(require 'f)

(let ((emr-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path emr-dir))

(require 'undercover)
(undercover "emr*.el"
	    (:exclude "*-test.el")
	    (:report-file "/tmp/undercover-report.json"))

;;; test-helper.el ends here
