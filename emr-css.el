;;; emr-css.el --- Refactoring commands for CSS  -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk

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

;; Refactoring commands for CSS.

;;; Code:

(require 'emr)
(require 's)
(require 'dash)
(require 'thingatpt)

;;;###autoload
(defun emr-css-toggle-important ()
  "Add or remove !important on the property at point."
  (interactive "*")
  (save-excursion
    (end-of-line)
    (backward-char 1)
    (if (looking-back "!important")
        (delete-char (- (length " !important")))
      (insert " !important"))))

(provide 'emr-css)

;;; emr-css.el ends here
