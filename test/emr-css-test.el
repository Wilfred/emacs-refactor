;;; emr-elisp-test --- Tests for emr-elisp  -*- lexical-binding: t; -*-

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

;; Tests for emr-css

;;; Code:

(require 'emr-css)
(require 'test-helper)

(check "css--adds !important"
  (with-temp-buffer
    (css-mode)
    (insert "display: block;")
    (emr-css-toggle-important)
    (should=
     (buffer-string)
     "display: block !important;")))

(check "css--removes !important"
  (with-temp-buffer
    (css-mode)
    (insert "display: block !important;")
    (emr-css-toggle-important)
    (should=
     (buffer-string)
     "display: block;")))

(provide 'emr-css-test)

;;; emr-css-test.el ends here
