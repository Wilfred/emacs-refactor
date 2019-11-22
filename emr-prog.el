;;; emr-prog.el --- Common refactoring commands for all programming modes.  -*- lexical-binding: t; -*-

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

;; Common refactoring commands for all programming modes.

;;; Code:

(require 'emr)

(emr-declare-command 'comment-region
  :title "comment"
  :description "region"
  :modes 'prog-mode
  :predicate (lambda ()
               (region-active-p)))

(emr-declare-command 'uncomment-region
  :title "uncomment"
  :description "region"
  :modes 'prog-mode
  :predicate (lambda ()
               (and (region-active-p)
                    (s-contains? comment-start
                                 (buffer-substring (region-beginning)
                                                   (region-end))))))

(provide 'emr-prog)

;;; emr-prog.el ends here
