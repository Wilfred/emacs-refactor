;;; emr-lisp.el --- Refactoring commands common to all Lisps.

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130604.0807

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

;; Refactoring commands common to all Lisps.

;;; Code:

(defun emr-lisp:back-to-open-round ()
  "Move to the opening paren for the Lisp list at point."
  (interactive)
  (when (or (not (equal "(" (thing-at-point 'char)))
            (emr-looking-at-string?))
    (beginning-of-sexp)
    (unless (equal "(" (thing-at-point 'char))
      (search-backward "("))))

(defun emr-lisp:back-to-open-round-or-quote ()
  "Move to the opening paren or quote for the Lisp list at point."
  (interactive)
  (emr-lisp:back-to-open-round)
  (when (or (thing-at-point-looking-at "'")
            (thing-at-point-looking-at "`")
            (emr-looking-at-string?))
    (search-backward-regexp (rx (or "'" "`")))))

;;;###autoload
(defun emr-lisp-comment-form ()
  "Comment out the current region or from at point."
  (interactive "*")
  (if (region-active-p)
      (comment-region (region-beginning)
                      (region-end))
    (emr-lisp:back-to-open-round-or-quote)
    (mark-sexp)
    (comment-region (region-beginning) (region-end))))

(emr-declare-action emr-lisp-comment-form
  :title "comment"
  :description "form"
  :modes
  (clojure-mode
   lisp-mode
   emacs-lisp-mode
   scheme-mode)
  :predicate (and (thing-at-point 'defun)
                  (not (emr-looking-at-comment?))))

(provide 'emr-lisp)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; emr-lisp.el ends here
