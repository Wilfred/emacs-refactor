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

(require 'emr)
(autoload 'thing-at-point-looking-at "thingatpt")
(autoload 'beginning-of-sexp "thingatpt")

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
  "Comment out the Lisp form at point."
  (interactive "*")
  (save-excursion
    (emr-lisp:back-to-open-round-or-quote)
    (mark-sexp)
    (comment-region (region-beginning) (region-end))))

;;;###autoload
(defun emr-lisp-uncomment-block ()
  "Uncomment the Lisp form or forms at point.

Searches the comment block for Lisp forms to avoid uncommenting
textual comments."
  (interactive "*")
  ;; Find start and end of commented form.
  ;;
  ;; In the first pass, we find the absolute bounds of the commented
  ;; form. We then narrow the bounds by searching for the start and end
  ;; of a Lisp list.
  (let* (
         ;; 1. Find the absolute end of the commented region.
         (end
          (save-excursion
            (while (save-excursion
                     (forward-line)
                     (or (emr-blank-line?)
                         (emr-line-matches?
                          (eval `(rx (* space) ,comment-start)))))
              (forward-line))
            (line-end-position)))

         ;; 2. Find the absolute start of the commented region.
         (beg
          (save-excursion
            (while (save-excursion
                     (forward-line -1)
                     (or (emr-blank-line?)
                         (emr-line-matches?
                          (eval `(rx (* space) ,comment-start)))))
              (forward-line -1))

            ;; 3. Restrict the starting position by finding the first open
            ;; paren in the comment block.
            (let ((list-start (eval `(rx (* space) ,comment-start (* space) "("))))
              (unless (emr-line-matches? list-start)
                (search-forward-regexp list-start end nil)))
            (line-beginning-position)))

         ;; 4. Restrict the ending position by finding the first closing
         ;; paren in the comment block.
         (end
          (save-excursion
            (goto-char end)
            (end-of-line)
            (let ((list-end (rx ")" (* space) eol)))
              (unless (emr-line-matches? list-end)
                (search-backward-regexp list-end beg nil)))
            (line-end-position)))
         )
    ;; `beg' and `end' should now roughly correspond to the start and end
    ;; of valid Lisp forms.
    (save-excursion
      (uncomment-region beg end))))

; ------------------

(emr-declare-action emr-lisp-comment-form
  :title "comment"
  :description "form"
  :modes
  (clojure-mode
   lisp-mode
   emacs-lisp-mode
   scheme-mode)
  :predicate (and (not (region-active-p))
                  (thing-at-point 'defun)
                  (not (or (emr-line-matches? (rx bol (* space) ";"))
                           (emr-looking-at-comment?)))))

(emr-declare-action emr-lisp-uncomment-block
    :title "uncomment"
    :description "block"
    :modes
    (clojure-mode
     lisp-mode
     emacs-lisp-mode
     scheme-mode)
    :predicate (and (not (region-active-p))
                    (emr-line-matches? (rx bol (* space) ";"))))

(provide 'emr-lisp)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; emr-lisp.el ends here
