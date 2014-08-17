;;; emr-lisp.el --- Refactoring commands common to all Lisps.  -*- lexical-binding: t; -*-

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

(defun emr-lisp-back-to-open-round ()
  "Move to the opening paren for the Lisp list at point."
  (interactive)
  (when (or (not (equal "(" (thing-at-point 'char)))
            (emr-looking-at-string?))
    (beginning-of-sexp)
    (unless (equal "(" (thing-at-point 'char))
      (search-backward "("))))

(defun emr-lisp-back-to-open-round-or-quote ()
  "Move to the opening paren or quote for the Lisp list at point."
  (interactive)
  (emr-lisp-back-to-open-round)
  (when (or (thing-at-point-looking-at "'")
            (thing-at-point-looking-at "`")
            (emr-looking-at-string?))
    (search-backward-regexp (rx (or "'" "`")))))

(defun emr-lisp-find-upwards (sym)
  "Search upwards from POINT for an enclosing form beginning with SYM."
  (save-excursion
    (cl-loop
     while (ignore-errors (backward-up-list) t)
     when (thing-at-point-looking-at
           (eval `(rx "(" ,(format "%s" sym) symbol-end)))
     do (return (point)))))

(defun emr-lisp-peek-back-upwards ()
  "Return the car of the enclosing form."
  (save-excursion
    (when (ignore-errors (backward-up-list) t)
      (forward-char 1)
      (sexp-at-point))))

; ------------------

(defun emr-lisp-reindent-defun ()
  "Reindent the current top level form."
  (save-excursion (end-of-defun) (beginning-of-defun) (indent-sexp)))

(defun emr-lisp-reindent-string (form-str)
  "Reformat FORM-STR, assuming it is a Lisp fragment."
  (with-temp-buffer
    (lisp-mode-variables)
    (insert form-str)
    (emr-lisp-reindent-defun)
    (buffer-string)))

(defun emr-lisp-insert-above-defun (form-str)
  "Insert and indent FORM-STR above the current top level form.
Return the position of the end of FORM-STR."
  (emr-insert-above-defun (emr-lisp-reindent-string form-str)))

(cl-defmacro emr-lisp-extraction-refactor ((&optional binding) description &rest body)
  "Kill the sexp near point then execute forms.
BINDING is the name to bind to the extracted form.
DESCRIPTION is used to report the result of the refactoring.
BODY is a list of forms to execute after extracting the sexp near point."
  (declare (indent 2))
  `(atomic-change-group
     (save-excursion

       ;; Either extract the active region or the sexp near point.
       (if (region-active-p)
           (kill-region (region-beginning) (region-end))
         (emr-lisp-back-to-open-round-or-quote)
         (kill-sexp))

       (emr-lisp-reindent-defun)

       (let
           ;; Define BINDING if supplied.
           ,(when binding `((,binding (s-trim (car kill-ring)))))

         ;; Revert kill-ring pointer.
         (setq kill-ring (cdr kill-ring))
         (save-excursion
           (emr-reporting-buffer-changes ,description
             ,@body))))))

; ------------------

;;;###autoload
(defun emr-lisp-comment-form ()
  "Comment out the Lisp form at point."
  (interactive "*")
  (save-excursion
    (emr-lisp-back-to-open-round-or-quote)
    (mark-sexp)
    (comment-region (region-beginning) (region-end))))

(defun emr-lisp:start-of-comment-block ()
  "Move to the start of the current comment block, ignoring blank lines."
  (while (save-excursion
           (forward-line -1)
           (unless (bobp)
             (or (emr-blank-line?)
                 (emr-line-matches?
                  (eval `(rx (* space) ,comment-start))))))
    (forward-line -1)))

(defun emr-lisp:end-of-comment-block ()
  "Move to the end of the current comment block,
ignoring blank lines."
  (while
      (save-excursion
        (forward-line)
        (unless (eobp)
          (or (emr-blank-line?)
              (emr-line-matches?
               (eval `(rx (* space) ,comment-start))))))
    (forward-line)))

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
  (condition-case _
      (let* (
             ;; 1. Find the absolute end of the commented region.
             (end
              (save-excursion
                (emr-lisp:end-of-comment-block)
                (line-end-position)))

             ;; 2. Find the absolute start of the commented region.
             (beg
              (save-excursion
                (emr-lisp:start-of-comment-block)

                ;; 3. Restrict the starting position by finding the first
                ;; opening delimiter in the comment block.
                (let ((list-start
                       (eval `(rx
                               (* space) ,comment-start (* space)
                               (* (or "'" "`" "#"))
                               (or "[" "{" "(" "\"")))))
                  (unless (emr-line-matches? list-start)
                    (search-forward-regexp list-start end nil)))
                (line-beginning-position)))

             ;; 4. Restrict the ending position by finding the first closing
             ;; delimiter in the comment block.
             (end
              (save-excursion
                (goto-char end)
                (end-of-line)
                (let ((list-end
                       (rx (or ")" "]" "}" "\"")
                           (* space) eol)))
                  (unless (emr-line-matches? list-end)
                    (search-backward-regexp list-end beg nil)))
                (line-end-position)))
             )
        ;; `beg' and `end' should now roughly correspond to the start and end
        ;; of valid Lisp forms.
        (save-excursion
          (uncomment-region beg end)))

    ;; If the regex searches bail, this is not a valid comment block.
    (error
     (user-error "Unable to find Lisp forms in comment block"))))

; ------------------

(emr-declare-command 'emr-lisp-comment-form
  :title "comment"
  :description "form"
  :modes '(clojure-mode lisp-mode emacs-lisp-mode scheme-mode)
  :predicate (lambda ()
               (and (not (region-active-p))
                    (thing-at-point 'defun)
                    (not (or (emr-line-matches? (rx bol (* space) ";"))
                             (emr-looking-at-comment?))))))

(emr-declare-command 'emr-lisp-uncomment-block
  :title "uncomment"
  :description "block"
  :modes '(clojure-mode lisp-mode emacs-lisp-mode scheme-mode)
  :predicate (lambda ()
               (and (not (region-active-p))
                    (emr-line-matches? (rx bol (* space) ";")))))

(provide 'emr-lisp)

;;; emr-lisp.el ends here
