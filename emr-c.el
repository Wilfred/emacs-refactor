;;; emr-c.el --- Refactoring commands for C

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

;; Refactoring commands for C.

;;; Code:

(require 'emr)
(require 's)
(require 'dash)
(require 'thingatpt)
(require 'cc-cmds)
(autoload 'c-get-style-variables "cc-styles")

(defun emr-c:extract-above (desc str)
  "Insert STR above the current defun."
  (declare (indent 1))
  (save-excursion
    (emr-move-above-defun)
    (open-line 2)
    (emr-reporting-buffer-changes desc
      (insert str)
      (c-indent-defun))))

(defun emr-c:maybe-append-semicolon (str)
  (if (s-ends-with? ";" str)
      str
    (s-append ";" str)))

(defun emr-c:maybe-prepend-return (str)
  (if (s-matches? (rx bol (* space) "return") str)
      str
    (format "return %s" (s-trim str))))

(defun emr-c:add-return-statement (str)
  "Prepend a return statement to the last line of STR."
  (destructuring-bind (&optional last &rest rest)
      ;; Clean up lines and reverse for processing.
      (->> (s-split "\n" str)
        (-map 's-trim)
        (-drop-while 'emr-blank?)
        (nreverse)
        (-drop-while 'emr-blank?))
    ;; Reformat last line.
    (->> (-> last
           (emr-c:maybe-prepend-return)
           (emr-c:maybe-append-semicolon)
           (cl-list* rest)
           (nreverse))
      ;; Rejoin with left padding.
      (--map (concat (s-repeat tab-width " ") it))
      (s-join "\n"))))

(defun emr-c:spacing-before-function-curly ()
  "Use the current style to format the spacing between the arglist and body."
  (let* ((vars (and (boundp 'c-indentation-style)
                    (c-get-style-variables c-indentation-style nil)))
         (spaces
          (->> (assoc 'c-offsets-alist vars) (cdr)
               (assoc 'defun-open) (cdr)))
         (newlines
          (->> (assoc 'c-hanging-braces-alist vars) (cdr)
               (assoc 'defun-open) (cdr))))
    (concat (s-repeat (or spaces 0) " ")
            (s-repeat (or newlines 0) " "))))

(defun emr-c:format-function-usage (name arglist)
  (let ((args (->> (s-split "," arglist)
                (--map (-> (s-trim it) (split-string (rx (any space "*")) t) (cadr)))
                (s-join ", "))))
    (format "%s(%s)" name args)))

(defun emr-c:infer-type (str)
  "Try to infer the resulting type of a series of C statements."
  (let ((fst (->> (s-lines str) (last) (car)
                  (s-trim)
                  (s-split (rx space))
                  (car))))
    (if (s-matches? (rx bol (+ alnum) eol) fst)
        fst
      "void")))

;;;###autoload
(defun emr-c-extract-function (name return arglist)
  "Extract the current region as a new function.
* NAME is the name of the function to create.
* RETURN is the return type.
* ARGLIST is the argument list."
  (interactive
   (list
    ;; Read name, ensuring it is not blank.
    (let ((input (s-trim (read-string "Name: " nil t))))
      (if (s-blank? input)
          (user-error "Must enter a function name")
        input))

    ;; Try to infer the type from assignments in the region.
    (let* ((beg (save-excursion (goto-char (region-beginning))
                                (line-beginning-position)))
           (end (save-excursion (goto-char (region-end))
                                (line-end-position)))
           (type (emr-c:infer-type (buffer-substring beg end))))
      (-> (format "Return type (default: %s): " type)
        (read-string nil t type)
        (s-trim)))

    (s-trim (read-string "Arglist: "))))

  (atomic-change-group
    (kill-region (region-beginning) (region-end))

    ;; Insert usage. Place point inside opening brace.
    (unless (thing-at-point-looking-at (rx space))
      (just-one-space))
    (insert (emr-c:format-function-usage name arglist))
    (search-backward "(")
    (forward-char)
    ;; Tidy usage site.
    (save-excursion
      (search-forward ")")
      (when (s-ends-with? ";" (s-trim (car kill-ring)))
        (insert ";"))
      (when (s-ends-with? "}" (s-trim (thing-at-point 'line)))
        (insert "\n"))
      (indent-for-tab-command))

    ;; Insert function definition.
    (emr-c:extract-above "Extracted function"
      (format
       "%s %s(%s)%s{\n%s\n}"
       return name arglist
       (emr-c:spacing-before-function-curly)
       ;; Add a return statement if not a void function.
       (if (equal "void" return)
           (s-trim (car kill-ring))
         (emr-c:add-return-statement (car kill-ring)))))
    (setq kill-ring (cdr kill-ring))))

(defun emr-c:expr-start ()
  "Return either the start of the right side of an assignment, or
the start of the current statement."
  (interactive)
  (cl-flet ((max-safe (&rest xs) (apply 'max (--map (or it 0) xs))))
    (max-safe
     ;; Expr after `=` sign.
     (save-excursion
       (when (search-backward "=" (line-beginning-position) t)
         (forward-char)
         (search-forward-regexp (rx (not space)) nil t)
         (forward-char -1)
         (point)))
     ;; Expr after `;`. Ignores `;` at eol.
     (save-excursion
       (when (search-backward-regexp (rx ";" (* space) (+ nonl))
                                     nil t)
         (forward-char)
         (search-forward-regexp (rx (not space)) nil t)
         (forward-char -1)
         (point)))
     ;; Expr from start of line.
     (save-excursion
       (back-to-indentation)
       (point))

     (c-beginning-of-statement 1))))

;;;###autoload
(defun emr-c-extract-function-from-expression ()
  "Extract a function from right side of the assignment at point.
If there is no assignment, extract the whole line."
  (interactive)
  ;; Select the current C expression, then delegate to the proper
  ;; extraction function.
  (let (pos)
    (save-excursion
      (goto-char (emr-c:expr-start))
      (set-mark-command nil)
      (c-end-of-statement)
      (call-interactively 'emr-c-extract-function)
      (setq pos (point)))
    (goto-char pos)))

(defun emr-c:inside-curlies? ()
  "Non-nil if point is inside a pair of curly braces."
  (save-excursion
    ;; First move back. This ensures that thing-at-point will not return
    ;; true if we start just BEFORE a curly.
    (forward-char -1)
    (while (ignore-errors (paredit-backward-up) t))
    (thing-at-point-looking-at (rx "{"))))

(emr-declare-action emr-c-extract-function
  :title "function"
  :description "region"
  :modes c-mode
  :predicate (and (region-active-p)
                  (emr-c:inside-curlies?)))

(emr-declare-action emr-c-extract-function-from-expression
  :title "function"
  :description "expression"
  :modes c-mode
  :predicate (and (not (region-active-p))
                  (emr-c:inside-curlies?)))

(provide 'emr-c)

;; Local Variables:
;; lexical-binding: t
; End:

;;; emr-c.el ends here
