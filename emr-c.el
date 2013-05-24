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

(autoload 'c-indent-defun "cc-cmds")
(autoload 'c-get-style-variables "cc-styles")

(defun emr-c:extract-above (desc str)
  "Insert STR above the current defun."
  (declare (indent 1))
  (save-excursion
    (beginning-of-defun)
    (while (emr-looking-at-comment?)
      (forward-line -1))
    (open-line 2)
    (emr-reporting-buffer-changes desc
      (insert str)
      (c-indent-defun))))

(defun emr-c:add-return-statement (str)
  "Prepend a return statement to the last line of STR."
  (destructuring-bind (&optional last &rest rest)
      ;; Clean up lines and reverse for processing.
      (->> (s-split "\n" str)
        (-map 's-trim)
        (-drop-while 'emr-blank?)
        (nreverse)
        (-drop-while 'emr-blank?))
    ;; Add return statement.
    (unless (and last (s-matches? (rx bol "return") last))
      (setq last (format "return %s" (s-trim last))))
    (unless (s-ends-with? ";" last)
      (setq last (s-append ";" last)))
    ;; Rejoin with left padding.
    (->> (cl-list* last rest)
      (nreverse)
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
                (-map 's-trim)
                (--map (car (last (s-split-words it))))
                (s-join ", "))))
    (format "%s(%s)" name args)))

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
    (s-trim (read-string "Return type (default: void): " nil t "void"))
    (s-trim (read-string "Arglist: "))))
  (cl-assert (region-active-p))

  (atomic-change-group
    (kill-region (region-beginning) (region-end))

    ;; Insert usage. Place point inside opening brace.
    (insert (emr-c:format-function-usage name arglist))
    (search-backward "(")
    (forward-char)

    ;; Insert a semicolon and newline if there's nothing else on this line.
    (save-excursion
      (search-forward ")")
      (when (s-matches? (rx (* space) "}" eol)
                        (buffer-substring (point) (line-end-position)))
        (insert ";\n"))
      (indent-for-tab-command))

    ;; Insert declaration.
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

(emr-declare-action emr-c-extract-function c-mode "function"
  :predicate (region-active-p))

(provide 'emr-c)

;; Local Variables:
;; lexical-binding: t
; End:

;;; emr-c.el ends here
