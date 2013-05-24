;;; emr-c.el --- Refactorings for C

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

;; Refactorings for C

;;; Code:

(require 'emr)

(defun emr-c:extract-above (desc str)
  "Insert STR above the current defun."
  (declare (indent 1))
  (save-excursion
    (beginning-of-defun)
    (open-line 2)
    (emr-reporting-buffer-changes desc
      (insert str))))

(defun emr-c:blank? (str)
  (or (s-blank? str)
      (s-matches? (rx bol (* space) eol) str)))

(defun emr-c:add-return-statement (str)
  "Prepend a return statement to the last line of STR."
  (destructuring-bind (&optional last &rest rest)
      ;; Clean up lines and reverse for processing.
      (->> (s-split "\n" str)
        (-map 's-trim)
        (-drop-while 'emr-c:blank?)
        (nreverse)
        (-drop-while 'emr-c:blank?))
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

;;;###autoload
(defun emr-c-extract-function (name return arglist)
  "Extract the current region as a new function.
* NAME is the name of the function to create.
* RETURN is the return type.
* ARGLIST is the argument list."
  (interactive
   (list
    (s-trim (read-string "Name: " nil t "fn"))
    (s-trim (read-string "Return type: " nil t "void"))
    (s-trim (read-string "Arglist: "))))
  (cl-assert (region-active-p))
  (atomic-change-group
    (kill-region (region-beginning) (region-end))
    ;; Insert usage.
    (insert (format "%s()" name))
    (when (save-excursion
            (goto-char (region-end))
            (emr-c:blank? (buffer-substring (point) (line-end-position))))
      (insert ";\n"))

    (indent-for-tab-command)
    ;; Insert declaration.
    (emr-c:extract-above "Extracted function"
      (format
       "%s %s(%s){\n%s\n}"
       return name arglist
       ;; Add a return statement if not a void function.
       (if (equal "void" return)
           (car kill-ring)
         (emr-c:add-return-statement (car kill-ring)))))
    (setq kill-ring (cdr kill-ring))))

(emr-declare-action emr-c-extract-function c-mode "function"
  :predicate (region-active-p))

(provide 'emr-c)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; emr-c.el ends here
