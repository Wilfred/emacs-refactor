;;; elr-elisp --- Functions for working with Elisp.

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

;; Functions for working with Elisp.

;;; Code:

(require 'cl-lib)

(defconst elr--newline-token :elr--newline)

;;; ----------------------------------------------------------------------------
;;; Navigation commands

(defun elr--goto-first-match (regex)
  "Move point to the first match in the buffer for REGEX."
  (save-match-data
    (when (string-match regex (buffer-string) 0)
      (goto-char (match-beginning 0)))))

(defun elr--insert-above (str)
  "Insert STR at the line above."
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (back-to-indentation)
  (insert str)
  (newline)
  str)

(defun elr--goto-open-round ()
  "Move to the opening paren for the Lisp list at point."
  (interactive)
  (unless (equal "(" (thing-at-point 'char))
    (beginning-of-sexp)
    (unless (equal "(" (thing-at-point 'char))
      (search-backward "("))))

(defun elr--goto-open-round-or-quote ()
  "Move to the opening paren or quote for the Lisp list at point."
  (interactive)
  (elr--goto-open-round)
  (when (thing-at-point-looking-at "'")
    (search-backward "'")))

;;; ----------------------------------------------------------------------------
;;; Read and write lisp forms.

(defun elr--read (str)
  "Read the given string STR, inserting tokens to represent whitespace."
  (read (replace-regexp-in-string
         "\n"
         (format " %s " elr--newline-token)
         str)))

(defun elr--print (form)
  "Print FORM, replacing whitespace tokens with newlines."
  (replace-regexp-in-string (eval `(rx (* space) ,(symbol-name elr--newline-token) (* space)))
                            "\n"
                            (with-output-to-string (princ form))))

;;; ----------------------------------------------------------------------------
;;; Formatting commands

(defun elr--symbol-file-name (fn)
  "Find the name of the file that declares function FN."
  (when-let (file (find-lisp-object-file-name fn (symbol-function fn)))
    (and (stringp file)
         (file-name-nondirectory (file-name-sans-extension file)))))

(defun elr--list-at-point ()
  "Return the Lisp list at point or enclosing point."
  (interactive)
  (save-excursion
    (elr--goto-open-round)
    (mark-sexp 1 nil)
    (read (buffer-substring-no-properties (region-beginning)
                                          (region-end)))))

(defun elr--format-defun (symbol name arglist body)
  "Format a defun-style form.  All vars should be strings.
SYMBOL is the name for the car of the list (eg defun, defmacro)
NAME is name being defined.
ARGLIST is the arglist for the definition.
BODY is the formatted body forms."
  (with-temp-buffer
    (lisp-mode-variables)
    (insert (format "(%s %s (%s) \n%s)" symbol name arglist body))
    (indent-region (point-min) (point-max))
    (buffer-string)))

(defun elr--format-list (&rest args)
  (concat "(" (s-trim (s-join " " args)) ")"))

(defun elr--global-var? (sym)
  (let ((s (symbol-name sym)))
    (or (s-contains? "--" s)
        (s-contains? "/" s))))

(defvar elr--special-symbols '(&rest &optional &key &allow-other-keys \,\@ \,)
  "A list of symbols that should be ignored by variable searches.")

;;; FIXME:
;;; This needs more work to make it more accurate.
(defun elr--unbound-symbols (form)
  "Try to find the symbols in FORM that do not have variable bindings."
  (->> (cl-list* form)
    (-flatten)
    (-filter 'symbolp)
    (--remove
     (or (symbol-function it)
         (booleanp it)
         (keywordp it)
         (elr--global-var? it)
         (-contains? elr--special-symbols it)))
    (-uniq)))

(defun elr--unbound-symbols-string ()
  "Format a string of the unbound symbols in the list at point."
  (->> (elr--list-at-point)
    (elr--unbound-symbols)
    (-map 'symbol-name)
    (s-join " ")
    (s-trim)))

;;; ----------------------------------------------------------------------------
;;; Refactoring Commands

(defmacro elr--extraction-refactor (description &rest body)
  "Kill the sexp near point then execute BODY forms.
The kill ring is reverted at the end of the body."
  (declare (indent 1))
  `(save-excursion
     (elr--goto-open-round-or-quote)
     (kill-sexp)
     (unwind-protect
         (save-excursion
           (elr--reporting-buffer-changes ,description
             ,@body))
       ;; Revert kill-ring pointer.
       (setq kill-ring (cdr kill-ring)))))

(defun elr--line-visible? (line)
  "Return true if LINE is within the visible bounds of the current window."
  (let* ((min (line-number-at-pos (window-start)))
         (max (line-number-at-pos (window-end))))
    (and (>= line min) (<= line max))))

(defmacro elr--reporting-buffer-changes (description &rest body)
  "Execute forms producing an effect described by DESCRIPTION.
Report the changes made to the buffer at a result of executing BODY forms."
  (declare (indent 1))
  `(let ((before-changes (buffer-string)))
     ,@body
     ;; Report changes.
     (when-let (diff (and elr-report-actions
                          (car (elr--diff-lines before-changes (buffer-string)))))
       (destructuring-bind (_ . (line . text)) diff
         (unless (elr--line-visible? line)
           (elr--report-action ,description line text))))))

;;; ----------------------------------------------------------------------------
;;; Sexp-type tests

(defun elr--macro-definition? (sexp)
  "Return t if SEXP expands to a macro definition."
  (ignore-errors
    (let ((exp (macroexpand-all sexp)))
      ;; yo dawg I herd you like cars
      (and (equal 'defalias (car exp))
           (equal 'macro (cadar (cdaddr exp)))))))

(defun elr--function-definition? (sexp)
  "Return t if SEXP expands to a function definition."
  (ignore-errors
    (let ((exp (macroexpand-all sexp)))
      (and (equal 'defalias (car exp))
           (equal 'function (caaddr exp))))))

(defun elr--variable-definition? (sexp)
  (ignore-errors
    (member (car (macroexpand-all sexp))
            '(defconst defvar defcustom))))

(defun elr--looking-at-definition? ()
  (let ((sexp (elr--list-at-point)))
    (or (elr--variable-definition? sexp)
        (elr--macro-definition? sexp)
        (elr--function-definition? sexp))))

;;; ----------------------------------------------------------------------------
;;; Inlining

(defun elr--extract-var-values (sexp)
  "Return the name and initializing value of SEXP if it is a variable definition."
  (let ((exp (macroexpand-all sexp)))
    (when (elr--variable-definition? exp)
      (cl-destructuring-bind (_def sym &rest forms) exp
          (cons sym (car forms))))))

(cl-defun elr--replace-usages ((sym . value))
  "Replace all instances of SYM with VALUE in the current buffer.
Returns a list of lines where changes were made."
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (let ((match-sym (eval `(rx (not (any "(")) (* space) (group ,(symbol-name sym)) word-end)))
            (lines))
        ;; Check for "(" since we don't want to replace function calls.
        (while (search-forward-regexp match-sym nil t)
          (setq lines (cons (line-number-at-pos) lines))
          ;; Perform replacement.
          (replace-match (pp-to-string value) t nil nil 1)
          ;; Try to pretty-format.
          (save-excursion (end-of-defun) (beginning-of-defun) (indent-sexp)))
        (nreverse lines)))))

(defun elr-inline-variable ()
  "Inline the variable at defined at point.
Uses of the variable are replaced with the initvalue in the variable definition."
  (interactive)
  (save-excursion
    (elr--goto-open-round)
    (if-let (vals (elr--extract-var-values (elr--list-at-point)))
      (if (> (length vals) 1)
          (elr--extraction-refactor "Inlining applied at"

            ;; Clean up line spacing.
            (while (s-blank? (buffer-substring-no-properties (line-beginning-position)
                                                             (line-end-position)))
              (kill-line))

            ;; Perform inlining.
            ;; elr--extraction-refactor will report the first insertion. If
            ;; there are none or more than one insertion, override this report.
            (if-let (lines (-map 'int-to-string (elr--replace-usages vals)))
              (when (> (length lines) 1)
                (message "Inlining applied at lines %s" (s-join ", " lines)))
              (message "No usages found")))

        (error "No value to inline for %s" (car vals)))
      (error "Not a variable definition"))))

(defun elr-eval-and-replace ()
  "Replace the form at point with its value."
  (interactive)
  (elr--extraction-refactor "Replacement at"
    (let ((str (prin1-to-string (eval (read (car kill-ring))))))
      (insert str)
      (indent-for-tab-command))))

(defun elr--read-args ()
  "Read an arglist from the user."
  (let ((syms (elr--unbound-symbols-string)))
    (if (s-blank? syms)
        (read-string "Arglist: ")
      (read-string (format "Arglist (default: %s): " syms) nil nil syms))))

(defun elr-extract-function (name arglist)
  "Extract a function from the sexp beginning at point.
NAME is the name of the new function.
ARGLIST is its argument list."
  (interactive
   (list (read-string "Name: ")
         (elr--read-args)))
  (cl-assert (not (s-blank? name)) t "Name must not be blank")
  (let* ((args (s-trim arglist))
         (args (if (or (equal "()" args) (equal "nil" args)) "" args))
         (name (s-trim name)))
    (elr--extraction-refactor "Extracted to"
      (insert (apply 'elr--format-list name (s-split-words args)))
      (beginning-of-defun)
      (elr--insert-above (elr--format-defun "defun" name args (car kill-ring))))))

(defun elr--extract-to-form (symbol-str name)
  (let ((name (s-trim name)))
    (elr--extraction-refactor "Extracted to"
      (insert name)
      (beginning-of-defun)
      (elr--insert-above (elr--format-list symbol-str name (car kill-ring)))
      (beginning-of-defun)
      (indent-sexp))))

(defun elr-extract-variable (name)
  "Extract a form as the argument to a defvar named NAME."
  (interactive "sName: ")
  (cl-assert (not (s-blank? name)) t "Name must not be blank")
  (let ((name (s-trim name)))
    (elr--extraction-refactor "Extracted to"
      (insert name)
      (beginning-of-defun)
      (elr--insert-above (elr--format-list "defvar" name (car kill-ring)))
      (beginning-of-defun)
      (indent-sexp))))

(defun elr-extract-constant (name)
  "Extract a form as the argument to a defconst named NAME."
  (interactive "sName: ")
  (cl-assert (not (s-blank? name)) t "Name must not be blank")
  (let ((name (s-trim name)))
    (elr--extraction-refactor "Extracted to"
      (insert name)
      (beginning-of-defun)
      (elr--insert-above (elr--format-list "defconst" name (car kill-ring))))))

(defun elr-extract-autoload (function file)
  "Create an autoload for FUNCTION.
FILE is the file that declares FUNCTION.
See `autoload' for details."
  (interactive
   (let* ((sym  (intern (or (thing-at-point 'symbol) (read-string "Function: "))))
          (file (or (elr--symbol-file-name sym)
                    (read-string "File: "))))
     (list sym file)))
  (let ((form  (format "(autoload '%s \"%s\")" function file)) )
    (save-excursion
      (elr--reporting-buffer-changes "Extracted to"
        (if (elr--goto-first-match "^(autoload ")
            (progn (forward-line 1) (end-of-line) (newline) (insert form))
          (beginning-of-defun)
          (elr--insert-above form))))))

(provide 'elr-elisp)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; elr-elisp.el ends here
