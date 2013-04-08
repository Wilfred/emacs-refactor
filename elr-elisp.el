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
(defconst elr--line-comment :elr--line-comment)

;;; ----------------------------------------------------------------------------
;;; Navigation commands

(defun elr--goto-first-match (regex)
  "Move point to the first match in the buffer for REGEX."
  (save-match-data
    (when (string-match regex (buffer-string) 0)
      (goto-char (match-beginning 0)))))

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

  ;; Print forms to any depth.
  (let (print-level eval-expression-print-level eval-expression-print-length)
    (->> (s-lines str)
      ;; Extract comments.
      (--map (if (s-matches? (rx bol (* space) (+ ";")) it)
                 (format " (%s %S ) " elr--line-comment it)
               it))
      ;; Insert newline token.
      (s-join (format " %s " elr--newline-token))
      (read))))

(defun elr--print (form)
  "Print FORM, replacing whitespace tokens with newlines."
  (let ((nl (prin1-to-string elr--newline-token))
        (lc (prin1-to-string elr--line-comment)))

    ;; Print forms to any depth.
    (setq print-quoted t)
    (setq print-level nil)
    (setq print-length nil)
    (setq print-escape-newlines t)

    (->> (prin1-to-string form)

      ;; Reconstruct newlines.
      (replace-regexp-in-string (eval `(rx (* space) ,nl (* space))) "\n")

      ;; Reconstruct comments.
      (s-lines)
      (--map (if (s-matches? (eval `(rx "(" ,lc)) it)
                 (replace-regexp-in-string
                  (eval `(rx "(" ,lc " " (group-n 1 (* nonl)) ")" (* space) eol))
                  "" it t "\1")
               it))

      (s-join "\n  "))))

;;; ----------------------------------------------------------------------------
;;; Formatting commands

;;; FIXME

(defun elr--insert-above (form-str)
  "Insert and indent FORM-STR above the current top level form."
  (save-excursion
    ;; Move to position above top-level form.
    (beginning-of-defun)
    (beginning-of-line)
    (newline)
    (forward-line -1)
    (back-to-indentation)
    ;; Insert FORM as a string.
    (insert form-str)
    (newline)
    ;; Reformat.
    (mark-defun)
    (indent-region (region-beginning) (region-end))))

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
;;; Refactoring Macros

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

(defmacro elr--extraction-refactor (description &rest body)
  "Kill the sexp near point then execute BODY forms.
The extracted expression is bound to the symbol 'extracted-sexp'.
"
  (declare (indent 1))
  `(save-excursion
     (elr--goto-open-round-or-quote)
     (kill-sexp)
     (let ((extracted-sexp (elr--read (car kill-ring))))
       ;; Revert kill-ring pointer.
       (setq kill-ring (cdr kill-ring))
       (save-excursion
         (elr--reporting-buffer-changes ,description
           ,@body)))))

(defun elr--line-visible? (line)
  "Return true if LINE is within the visible bounds of the current window."
  (let* ((min (line-number-at-pos (window-start)))
         (max (line-number-at-pos (window-end))))
    (and (>= line min) (<= line max))))

;;; ----------------------------------------------------------------------------
;;; Definition site tests.

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
  (let* ((syms (elr--unbound-symbols-string))
         (input (s-trim
                 (if (s-blank? syms)
                     (read-string "Arglist: ")
                   (read-string (format "Arglist (default: %s): " syms) nil nil syms)))))
    (unless (or (s-blank? input)
                (s-matches? (rx (or "()" "nil")) input))
      (read (format "(%s)" input)))))

(defun elr--format-defun (defun-str)
  "Format DEFUN-STR to a prettier defun representation."
  (replace-regexp-in-string
   (rx bol "(defun" (* nonl) (group "nil" (* space)) eol)
   "()"
   defun-str t nil 1))

(defun elr-extract-function (name arglist)
  "Extract a function from the sexp beginning at point.
NAME is the name of the new function.
ARGLIST is its argument list."
  (interactive (list (read-string "Name: ")
                     (elr--read-args)))
  (cl-assert (not (s-blank\? name)) t "Name must not be blank")
  (elr--extraction-refactor "Extracted to"
    (let ((name (intern name)))
      ;; Insert usage.
      (insert (elr--print (cl-list* name arglist)))
      ;; Insert defun, substituting parens for nil arglist.
      (elr--insert-above
       (elr--format-defun
        (elr--print
         `(defun ,name ,arglist
            ,elr--newline-token
            ,extracted-sexp)))))))

(defun elr-extract-variable (name)
  "Extract a form as the argument to a defvar named NAME."
  (interactive "sName: ")
  (cl-assert (not (s-blank? name)) t "Name must not be blank")
  (elr--extraction-refactor "Extracted to"
    ;; Insert usage.
    (insert (s-trim name))
    ;; Insert definition.
    (elr--insert-above
     (elr--print
      (list 'defvar (intern name) extracted-sexp)))))

(defun elr-extract-constant (name)
  "Extract a form as the argument to a defconst named NAME."
  (interactive "sName: ")
  (cl-assert (not (s-blank? name)) t "Name must not be blank")
  (elr--extraction-refactor "Extracted to"
    ;; Insert usage
    (insert (s-trim name))
    ;; Insert definition.
    (elr--insert-above
     (elr--print
      (list 'defconst (intern name) extracted-sexp)))))

(defun elr-extract-autoload (function file)
  "Create an autoload for FUNCTION.
FILE is the file that declares FUNCTION.
See `autoload' for details."
  (interactive
   (let* ((sym  (intern (or (thing-at-point 'symbol) (read-string "Function: "))))
          (file (or (elr--symbol-file-name sym)
                    (read-string "File: "))))
     (list sym file)))
  (let ((form `(autoload ',function ,file)))
    (save-excursion
      (elr--reporting-buffer-changes "Extracted to"
        ;; Put the extraction next to existing autoloads if any, otherwise
        ;; insert above top-level form.
        (if (elr--goto-first-match "^(autoload ")
            (progn (forward-line 1) (end-of-line) (newline)
                   (insert (elr--print form)))
          (elr--insert-above
           (elr--print form)))))))

(provide 'elr-elisp)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; elr-elisp.el ends here
