;;; elr-elisp --- Refactoring commands for Emacs Lisp

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

;;; Refactoring commands for Emacs Lisp. Part of the ELR suite.

;;; Code:

(require 'cl-lib)

;;; ----------------------------------------------------------------------------
;;; Read and write lisp forms.
;;;
;;; Because the Elisp reader does not preserve comments or newlines, we embed
;;; them in forms so that they can be reconstructed when printing.

(defconst elr--newline-token :elr--newline)

(defconst elr--comment :elr--comment)

(defun elr--newline-token? (form)
  "Non-nil if FORM is a newline token."
  (equal form elr--newline-token))

(defun elr--comment? (form)
  "Non-nil if FORM is an elr--comment."
  (equal elr--comment (car-safe form)))

(defun elr--format-comments (line)
  "Wrap any comments at the end of LINE in a comment form.  Otherwise return LINE unchanged."
  (if-let (pos (s-index-of ";" line))
    (let ((code    (substring line 0 (1- pos)))
          (comment (substring line pos)))
      (concat
       code
       (if (s-blank? comment) "" (format " (%s %S)" elr--comment comment))))
    line))

(defun elr--read (str)
  "Read the given string STR as a Lisp expression, inserting tokens to represent whitespace."
  (let ((print-quoted t)
        (print-level nil)
        (print-length nil)
        (print-escape-newlines t)
        )
    (->> (s-lines str)
      (-map 'elr--format-comments)
      ;; Insert newline tokens.
      (s-join (format " %s " elr--newline-token))
      (read))))

(defun elr--reconstruct-comments (str)
  "Unpack any eol comments in STR, otherwise leave STR unchanged."
  (let ((prefix (format "(%s" elr--comment)))
    (if (s-contains? prefix str)
        (let* ((split   (s-split (s-trim prefix) str))
               (code    (or (car split) ""))
               (comment (format "%s" (read (cdr split)))))
          (concat code comment))
      str)))

(defun elr--print (form)
  "Print FORM as a Lisp expression, replacing whitespace tokens with newlines."
  (let ((nl (format "%s" elr--newline-token))
        ;; Print forms to any depth.
        (print-quoted t)
        (print-level nil)
        (print-length nil)
        (print-escape-newlines t)
        )
    (->> (prin1-to-string form)
      ;; Reconstruct newlines.
      (replace-regexp-in-string (eval `(rx (* space) ,nl (* space))) "\n")
      (s-lines)
      (-map 'elr--reconstruct-comments)
      (s-join "\n  "))))

;;; ----------------------------------------------------------------------------
;;; Navigation commands

(defun elr--goto-first-match (regex)
  "Move point to the first match in the buffer for REGEX."
  (save-match-data
    (when (string-match regex (buffer-string) 0)
      (goto-char (match-beginning 0)))))

(defun elr--looking-at-string? ()
  "Return non-nil if point is inside a string."
  (save-excursion
    (let ((point (point)))
      (beginning-of-defun)
      (nth 3 (parse-partial-sexp (point) point)))))

(defun elr--goto-open-round ()
  "Move to the opening paren for the Lisp list at point."
  (interactive)
  (when (or (not (equal "(" (thing-at-point 'char)))
            (elr--looking-at-string?))
    (beginning-of-sexp)
    (unless (equal "(" (thing-at-point 'char))
      (search-backward "("))))

(defun elr--goto-open-round-or-quote ()
  "Move to the opening paren or quote for the Lisp list at point."
  (interactive)
  (elr--goto-open-round)
  (when (or (thing-at-point-looking-at "'")
            (thing-at-point-looking-at "`")
            (elr--looking-at-string?))
    (search-backward-regexp (rx (or "'" "`")))))

;;; ----------------------------------------------------------------------------
;;; Formatting commands

(defun elr--reindent-string (form-str)
  "Reformat FORM-STRING, assuming it is a Lisp fragment."
  (with-temp-buffer
    (lisp-mode-variables)
    (insert form-str)
    (newline)
    (mark-defun)
    (indent-region (region-beginning) (region-end))
    (buffer-string)))

(defun elr--insert-above (form-str)
  "Insert and indent FORM-STR above the current top level form."
  (save-excursion
    ;; Move to position above top-level form.
    (beginning-of-defun)
    (beginning-of-line)
    (newline)
    (forward-line -1)
    (back-to-indentation)
    (insert (elr--reindent-string form-str))))

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
    (elr--read (buffer-substring-no-properties (region-beginning)
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
  "Kill the sexp near point then execute forms.
DESCRIPTION is used to report the result of the refactoring.
BODY is a list of forms to execute after extracting the sexp near point.
The extracted expression is bound to the symbol 'extracted-sexp'."
  (declare (indent 1))
  `(save-excursion

     ;; Either extract the active region or the sexp near point.
     (if (region-active-p)
         (kill-region (region-beginning)
                      (region-end))

       (elr--goto-open-round-or-quote)
       (kill-sexp))

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

(defun elr--macro-definition? (form)
  "Return t if FORM expands to a macro definition."
  (ignore-errors
    (let ((exp (macroexpand-all form)))
      ;; yo dawg I herd you like cars
      (and (equal 'defalias (car exp))
           (equal 'macro (cadar (cdaddr exp)))))))

(defun elr--function-definition? (form)
  "Return t if FORM expands to a function definition."
  (ignore-errors
    (let ((exp (macroexpand-all form)))
      (and (equal 'defalias (car exp))
           (equal 'function (caaddr exp))))))

(defun elr--variable-definition? (form)
  (ignore-errors
    (-contains? '(defconst defvar defcustom)
                (car (macroexpand-all form)))))

(defun elr--definition? (form)
  "Return non-nil if FORM is a definition."
  (or (elr--variable-definition? form)
      (elr--macro-definition? form)
      (elr--function-definition? form)))

(defun elr--looking-at-definition? ()
  "Return non-nil if point is at a definition form."
  (elr--definition? (elr--list-at-point)))

(defun elr--autoload-exists? (function str)
  "Returns true if an autoload for FUNCTION exists in string STR."
  (s-contains? (format "(autoload '%s " function) str))

;;; ----------------------------------------------------------------------------
;;; Define refactoring commands.

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
        (while (and (search-forward-regexp match-sym nil t)
                    (match-data))
          (setq lines (cons (line-number-at-pos) lines))
          ;; Perform replacement.
          (replace-match (elr--print value) t nil nil 1)
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
    (let ((str (prin1-to-string (eval extracted-sexp))))
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
   (rx bol "(" (* nonl) "def" (* nonl) (group "nil" (* space)) eol)
   "()"
   defun-str t nil 1))

(defun elr-extract-function (name arglist)
  "Extract a function from the sexp beginning at point.
NAME is the name of the new function.
ARGLIST is its argument list."
  (interactive (list (read-string "Name: ")
                     (elr--read-args)))
  (cl-assert (not (s-blank? name)) t "Name must not be blank")
  (elr--extraction-refactor "Extracted to"
    (let ((name (intern name)))
      ;; Insert usage.
      (insert (elr--print (cl-list* name arglist)))
      ;; Insert defun.
      (elr--insert-above
       (elr--format-defun
        (elr--print
         `(defun ,name ,arglist
            ,elr--newline-token
            ,(--drop-while (equal elr--newline-token it)
                           extracted-sexp))))))))

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

(defun elr-comment-form ()
  "Comment out the list at point."
  (interactive)
  (elr--goto-open-round-or-quote)
  (mark-sexp)
  (comment-region (region-beginning) (region-end)))

;;; ----------------------------------------------------------------------------
;;; Let expressions.

(defun elr--let-form? (form)
  "Non-nil if FORM is a let or let* form."
  (-contains? '(let let*) (car-safe form)))

(defun elr--defun-form? (form)
  "Non-nil if FORM is a function or macro definition form."
  (-contains? '(defun cl-defun defun* defmacro cl-defmacro defmacro*)
              (car-safe form)))

(defun elr--let-wrap (form)
  "Ensure FORM is wrapped with a `let' form."
  (if (elr--let-form? form)
      form
    (cl-list* 'let nil :elr--newline form)))

(cl-defun elr--insert-let-var (symbol value-form (let bindings &rest body))
  "Insert a binding into the given let expression."
  (cl-assert (elr--let-form? (list let)))
  (let* ((new    `((,symbol ,value-form)))
         (updated (if bindings (-concat bindings (list elr--newline-token) new) new)))
    (cl-list* let updated body)))

(defun elr--index-of (elt coll)
  "Find the index of ELT in COLL, or nil if not found."
  (->> coll
    (-map-indexed (lambda (i x) (cons i x)))
    (--first (equal elt (cdr it)))
    (car)))

(defun elr--split-defun (form)
  "Split a defun FORM into a list of (header body).
The body is the part of FORM that can be safely transformed without breaking the definition."
  (cl-assert (elr--defun-form? form) "Not a recognised definition form.")
  (let* (
         ;; Inspect the structure of the form. A definition contains an optional
         ;; docstring and interactive/declare specs which should not be changed
         ;; by operations to the body, so we skip those.

         (split-point

          (->> form
            ;; Newlines and comments not semantically useful here.
            (--remove (or (elr--newline-token? it) (elr--comment? it)))
            ;; Skip defun, symbol and arglist.
            (-drop 3)
            ;; Skip docstring, but only if there are forms afterwards.
            (funcall
             (lambda (xs) (if (and (stringp (car-safe xs)) (cdr xs))
                              (cdr xs)
                            xs)))
            ;; Skip INTERACTIVE and DECLARE forms.
            (--drop-while (or (equal 'interactive (car-safe it))
                              (equal 'declare (car-safe it))))
            ;; The car should be the first BODY form.
            (car)))

         ;; Now that we know which form is probably the body, get its position
         ;; in FORM and split FORM at that point.
         (pos (elr--index-of split-point form))
         )
    (cl-assert (integerp pos) "Unable to determine position of body within form %s" form)
    (-split-at pos form)))

(defun elr--partition-body (form)
  "Splits the given form into a 2-item list at its body forms, if any."
  (if (elr--defun-form? form)
      (elr--split-defun form)
    (list nil form)))

(defun elr--refactor-let-binding (symbol value form)
  "Insert (SYMBOL VALUE) into FORM, encapsulating FORM in a `let' expression if necessary."
  (destructuring-bind (header body) (elr--partition-body form)
    (-concat header (list (->> (elr--let-wrap body)
                            (elr--insert-let-var symbol value))))))

(defun elr-extract-to-let (symbol)
  "Extract the form at point into a let expression.
The expression will be bound to SYMBOL."
  (interactive "SSymbol: ")
  (elr--extraction-refactor "Extracted to let expression"
    ;; Insert usage.
    (insert (elr--print symbol))
    ;; Replace the top-level form.
    (beginning-of-defun)
    (kill-sexp)
    ;; Insert updated let-binding.
    (->> (elr--read (car kill-ring))
      (elr--refactor-let-binding symbol extracted-sexp)
      ;; Pretty-format for insertion.
      (elr--print)
      (elr--format-defun)
      (elr--reindent-string)
      (insert))))

;;; ----------------------------------------------------------------------------
;;; Declare commands with ELR.

;;; Inline variable
(elr-declare-action elr-inline-variable emacs-lisp-mode "inline"
  :predicate (elr--variable-definition? (elr--list-at-point)))

;;; Extract function
(elr-declare-action elr-extract-function emacs-lisp-mode "function"
  :predicate (not (elr--looking-at-definition?))
  :description "defun")

;;; Extract variable
(elr-declare-action elr-extract-to-let emacs-lisp-mode "let-bind"
  :predicate (not (elr--looking-at-definition?))
  :description "let")

;;; Extract variable
(elr-declare-action elr-extract-variable emacs-lisp-mode "variable"
  :predicate (and (not (elr--looking-at-definition?))
                  (thing-at-point 'defun))
  :description "defvar")

;;; Extract constant
(elr-declare-action elr-extract-constant emacs-lisp-mode "constant"
  :predicate (not (elr--looking-at-definition?))
  :description "defconst")

;;; Eval and replace expression
(elr-declare-action elr-eval-and-replace emacs-lisp-mode "eval"
  :predicate (not (elr--looking-at-definition?))
  :description "value")

;;; Extract autoload
(elr-declare-action elr-extract-autoload emacs-lisp-mode "autoload"
  :description "autoload"
  :predicate (and (functionp (symbol-at-point))
                  (not (elr--variable-definition? (elr--list-at-point)))
                  (not (elr--autoload-exists? (symbol-at-point) (buffer-string)))))

;;; Comment-out form
(elr-declare-action elr-comment-form emacs-lisp-mode "comment"
  :predicate t)

(provide 'elr-elisp)

;;; NB: callargs warnings disabled to prevent format warnings caused by
;;; `cl-assert', as of Emacs 24.3.50 darwin.

;; Local Variables:
;; lexical-binding: t
;; byte-compile-warnings: (not callargs)
;; End:

;;; elr-elisp.el ends here
