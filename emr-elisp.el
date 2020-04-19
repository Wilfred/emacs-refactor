;;; emr-elisp.el --- Refactoring commands for Emacs Lisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Chris Barrett
;; Copyright (C) 2018 Wilfred Hughes

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

;; Refactoring commands for Emacs Lisp

;;; Code:

(require 's)
(require 'list-utils)
(require 'dash)
(require 'thingatpt)
(require 'emr)
(require 'emr-lisp)
(autoload 'define-compilation-mode "compile")
(autoload 'paredit-splice-sexp-killing-backward "paredit")

(defcustom emr-el-definition-macro-names
  '(defun defun* cl-defun defmacro defmacro* cl-defmacro defcustom
     defvar defvar-local defconst defsubst defsubst* cl-defsubst)
  "Lists the function, macro and variable definition forms in Elisp.
Used when searching for usages across the whole buffer."
  :group 'emr)

(defun emr-el:safe-read (sexp)
  "A wrapper around `read' that return nil immediately if SEXP is null.

If sexp is nil, `read' would prompt the user for input from stdin.
Bad."
  (and sexp (read sexp)))

(defun emr-el:print (form)
  "Print FORM as a Lisp expression."
  (let (
        ;; Print forms to any depth.
        (print-quoted t)
        (print-level nil)
        (print-length nil)
        (print-escape-newlines t)
        )
    (prin1-to-string form)))

;;;; Navigation commands

(defun emr-el:goto-first-match (regex)
  "Move point to the first match in the buffer for REGEX."
  (save-match-data
    (when (string-match regex (buffer-string) 0)
      (goto-char (match-beginning 0)))))

(defun emr-el:looking-at-decl? ()
  (-contains? '(interactive declare) (car-safe (list-at-point))))

;;;; Formatting commands

(defun emr-el:symbol-file-name (fn)
  "Find the name of the file that declares function FN."
  (-when-let (file (find-lisp-object-file-name fn (symbol-function fn)))
    (and (stringp file)
         (file-name-nondirectory (file-name-sans-extension file)))))

(defvar emr-el:special-symbols
  '(--cl-rest-- &rest &optional &key &allow-other-keys \,\@ \,)
  "A list of symbols that should be ignored by variable searches.")

(cl-defun emr-el:bindings-in-lambda ((_lam arglist &rest body))
  "Return all bound variables within a lambda form."
  (let ((bs (-difference arglist emr-el:special-symbols)))
    (-concat bs (emr-el:bound-variables body))))

(cl-defun emr-el:bindings-in-let ((_let bindings &rest body))
  "Return the list of bound values in the given `let' or `let*' expression."
  (-concat (emr-el:let-binding-list-symbols bindings)
           (emr-el:bound-variables body)))

(cl-defun emr-el:bindings-in-defalias ((_def (_quote sym) func))
  "Return the bindings in a defalias form, including the named alias."
  (cons sym (emr-el:bound-variables func)))

(defun emr-el:bound-variables (form)
  "Find the list of let- or lambda-bound variables in FORM."
  ;; Form traversal can recur infinitely in some quotation scenarios. In
  ;; such cases it is not a problem to bail and unwind the stack.
  (ignore-errors
    (-uniq
     (let* (
            ;; Handle errors in expansion. Expansion errors are common with syntax
            ;; quotes, for example.
            (form (or (ignore-errors (macroexpand-all form))
                      (ignore-errors (macroexpand form))
                      form))

            (hd (car-safe form)))
       (cond
        ((equal 'lambda hd) (emr-el:bindings-in-lambda form))
        ((equal 'let hd)  (emr-el:bindings-in-let form))
        ((equal 'let* hd) (emr-el:bindings-in-let form))
        ((equal 'defalias hd) (emr-el:bindings-in-defalias form))
        ;; `function' is the quotation form for function objects.
        ;; Do not bail if the next item is not a lambda.
        ((equal 'function hd) (condition-case _err
                                  (-mapcat 'emr-el:bindings-in-lambda (cdr form))
                                (error
                                 (-mapcat 'emr-el:bound-variables (cdr form)))))
        ;; FORM is probably a value if we're not looking at a list, and can be
        ;; ignored.
        ((listp form)
         (->> form
              ;; Handle improper lists.
              (list-utils-make-proper-copy)
              (-mapcat 'emr-el:bound-variables))))))))

(defun emr-el:unquoted-symbols (form)
  "Return a list of every unquoted symbol in FORM."
  (let (syms
        (forms-remaining (list form)))
    (while (not (null forms-remaining))
      (let ((subform (pop forms-remaining)))
        (cond
         ;; Skip quoted symbols.
         ((and (consp subform) (eq (car subform) 'quote)))
         ;; Iterate on the subforms for lists.
         ((consp subform)
          (push (cdr subform) forms-remaining)
          (push (car subform) forms-remaining))
         ;; If this node is a symbol, add it to our list.
         ((and subform (symbolp subform))
          (push subform syms)))))
    syms))

(defun emr-el:free-variables (form &optional context)
  "Try to find the symbols in FORM that do not have variable bindings.
CONTEXT is the top level form that encloses FORM."

  ;; Macro-expand FORM and find the list of bound symbols. Diff this with the
  ;; other symbols in FORM. Figure out which ones are not functions, keywords,
  ;; special vars, etc. This should give a pretty good idea of which symbols are
  ;; 'free'.

  (let ((bound-vars (emr-el:bound-variables form))
        (ctx-bound (emr-el:bound-variables context))
        (form-syms (emr-el:unquoted-symbols form)))
    (->> (or (ignore-errors (macroexpand-all form)) form)
      ;; Get all symbols from FORM's macro-expansion.
      (list)
      (list-utils-make-proper-inplace)
      (-flatten)
      (-filter 'symbolp)
      (-distinct)
      ;; Only use symbols present in the original form. This prevents free vars
      ;; from the macro-expansion leaking in.
      (--filter (-contains? form-syms it))
      ;; Finally, reduce the candidates.
      (--remove (or (-contains? bound-vars it)
                    (-contains? emr-el:special-symbols it)
                    (booleanp it)
                    (keywordp it)
                    ;; Remove special vars and function names, unless they've
                    ;; been bound in the enclosing form.
                    (unless (-contains? ctx-bound it)
                      (or
                       (special-variable-p it)
                       (ignore-errors
                        (symbol-function it)))))))))

;;;; Definition site tests

(defun emr-el:macro-definition? (form)
  "Return t if FORM expands to a macro definition."
  (ignore-errors
    (let* ((exp (macroexpand-all form))
           ;; Skip surrounding `prog1'. This will exist if the macro has
           ;; `declare' specs.
           (exp (if (equal 'prog1 (car exp))
                    (cdr exp)
                  exp)))
      ;; A macro expands to a defalias.
      (cl-destructuring-bind (&optional def _sym binding &rest rest) exp
        ;; The binding is a call to `cons'. The first arg is the quoted
        ;; symbol `macro'.
        (cl-destructuring-bind (&optional _cons mac &rest lambda) binding
          (and (equal 'defalias def)
               ;; NB quoted symbol 'macro must be quoted twice for comparison.
               (equal ''macro mac)))))))

(defun emr-el:function-definition? (form)
  "Return t if FORM expands to a function definition."
  (ignore-errors
    (let ((exp (macroexpand-all form)))
      (and (equal 'defalias (car exp))
           (equal 'function (caaddr exp))))))

(defun emr-el:variable-definition? (form)
  (ignore-errors
    (-contains? '(defconst defvar defcustom)
                (car (macroexpand-all form)))))

(defun emr-el:definition? (form)
  "Return non-nil if FORM is a definition."
  (or (emr-el:variable-definition? form)
      (emr-el:macro-definition? form)
      (emr-el:function-definition? form)))

(defun emr-el:looking-at-definition? ()
  "Non-nil if point is at a definition form."
  (ignore-errors (emr-el:definition? (list-at-point))))

;;;; Refactoring commands

(defun emr-el:extract-var-values (sexp)
  "Return the name and initializing value of SEXP if it is a variable definition."
  (let ((exp (macroexpand-all sexp)))
    (when (emr-el:variable-definition? exp)
      (cl-destructuring-bind (_def sym &rest forms) exp
          (cons sym (car forms))))))

(cl-defun emr-el:replace-usages ((sym . value))
  "Replace all instances of SYM with VALUE in the current buffer.
Returns a list of lines where changes were made."
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (let ((match-sym
             (rx-to-string
              `(seq (not (any "(")) (* space)
                    (group symbol-start ,(symbol-name sym)
                           symbol-end))))
            (lines))
        ;; Check for "(" since we don't want to replace function calls.
        (while (and (search-forward-regexp match-sym nil t)
                    (match-data))
          (setq lines (cons (line-number-at-pos) lines))
          ;; Perform replacement.
          (replace-match (emr-el:print value) t nil nil 1)
          (emr-lisp-reindent-defun))
        (nreverse lines)))))

;;;###autoload
(defun emr-el-inline-variable ()
  "Inline the variable defined at point.

Uses of the variable in the current buffer are replaced with the
initvalue in the variable definition.

EXAMPLE:

  (emr-el-inline-variable)

BEFORE:

  (defvar x| value)

  (usage x)

AFTER:

  (usage value)"
  (interactive "*")
  (save-excursion
    (emr-lisp-back-to-open-round)
    (-if-let (def (emr-el:extract-var-values (list-at-point)))
      (if (or (consp def) (> (length def) 1))
          (emr-lisp-extraction-refactor () "Inlining applied at"

            ;; Clean up line spacing.
            (while (emr-blank-line?)
              (kill-line))

            ;; Perform inlining.
            ;;
            ;; emr-lisp-extraction-refactor will report the first insertion. If
            ;; there are none or more than one insertion, override this report.
            (-if-let (lines (-map 'int-to-string (emr-el:replace-usages def)))
              (when (> (length lines) 1)
                (message "Inlining applied at lines %s" (s-join ", " lines)))
              (user-error "No usages found")))

        (user-error "No value to inline for %s" (car def)))
      (user-error "Not a variable definition"))))

; ------------------

(defun emr-el:eval-and-print-progn (prog)
  "Eval and print each form in sexp PROG."
  (->> (macroexp-unprogn prog)
    (-map 'eval)
    (-remove 'null)
    (-map 'emr-el:print)))

;;;###autoload
(defun emr-el-eval-and-replace ()
  "Replace the current region or the form at point with its value.

EXAMPLE:

  (emr-el-eval-and-replace)

BEFORE:

  (+ (+ 1 2)| 3)

AFTER:

  (+ 3 3)"
  (interactive "*")
  (emr-lisp-extraction-refactor (sexp) "Replacement at"

    (-if-let (form (emr-el:safe-read sexp))
      (progn
        (insert (->> form (emr-el:eval-and-print-progn) (s-join "\n")))
        (indent-for-tab-command)
        (emr-lisp-reindent-defun))
      (user-error "Unable to read the given form"))))

; ------------------

(defun emr-el:read-with-default (prompt value)
  "Prompt for user input, showing PROMPT with an inline default VALUE."
  (let ((val (s-trim (format "%s" value))))
    (s-trim
     (if (s-blank? val)
         (read-string (format "%s:  " prompt))
       (read-string (format "%s (default: %s): "  prompt val) nil nil val)))))

(defun emr-el:format-submitted-arglist (arglist)
  "Format a user-submitted ARGLIST, raising an error if it is malformed."
  (unless (or (s-blank? arglist)
              (s-matches? (rx (or "()" "nil")) arglist))
    (condition-case _err
        (emr-el:safe-read (format "(%s)" arglist))
      (error
       ;; Rethrow reader errors as something more informative.
       (user-error "Malformed arglist")))))

(defun emr-el:read-args (form context)
  "Read an arglist from the user, using FORM to generate a suggestion.
CONTEXT is the top level form that encloses FORM."
  (let ((input
         ;; Generate suggested arglist for prompt.
         (->> (emr-el:free-variables form context)
           (-map 'symbol-name)
           (s-join " ")
           (s-trim)
           ;; Read user input, supplying default arglist.
           (emr-el:read-with-default "Arglist" )))
        )
    (emr-el:format-submitted-arglist input)))

(defun emr-el:format-defun (defun-str)
  "Format DEFUN-STR to a prettier defun representation."
  (replace-regexp-in-string
   (rx bol "(" (* nonl) "def" (* nonl) (group "nil" (* space)) eol)
   "()"
   defun-str t nil 1))

(defun emr-el:form-extent-for-extraction ()
  "Return either the current region or the list at point."
  (if (region-active-p)
      (emr-line-str)
    (list-at-point)))

(defun emr-el:unprogn (str)
  (with-temp-buffer
    (save-excursion (insert str))
    (forward-word)
    (when (thing-at-point-looking-at "progn")
      (ignore-errors
       (paredit-splice-sexp-killing-backward)))
    (buffer-string)))

;;;###autoload
(defun emr-el-extract-function (name arglist)
  "Extract a function, using the current region or form at point as the body.
NAME is the name of the new function.
ARGLIST is its argument list.

EXAMPLE:

  (emr-el-extract-function \"extracted\" '(x))

BEFORE:

  (defun orig (x)
    (application| x))

AFTER:

  (defun extracted (x)
    (application x))

  (defun orig (x)
    (extracted x))"
  (interactive
   (list
    ;; Function name.
    (read-string "Name: ")

    ;; Prompt user with default arglist.
    (emr-el:read-args (emr-el:form-extent-for-extraction)
                      (thing-at-point 'defun))))

  (when (s-blank? name)
    (user-error "Name must not be blank"))

  (emr-lisp-extraction-refactor (sexp) "Extracted to"
    (let ((name (intern name))
          ;; Extract to a `cl-defun' if given a Common Lisp-style arglist.
          (defun-form (if (-any? 'listp arglist) 'cl-defun 'defun))
          (body (emr-el:unprogn sexp)))
      ;; Insert usage at point.
      (insert (emr-el:print (cl-list* name arglist)))
      ;; Insert defun.
      (->> (format "(%s %s %s\n  %s)" defun-form name arglist body)
           (emr-el:format-defun)
           (emr-lisp-insert-above-defun)))))

; ------------------

(defun emr-el:infer-arglist-for-usage (form)
  "Suggest a suitable arglist for the given function application FORM."
  (->> form
    ;; Anything that isn't a symbol becomes 'argn'.
    (--map-indexed (if (symbolp it) it (intern (format "arg%s" it-index))))
    ;; Drop function name.
    (-drop 1)))

;;;###autoload
(defun emr-el-implement-function (name arglist)
  "Create a function definition for the symbol at point.
The function will be called NAME and have the given ARGLIST.

EXAMPLE:

  (emr-el-implement-function \"hello\" '(x y))

BEFORE:

  |(hello x y)

AFTER:

  (defun hello (x y)
    )

  (hello x y)"
  (interactive (list
                (emr-el:safe-read
                 (emr-el:read-with-default "Name" (symbol-at-point)))
                ;; Infer arglist from usage.
                (->> (list-at-point)
                  (emr-el:infer-arglist-for-usage)
                  (-map 'symbol-name)
                  (s-join " ")
                  (s-trim)
                  (emr-el:read-with-default "Arglist")
                  (emr-el:format-submitted-arglist))))
  ;; Determine which defun form to use.
  (let ((defun-form (if (-any? 'listp arglist) 'cl-defun 'defun))
        pos)

    ;; Insert usage and defun, then move to the point to the body of the defun.

    (save-excursion
      ;; Mark whole list at point.
      (beginning-of-thing 'sexp)
      (mark-sexp)

      (emr-lisp-extraction-refactor ()  "Defined function"

        ;; Insert reference. Quote the symbol if it's not in the funcall
        ;; position.
        (if (thing-at-point-looking-at "(")
            (insert (format "%s" name))
          (insert (format "#'%s" name)))

        ;; Insert definition.
        (setq pos (->> (format "(%s %s %s\n  )" defun-form name arglist)
                    (emr-el:format-defun)
                    (emr-lisp-insert-above-defun)))))

    ;; Move to end of inserted form.
    (goto-char pos)
    ;; Move to body position.
    (beginning-of-defun)
    (forward-line 1)
    (indent-for-tab-command)))

; ------------------

;;;###autoload
(defun emr-el-extract-variable (name)
  "Extract the current region or form at point to a special variable.
The variable will be called NAME.

EXAMPLE:

  (emr-el-extract-variable \"x\")

BEFORE:

  (usage (+ 1 2)|)

AFTER:

  (defvar x (+ 1 2))

  (usage x)"
  (interactive "*sName: ")
  (when (s-blank? name)
    (user-error "Name must not be blank"))
  (emr-lisp-extraction-refactor (sexp) "Extracted to"
    ;; Insert usage.
    (insert (s-trim name))
    ;; Insert definition.
    (emr-lisp-insert-above-defun (format "(defvar %s %s)" name sexp))))

;;;###autoload
(defun emr-el-extract-constant (name)
  "Extract the current region or form at point to a constant special variable.
The variable will be called NAME.

EXAMPLE:

  (emr-el-extract-constant \"x\")

BEFORE:

  (usage (+ 1 2)|)

AFTER:

  (defconst x (+ 1 2))

  (usage x)"
  (interactive "*sName: ")
  (when (s-blank? name)
    (user-error "Name must not be blank"))
  (emr-lisp-extraction-refactor (sexp) "Extracted to"
    ;; Insert usage
    (insert (s-trim name))
    ;; Insert definition.
    (emr-lisp-insert-above-defun (format "(defconst %s %s)" name sexp))))

; ------------------

(defun emr-el:autoload-exists? (function str)
  "Return non-nil if an autoload for FUNCTION exists in string STR."
  (s-contains? (format "(autoload '%s " function) str))

(defun emr-el:beginning-of-defun ()
  "A safe version of `beginning-of-defun'.
Attempts to find an enclosing defun form first, rather than
relying on indentation."
  (or
   ;; Search for known defun form enclosing point.
   (cl-loop
    while (ignore-errors (backward-up-list) t)
    do (when (thing-at-point-looking-at
              (rx-to-string `(seq "(" (or ,@(-map 'symbol-name emr-el-definition-macro-names)))))
         (return (point))))
   ;; Fall back to using indentation.
   (ignore-errors
     (beginning-of-thing 'defun))))

(defun emr-el:autoload-directive-exsts-above-defun? ()
  "Non-nil if the current defun is preceeded by an autoload directive."
  (save-excursion
    (emr-el:beginning-of-defun)
    (forward-line -1)
    (emr-line-matches? (rx bol (* space) ";;;###autoload" (* space) eol))))

;;;###autoload
(defun emr-el-insert-autoload-directive ()
  "Insert an autoload directive above the current defun, macro or keymap.

EXAMPLE:

  (emr-el-insert-autoload-directive)

BEFORE:

  (defun hello| ())

AFTER:

  ;;;###autoload
  (defun hello ())"
  (interactive "*")
  (unless (emr-el:autoload-directive-exsts-above-defun?)
    (emr-reporting-buffer-changes "Inserted autoload"
      (save-excursion
        (beginning-of-thing 'defun)
        (open-line 1)
        (insert ";;;###autoload")))))

(defun emr-el:sort-autoloads (autoloads)
  (let ((file-grouping (->> autoloads (--group-by (nth 1 it)))))
    ;; Order by file name...
    (->> (sort file-grouping (lambda (L R) (string< (car L) (car R))))
      ;; ...then by function name.
      (-mapcat (lambda (assoc)
                 (sort (cdr assoc) (lambda (L R) (string< (car L) (car R))))))
      ;; Trim all components.
      (-map (lambda (xs) (--map (s-trim it) xs)))
      ;; ;; Recombine and insert into buffer.
      (--map (cl-destructuring-bind (fname file &optional rest) it
               (if (not (s-blank? rest))
                   (format "(autoload %s %s %s)" fname file rest)
                 (format "(autoload %s %s)" fname file)))))))

;;;###autoload
(defun emr-el-tidy-autoloads ()
  "Consolidate and reorder autoloads in the current buffer.
Order autoloads alphabetically by their file, then by their function name."
  (interactive "*")
  (let (autoloads
        (rx-autoload (rx bol (* space) "(autoload" (+ space)
                         (group-n 1 (+ (not space)))
                         (+ space)
                         (group-n 2 (+ (not space)))
                         (* space)
                         (group-n 3 (*? nonl))
                         ")")))
    (save-excursion
      (when (emr-el:goto-first-match rx-autoload)
        (beginning-of-line)
        (forward-line)

        ;; Collect autoloads in buffer.
        (save-excursion
          (goto-char (point-min))
          (while (search-forward-regexp rx-autoload nil t)
            (push (list (match-string 1) (match-string 2) (match-string 3))
                  autoloads)
            (replace-match "")
            (when (emr-blank-line?)
              (ignore-errors
                (kill-line)))))

        (->> (emr-el:sort-autoloads autoloads)
          (-distinct)
          (s-join "\n")
          (s-append "\n")
          (insert))))))

;;;###autoload
(defun emr-el-extract-autoload (function file)
  "Create an autoload for FUNCTION and insert it into the buffer.
FILE is the file that declares FUNCTION.  See `autoload' for
details.

* If there are no autoloads in the buffer, the new autoload will
  be inserted above the current toplevel form.

* If other autoloads exist in the buffer, the new autoload will
  be inserted near them."
  (interactive
   (let* ((sym  (intern (or (thing-at-point 'symbol) (read-string "Function: "))))
          (file (or (emr-el:symbol-file-name sym)
                    (read-string "File: "))))
     (list sym file)))

  (let ((form `(autoload ',function ,file)))
    (save-excursion
      (emr-reporting-buffer-changes "Extracted to"
        ;; Put the extraction next to existing autoloads if any, otherwise
        ;; insert above top-level form.
        (if (emr-el:goto-first-match "^(autoload ")
            (progn (forward-line 1) (end-of-line) (newline)
                   (insert (emr-el:print form)))
          (emr-lisp-insert-above-defun
           (emr-el:print form)))))

    (emr-el-tidy-autoloads)))

; ------------------

(defun emr-el:first-atom (form)
  "Return the first atom the car of FORM, at any level of nesting."
  (if (listp form)
      (car-safe (-flatten form))
    form))

(defun emr-el:let-binding-list-symbols (binding-forms)
  "Return the symbols defined in the given let BINDING-FORMS."
  (->> binding-forms
    (--map (or (car-safe it) it))
    (-remove 'null)))

(defconst emr-el:scope-boundary-forms
  '(lambda defun cl-defun defun* defmacro cl-defmacro defmacro*
     let let* save-excursion unwind-protect
     flet cl-flet cl-flet* cl-labels labels
     ert-deftest)
  "A list of forms that define some kind of scope or context.
They will bound upward searches when looking for places to insert let forms.")

(defun emr-lisp-peek-back-upwards ()
  "Return the car of the enclosing form."
  (save-excursion
    (when (ignore-errors (backward-up-list) t)
      (forward-char 1)
      (sexp-at-point))))

(defun emr-el:simplify-let-form-at-point ()
  "Tidy the let form at point.
If it has no bindings, splice its contents into the surrounding
form or replace with `progn'."
  (save-excursion
    (emr-el:goto-start-of-let-binding)
    ;; Move into list.
    (forward-char 1)
    (-let (((let-keyword bindings . body) (list-at-point)))
      ;; Move to position after bindings list.
      (forward-list 1)
      (cond
       ;; If there are no bindings, splice the body forms into the
       ;; surrounding context if any of the following are true:
       ;;
       ;; 1. the let body has only a single form
       ;;
       ;; 2. the let expression is an &body or &rest argument to the
       ;; enclosing form.
       ;;
       ((and (null bindings)
             (or (>= 1 (length body))
                 (-contains? (cons 'progn emr-el:scope-boundary-forms)
                             (emr-lisp-peek-back-upwards))))
        (paredit-splice-sexp-killing-backward))

       ;; Replace `let*' with `let' if there's only a single binding form.
       ((equal 1 (length bindings))
        (when (eq let-keyword 'let*)
          (emr-el-toggle-let*)))

       ;; Otherwise replace `let' with `progn'.
       ((null bindings)
        (backward-kill-sexp 2)
        (insert "progn"))))

    (emr-lisp-reindent-defun)))

(defun emr-el:join-line-after-let-binding-kill ()
  "Tidy up newlines after modifiying a let-form binding list."
  (when (or (emr-blank-line?)
            (emr-line-matches? (rx "(" (* space) eol))
            (emr-line-matches? (rx bol (* space) ")" (* space) eol)))
    (forward-char 1)
    (join-line)))

;;;###autoload
(defun emr-el-delete-let-binding-form ()
  "Delete the let binding around point."
  (interactive "*")
  (cl-assert (emr-el:looking-at-let-binding-symbol?))
  (let ((kr kill-ring))
    (unwind-protect
        (save-excursion
          ;; Delete binding.
          (emr-lisp-back-to-open-round)
          (kill-sexp)

          ;; Reformat after kill.
          (emr-el:join-line-after-let-binding-kill)

          ;; Ensure whole form is correctly reindented.
          (mark-defun)
          (indent-region (region-beginning) (region-end)))

      ;; Restore kill-ring.
      (setq kill-ring kr)
      (emr-el:simplify-let-form-at-point))))

; ------------------

(defun emr-el:goto-containing-body-form ()
  "Search upwards for the first function or macro declaration enclosing point.
Move to that body form that encloses point."
  ;; Ensure we're at the start of the current symbol.
  (when (looking-at (rx symbol-end))
    (backward-sexp))
  (catch 'found
    (while t
      (when (-contains-p emr-el:scope-boundary-forms (emr-lisp-peek-back-upwards))
        (throw 'found (point)))
      (condition-case nil
          (backward-up-list)
        (error
         ;; Outer sexp, can't go up any further. We didn't find any
         ;; body form.
         (throw 'found nil))))))

(defun emr-el:let-start-pos ()
  "Search upward form point to find the start position of the innermost let."
  (let ((positions
         (list
          (emr-lisp-find-upwards 'let)
          (emr-lisp-find-upwards 'let*)
          (emr-lisp-find-upwards '-let)
          (emr-lisp-find-upwards '-let*))))
    (setq positions (-non-nil positions))
    (when positions
      (-max positions))))

;; TODO: the example is redundant here.
(defun emr-el-toggle-let* ()
  "Toggle between let and let* in the enclosing let form.

EXAMPLE:

  (emr-el-toggle-let*)

BEFORE:

  (let* ((x 1))
    (+| x 1))

AFTER:

  (let ((x 1))
    (+ x 1))"
  (interactive)
  (save-excursion
    (goto-char (emr-el:let-start-pos))
    (forward-char 1)
    (forward-sexp)
    ;; TODO: reindent afterwards
    (if (eq (char-before (point)) ?*)
        (delete-char -1)
      (insert "*"))))

(defun emr-el:wrap-body-form-at-point-with-let ()
  "Wrap the form with a let statement at a sensible place."
  (emr-el:goto-containing-body-form)
  (let ((start-pos (point)))
    (forward-sexp)
    (insert ")")
    (goto-char start-pos))
  (insert "(let ()\n  ")
  (emr-lisp-reindent-defun))

;; https://github.com/Wilfred/emacs-refactor/issues/35
(defun emr-el:add-let-binding (var val)
  "Add a binding for symbol VAR assigned to VAL to the innermost let form.

Ensures that VAR is inserted before point.

VAL should be a string of elisp source code."
  (-let* ((let-form-start (emr-el:let-start-pos))
          ;; Read the whole let form.
          ((let-keyword let-vars . _)
           (save-excursion
             (goto-char let-form-start)
             (read (current-buffer))))
          (let-paren-depth
           (save-excursion
             (nth 0 (syntax-ppss let-form-start))))
          (vars-end
           (save-excursion
             (goto-char let-form-start)
             ;; Step over opening paren.
             (forward-char)
             ;; Step over let keyword.
             (forward-sexp)
             ;; Step over the vars.
             (forward-sexp)
             (point))))
    (save-excursion
      (let ((var-start-depth (+ let-paren-depth 2))
            vars-after-p)
        (if (> (point) vars-end)
            ;; We're extracting a let binding from the body, so we'll insert
            ;; this new var after all the existing vars.
            (progn
              (goto-char vars-end)
              (backward-char)
	      (when let-vars
		(newline-and-indent)))
          ;; Move up s-expressions until we're at the beginning of the
          ;; variable declaration.
          ;; (let (|(x foo)) ...)
          (setq vars-after-p t)
          (while (not (eq (nth 0 (syntax-ppss)) var-start-depth))
            (goto-char (nth 1 (syntax-ppss))))
          ;; Move to the end of the previous sexp, if present.
          (if (> (length let-vars) 1)
              (progn
                (backward-sexp)
                (forward-sexp)))
          ;; Convert let to let* if we only had a single binding.
          (when (and (= (length let-vars) 1)
                     (eq let-keyword 'let))
            (emr-el-toggle-let*)))

        (insert (format "(%s %s)" var val))
        (when vars-after-p
          (newline-and-indent))))))

;;;###autoload
(defun emr-el-extract-to-let (symbol)
  "Extract the region or expression at point to a let-binding named SYMBOL.

* extracts the list at or around point

* if there is no enclosing let-form, inserts one at the top of
  the current context (e.g. the enclosing `defun' or `lambda' form)."
  (interactive "*SVariable name: ")
  (atomic-change-group
    (let (val-start-pos
          val-end-pos
          val)

      ;; Get the position of the expression that will be the value
      ;; for our extrcted var.
      (if (use-region-p)
          (progn
            (setq val-start-pos (region-beginning))
            (setq val-end-pos (region-end))
            (deactivate-mark))
        (save-excursion
          (emr-lisp-back-to-open-round)
          (setq val-start-pos (point))
          (setq val-end-pos (progn (forward-sexp) (point)))))

      (setq val
            (buffer-substring-no-properties val-start-pos val-end-pos))

      ;; Replace the expression with our new variable.
      (goto-char val-start-pos)
      (delete-region val-start-pos val-end-pos)
      (insert (symbol-name symbol))

      ;; If we're not inside a let, add one.
      (save-excursion
        (unless (emr-el:let-start-pos)
          (emr-el:wrap-body-form-at-point-with-let)))

      ;; Insert the new var in the let form.
      (emr-el:add-let-binding symbol val))))

; ------------------

(defun emr-el:goto-start-of-let-binding ()
  "Move to the opening paren of the let-expression at point.
Otherwise move to the previous one in the current top level form."
  (-when-let (pos (emr-el:let-start-pos))
    (when (< 0 pos)
      (goto-char pos)
      (point))))

(defun emr-el:find-in-tree (elt tree)
  "Return non-nil if ELT is in TREE."
  (cond ((equal elt tree) elt)
        ((listp tree)
         (--reduce-from (or acc (emr-el:find-in-tree elt it))
                        nil tree))))

(defun emr-el:point-sexp-index ()
  "Return the position of point in the current sexp.

For example:
  (foo| foo) => 0
  (foo fo|o) => 1"
  (let ((init-pos (point))
        (result 0)
        sexp-start sexp-end)
    (save-excursion
      ;; Find the boundaries of the containing sexp.
      (backward-up-list)
      (setq sexp-start (point))

      (forward-sexp)
      (setq sexp-end (point))

      ;; Move over the opening paren of the containing sexp.
      (goto-char (1+ sexp-start))

      ;; Move forward, counting subitems until we pass the original
      ;; point position.
      (while (and
              (< (point) init-pos)
              (< (point) sexp-end))
        (forward-sexp)
        (setq result (1+ result))))

    ;; When we stop, we've gone past the original position, so we've
    ;; overcounted by 1.
    (max (1- result)
         0)))

(defun emr-el:looking-at-let-binding-symbol? ()
  "Non-nil if point is on a binding symbol in a let-binding form."
  (when (symbol-at-point)
    (let* ((let-form
            (save-excursion
              (emr-el:goto-start-of-let-binding)
              (read (current-buffer))))
           (vars-sexp
            (-second-item let-form))
           (enclosing-form
            (save-excursion
              (ignore-errors
                (backward-up-list 1)
                (read (current-buffer)))))
           (enclosing-form-2
            (save-excursion
              (ignore-errors
                (backward-up-list 2)
                (read (current-buffer))))))
      (and let-form
           (or
            ;; If the immediately enclosing form is the vars form,
            ;; then we were in a form (let (x y| z) ...).
            (equal vars-sexp enclosing-form)
            ;; If the next enclosing form is the vars form, we are in
            ;; a form (let ((x| y)) ...). Ensure we're also at the
            ;; var, not its value.
            (and (equal vars-sexp enclosing-form-2)
                 (zerop (emr-el:point-sexp-index))))))))

(defun emr-el:let-bindings-recursively-depend? (elt bindings)
  "Non-nil if the given let BINDINGS list has recursive dependency on ELT."
  (-when-let* ((b   (--first (equal elt (emr-el:first-atom it)) bindings))
               (pos (cl-position b bindings :test 'equal)))
    (-> (-split-at (1+ pos) bindings)
        (cl-second)
        (-flatten)
        (-contains? elt))))

(cl-defun emr-el:let-binding-is-used? (symbol (_let &optional bindings &rest body))
  "Non-nil if SYMBOL is used in the body or other bindings of the given let expression."
  (or
   ;; Subsequent references in bindings list?
   (emr-el:let-bindings-recursively-depend? symbol bindings)
   ;; Body contains usage?
   (-contains? (-flatten body) symbol)))

(defun emr-el:let-bound-var-at-point-has-usages? ()
  "Non-nil if the let-bound symbol at point is referred to in the
bindings or body of the enclosing let expression."
  (and (emr-el:looking-at-let-binding-symbol?)
       (save-excursion
         (let ((sym (or (car (list-at-point))
                        (symbol-at-point))))
           (emr-el:goto-start-of-let-binding)
           (forward-symbol 1)
           (emr-el:let-binding-is-used? sym (list-at-point))))))

(defun emr-el:split-binding-string (binding-form)
  (let* ((binding-form (s-trim binding-form))
         (str (->> binding-form (s-chop-prefix "(") (s-chop-suffix ")")))
         (idx (string-match (rx (+ (or space "\n"))) binding-form)))
    (list (s-trim (substring str 0 idx))
          (s-trim (substring str idx)))))

;;;###autoload
(defun emr-el-inline-let-variable ()
  "Inline the let-bound variable at point.

EXAMPLE:

  (emr-el-inline-let-variable)

BEFORE:

  (let ((x 1)
        (y| 2))
    (+ x y))

AFTER:

  (let ((x 1))
    (+ x 2))"
  (interactive "*")
  (cl-assert (emr-el:looking-at-let-binding-symbol?))
  (save-excursion
    ;; Extract binding list.
    ;;
    ;; This will remove it from the let bindings list. We then replace all
    ;; occurences of SYM with VALUE in the scope of the current let form.
    (emr-lisp-extraction-refactor (form) "Inlined let-bound symbol"
      (cl-destructuring-bind (sym value) (emr-el:split-binding-string form)
        (save-restriction
          ;; Narrow region to the scope of the current let form.
          ;; The start is the position of the extracted binding list. This
          ;; prevents preceding bindings from being altered.
          (narrow-to-region (point)
                            (save-excursion
                              (emr-el:goto-start-of-let-binding)
                              (forward-sexp)
                              (point)))
          (goto-char (point-min))
          ;; Replace occurences of SYM with VALUE.
          (while (search-forward-regexp
                  (rx-to-string `(seq symbol-start (group-n 1 ,sym) symbol-end))
                  nil t)
            (unless (or (emr-looking-at-string?)
                        (emr-looking-at-comment?))
              (replace-match value 'fixcase t nil 1)))))))
  ;; Clean up.
  ;;
  ;; Ensure the binding list is well-formatted after removing the
  ;; binding. Perform general tidyups.
  (save-excursion
    (emr-el:join-line-after-let-binding-kill)
    (emr-el:simplify-let-form-at-point)
    (emr-lisp-reindent-defun)))

; ------------------

(defun emr-el:extract-arguments-in-usage-form (usage)
  "Given a function USAGE form, extract the arguments applied to the function."
  (with-temp-buffer
    (save-excursion (insert usage))
    ;; Move to funcall parameters.
    (forward-char)
    (if (thing-at-point-looking-at (rx (or "funcall" "apply") symbol-end))
        (forward-symbol 2)
      (forward-symbol 1))
    ;; Collect all arguments in usage.
    (let ((acc)
          (kr kill-ring))
      (unwind-protect
          (while (ignore-errors (kill-sexp) t)
            (push (s-trim (car kill-ring)) acc))
        ;; Restore kill-ring to previous state.
        (setq kill-ring kr))
      (nreverse acc))))

(cl-defun emr-el:defun-arglist-symbols ((_def _sym arglist &rest body))
  arglist)

(cl-defun emr-el:peek-forward-sexp (&optional (n 1))
  "Return the sexp N positions forward of point."
  (ignore-errors
    (let ((start (save-excursion (forward-sexp (1- n)) (point)))
          (end   (save-excursion (forward-sexp n) (point))))
      (emr-el:safe-read (buffer-substring start end)))))

(defun emr-el:defun-body-str (defun-str)
  "Extract the body forms from DEFUN-STR."
  (with-temp-buffer
    (lisp-mode)
    (save-excursion (insert defun-str))
    ;; Move past arglist.
    (forward-char)
    (forward-sexp 3)

    ;; Move past docstring.
    (when (and (stringp (emr-el:peek-forward-sexp))
               (emr-el:peek-forward-sexp 2))
      (forward-sexp))

    ;; Move past `declare' and `interactive' forms.
    (while (-contains? '(declare interactive) (car-safe (emr-el:peek-forward-sexp)))
      (forward-sexp))

    ;; Everything from here is the body. Delete everything prior to this point.
    (paredit-splice-sexp-killing-backward)
    (buffer-string)))

(defun emr-el:transform-function-usage (def usage)
  "Replace the USAGE of a function with the body from its DEF.
Its variables will be let-bound."
  (let* ((params (->> (emr-el:safe-read def)
                   (emr-el:defun-arglist-symbols)
                   (--remove (-contains? emr-el:special-symbols it))))
         (args (emr-el:extract-arguments-in-usage-form usage))
         ;; Join the function arglist and funcall arguments to create a
         ;; let-bindings list.
         (bindings (or (->> (-zip params args) (-map 'list-utils-make-proper))
                       "()"))
         (body (emr-el:defun-body-str def)))
    (with-temp-buffer
      (lisp-mode)
      (save-excursion
        (insert (format "(let %s\n %s)" bindings body)))
      (emr-el:simplify-let-form-at-point)
      (emr-lisp-reindent-defun)
      (buffer-string))))

(defun emr-el:defun-at-point-has-body ()
  (not (s-blank-str? (emr-el:defun-body-str (thing-at-point 'defun)))))

;;;###autoload
(defun emr-el-inline-function ()
  "Replace usages of a function with its body forms.
Replaces all usages in the current buffer."
  (interactive "*")
  ;; Warn the user if the defun at point has an empty body. Prompt before
  ;; continuing.
  (when (or (emr-el:defun-at-point-has-body)
            (y-or-n-p "Warning: This function has no body.  Continue? "))
    (atomic-change-group
      (save-excursion

        ;; Extract the whole defun at point.
        (beginning-of-defun)
        (emr-lisp-extraction-refactor (def) "Inlined function"

          ;; There will now be a blank line where the defun used to be. Join
          ;; lines to fix this.
          (emr-collapse-vertical-whitespace)

          (let ((fname (nth 1 (s-split (rx space) def)))
                ;; Tracks the line numbers where inlinings are performed.
                (modified-lines))

            (goto-char (point-min))

            ;; Search the buffer for function usages.
            ;;
            ;; Functions can be called directly with the function name in
            ;; the car of a list, or indirectly using funcall/apply. Handle
            ;; all these cases.
            (while (search-forward-regexp
                    (rx-to-string `(seq "("
                                        ;; Optional use of apply/funcall.
                                        (? (or "apply" "funcall")
                                           (+ (any space "\n" "\t"))
                                           "'")
                                        ;; Usage of name.
                                        ,fname symbol-end))
                    nil t)
              ;; Move to start of the usage form.
              (search-backward "(")

              ;; Inline the function at and update the `modified-lines' list.
              (emr-lisp-extraction-refactor (usage) "Replace usage"
                (push (line-number-at-pos) modified-lines)
                (insert (emr-el:transform-function-usage def usage))
                (emr-lisp-reindent-defun)))

            ;; Report inlining count to the user.
            (if modified-lines
                (let* ((n (length modified-lines))
                       (s (if (equal 1 n) "" "s")))
                  (message "%s replacement%s performed at line%s: %s" n s s
                           (->> modified-lines
                             (reverse)
                             (-map 'number-to-string)
                             (s-join ", "))))
              ;; Abort if no changes were made to the buffer. This will revert
              ;; the buffer text to its state before the extraction.
              (error "No usages found"))))))))

; ------------------

(defun emr-el:def-name (definition-form)
  "Given a DEFINITION-FORM such as defvar/defun/..., return its name."
  (let* ((form-name (nth 1 definition-form)))
    (when (symbolp form-name)
      form-name)))

(defun emr-el:def-find-usages (definition-form)
  "Find the usages for a given DEFINITION-FORM symbol.

Returns a list of conses, where the car is the line number and
the cdr is the usage form."
  (-when-let (sym (emr-el:def-name definition-form))
    ;; Search the buffer for usages of `sym'. Remove the definition form
    ;; from the results.
    (let (acc)
      (save-excursion
        (goto-char (point-min))
        (while (search-forward-regexp
                (rx-to-string `(seq symbol-start ,(symbol-name sym) symbol-end))
                nil t)
          (-when-let (form (list-at-point))
            (unless (equal definition-form form)
              ;; Add this usage to `acc', unless it is the original definition.
              (push (cons (line-number-at-pos) form) acc)))))
      (nreverse acc))))

;;;###autoload
(defun emr-el-delete-unused-definition ()
  "Delete the definition form at point if it does not have usages."
  (interactive "*")
  (unless (emr-el:def-find-usages (list-at-point))
    (save-excursion
      (beginning-of-thing 'defun)
      (kill-sexp)

      (emr-collapse-vertical-whitespace))))

;;; `emr-el-ref': A reference to a function or variable within a file.
(cl-defstruct emr-el-ref file line col identifier type form)

(defun emr-el:interactive-form-p (form)
  "Does FORM contain an (interactive) expression?"
  ;; (defun foo () x y ...) -> (x y ...)
  (let ((body (-drop 3 form)))
    ;; Ignore docstring, if present.
    (when (stringp (car body))
      (setq body (-drop 1 body)))

    (eq (car-safe (car body))
        'interactive)))

(defun emr-el:find-unused-defs ()
  "Return a list of all unused definitions in the buffer.
The result is a list of `emr-el-ref'."
  (save-excursion
    (let (acc)
      (goto-char (point-min))

      ;; Find definitions in this buffer.
      ;;
      ;; This will search the buffer for known defun forms. As a special
      ;; cases, forms with a preceding autoload directive are ignored.
      (while (search-forward-regexp
              (rx-to-string `(seq "(" (or ,@(-map 'symbol-name emr-el-definition-macro-names))
                                  symbol-end))
              nil t)
        (unless (or (emr-looking-at-string?)
                    (emr-looking-at-comment?)
                    (emr-el:autoload-directive-exsts-above-defun?))
          ;; Collect definitions that do not have usages.
          (-when-let* ((form (list-at-point))
                       (col  (save-excursion
                               (emr-el:beginning-of-defun)
                               (current-column))))
            (unless (or
                     ;; Consider interactive forms to be used.
                     (emr-el:interactive-form-p form)
                     (emr-el:def-find-usages form))
              (push
               (make-emr-el-ref :file (buffer-file-name)
                                :line (line-number-at-pos)
                                :col  col
                                :type (car form)
                                :identifier (nth 1 form)
                                :form form)
               acc)))))
      (nreverse acc))))

(define-compilation-mode emr-buffer-report-mode "EMR Report"
  "EMR buffer report compilation mode."
  (set (make-local-variable 'truncate-lines) t)
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'compilation-error-face) compilation-info-face))

;; TODO: This is fooled by recursive functions.
;;;###autoload
(defun emr-el-find-unused-definitions ()
  "Search the buffer for functions and variables that have no usages.
Definitions with export directives are ignored.  If any unused
definitions are found, they will be collated and displayed in a
popup window."
  (interactive)

  (let ((defs-buf (get-buffer-create "*Unused Definitions*")))
    ;; Find unused refs. If there are none, delete any windows showing `defs-buf'.
    (-if-let (defs (emr-el:find-unused-defs))

      ;; Show results window.
      ;;
      ;; The results buffer uses a custom compilation mode so the user can
      ;; navigate to unused declarations.
      (with-help-window defs-buf
        (let ((header (format "Unused definitions in %s:\n\n" (buffer-file-name))))
          (with-current-buffer defs-buf
            (atomic-change-group
              (emr-buffer-report-mode)
              ;; Prepare buffer.
              (read-only-mode -1)
              (delete-region (point-min) (point-max))
              (insert header)
              ;; Insert usages.
              (->> defs
                (--map (format
                        "%s:%s:%s:%s: %s"
                        (file-name-nondirectory (emr-el-ref-file it))
                        (emr-el-ref-line it)
                        (emr-el-ref-col  it)
                        (emr-el-ref-type it)
                        (format "%s" (emr-el-ref-identifier it))))
                (s-join "\n\n")
                (insert))

              ;; Insert summary.
              (newline 2)
              (insert (format
                       "Finished. %s item%s found."
                       (length defs)
                       (if (equal 1 (length defs)) "" "s")))
              (read-only-mode +1)))))

      ;; No results to show. Clean results window.
      (progn
        (message "No unused definitions found")
        (-when-let (win (get-window-with-predicate
                         (lambda (w) (equal defs-buf (window-buffer w)))))
          (delete-window win)
          (kill-buffer defs-buf))))))

; ------------------

;;;; EMR declarations

(emr-declare-command 'emr-el-toggle-let*
  :title "toggle let/let*"
  :modes 'emacs-lisp-mode
  :predicate #'emr-el:let-start-pos)

(emr-declare-command 'emr-el-implement-function
  :title "implement function"
  :modes 'emacs-lisp-mode
  :predicate (lambda ()
               (and (symbol-at-point)
                    (not (emr-looking-at-string?))
                    (not (emr-looking-at-comment?))
                    (not (thing-at-point 'number))
                    (not (emr-el:looking-at-definition?))
                    (not (emr-el:looking-at-let-binding-symbol?))
                    (not (boundp (symbol-at-point)))
                    (not (fboundp (symbol-at-point))))))

(emr-declare-command 'emr-el-inline-variable
  :title "inline"
  :modes 'emacs-lisp-mode
  :predicate (lambda ()
               (and (emr-el:variable-definition? (list-at-point))
                    (emr-el:def-find-usages (list-at-point)))))

(emr-declare-command 'emr-el-inline-function
  :title "inline"
  :modes 'emacs-lisp-mode
  :predicate (lambda ()
               (and (emr-el:function-definition? (list-at-point))
                    (emr-el:def-find-usages (list-at-point)))))

(emr-declare-command 'emr-el-extract-function
  :title "extract function"
  :description "defun"
  :modes 'emacs-lisp-mode
  :predicate (lambda ()
               (not (or (emr-el:looking-at-definition?)
                        (emr-el:looking-at-let-binding-symbol?)))))

(emr-declare-command 'emr-el-extract-variable
  :title "extract variable"
  :description "defvar"
  :modes 'emacs-lisp-mode
  :predicate (lambda ()
               (and (not (emr-el:looking-at-definition?))
                    (not (emr-el:looking-at-let-binding-symbol?))
                    (thing-at-point 'defun))))

(emr-declare-command 'emr-el-extract-constant
  :title "extract constant"
  :description "defconst"
  :modes 'emacs-lisp-mode
  :predicate (lambda ()
               (not (or (emr-el:looking-at-definition?)
                        (emr-el:looking-at-let-binding-symbol?)))))

(emr-declare-command 'emr-el-extract-to-let
  :title "extract to let"
  :description "let"
  :modes 'emacs-lisp-mode
  :predicate (lambda ()
               (not (or (emr-el:looking-at-definition?)
                        (emr-el:looking-at-decl?)
                        (emr-el:looking-at-let-binding-symbol?)))))

(emr-declare-command 'emr-el-delete-let-binding-form
  :title "delete binding"
  :description "unused"
  :modes 'emacs-lisp-mode
  :predicate (lambda ()
               (and (emr-el:looking-at-let-binding-symbol?)
                    (not (emr-el:let-bound-var-at-point-has-usages?)))))

(emr-declare-command 'emr-el-inline-let-variable
  :title "inline binding"
  :modes 'emacs-lisp-mode
  :predicate (lambda ()
               (and (emr-el:looking-at-let-binding-symbol?)
                    (emr-el:let-bound-var-at-point-has-usages?))))

(emr-declare-command 'emr-el-extract-autoload
  :title "add autoload"
  :description "autoload"
  :modes 'emacs-lisp-mode
  :predicate (lambda ()
               (let ((sym (symbol-at-point)))
                 (and (not (emr-el:autoload-exists? sym (buffer-string)))
                      (not (emr-el:looking-at-definition?))
                      (not (emr-el:variable-definition? (list-at-point)))
                      (or (functionp sym)
                          (macrop sym))
                      ;; Don't offer autoload if this function is
                      ;; defined in the current file.
                      (not (equal
                            (cdr-safe (find-function-library sym))
                            (buffer-file-name)))))))

(emr-declare-command 'emr-el-insert-autoload-directive
  :title "autoload cookie"
  :description "directive"
  :modes 'emacs-lisp-mode
  :predicate (lambda ()
               (and (emr-el:looking-at-definition?)
                    (not (emr-el:autoload-directive-exsts-above-defun?)))))

(emr-declare-command 'emr-el-eval-and-replace
  :title "eval and replace"
  :description "value"
  :modes 'emacs-lisp-mode
  :predicate (lambda ()
               (not (or (emr-el:looking-at-definition?)
                        (emr-el:looking-at-let-binding-symbol?)))))

(emr-declare-command 'emr-el-tidy-autoloads
  :title "tidy"
  :description "autoloads"
  :modes 'emacs-lisp-mode
  :predicate (lambda ()
               (thing-at-point-looking-at
                (rx bol (* space) "(autoload " (* nonl)))))

(emr-declare-command 'emr-el-delete-unused-definition
  :title "delete"
  :description "unused"
  :modes 'emacs-lisp-mode
  :predicate (lambda ()
               (and (emr-el:looking-at-definition?)
                    (not (emr-el:autoload-directive-exsts-above-defun?))
                    (not (emr-el:def-find-usages (list-at-point))))))


(defun emr-el:looking-at-symbol-p ()
  "Is point looking at an unquoted symbol?"
  (save-excursion
    (-let (((_ _ _ in-string in-comment . _) (syntax-ppss))
           (sym-bounds (bounds-of-thing-at-point 'symbol))
           (sexp nil))
      (when (and
             (not in-string)
             (not in-comment)
             sym-bounds)
        (goto-char (car sym-bounds))
        (setq sexp (read (current-buffer)))

        (and
         (symbolp sexp)
         (not (null sexp))
         (not (keywordp sexp)))))))

(defun emr-el:looking-at-local-var-p ()
  "Is point looking at a symbol for a locally bound variable?"
  (when (emr-el:looking-at-symbol-p)
    (let ((sym (symbol-at-point))
          form)
      (save-excursion
        (emr-el:beginning-of-defun)
        (setq form (read (current-buffer)))
        (memq sym (emr-el:bound-variables form))))))

;;;; Setup

(defun emr-el:show-menu ()
  (easy-menu-add-item
   nil
   '("EMR")
   ["Find unused definitions" emr-el-find-unused-definitions]))

;;;###autoload
(defun emr-el-initialize ()
  "Enable the EMR menu for Elisp buffers."
  (add-hook 'emacs-lisp-mode-hook 'emr-el:show-menu)
  (--each (buffer-list)
    (with-current-buffer it
      (when (derived-mode-p 'emacs-lisp-mode)
        (emr-el:show-menu)))))

(provide 'emr-elisp)

;;; emr-elisp.el ends here
