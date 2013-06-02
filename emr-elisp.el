;;; emr-elisp.el --- Refactoring commands for Emacs Lisp

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130531.2226

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
(autoload 'paredit-splice-sexp-killing-backward "paredit")
(autoload 'redshank-letify-form-up "redshank")

(defcustom emr-el-lines-between-toplevel-forms 1
  "The number of lines to try to preserve between functions and vars when refactoring Elisp."
  :group 'emr)

(defun emr-el:collapse-vertical-whitespace ()
  "Collapse blank lines around point.
Ensure there are at most `emr-el-lines-between-toplevel-forms' blanks."
  (cl-flet ((this-line () (buffer-substring (line-beginning-position) (line-end-position))))
    (when (emr-blank? (this-line))
      (save-excursion
        ;; Delete blank lines.
        (search-backward-regexp (rx (not (any space "\n"))) nil t)
        (forward-line 1)
        (while (emr-blank? (this-line))
          (forward-line)
          (join-line))
        ;; Open a user-specified number of blanks.
        (open-line emr-el-lines-between-toplevel-forms)))))

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

(defun emr-el:goto-open-round ()
  "Move to the opening paren for the Lisp list at point."
  (interactive)
  (when (or (not (equal "(" (thing-at-point 'char)))
            (emr-looking-at-string?))
    (beginning-of-sexp)
    (unless (equal "(" (thing-at-point 'char))
      (search-backward "("))))

(defun emr-el:goto-open-round-or-quote ()
  "Move to the opening paren or quote for the Lisp list at point."
  (interactive)
  (emr-el:goto-open-round)
  (when (or (thing-at-point-looking-at "'")
            (thing-at-point-looking-at "`")
            (emr-looking-at-string?))
    (search-backward-regexp (rx (or "'" "`")))))

;;;; Formatting commands

(defun emr-el:reindent-defun ()
  "Reindent the current top level form."
  (save-excursion (end-of-defun) (beginning-of-defun) (indent-sexp)))

(defun emr-el:reindent-string (form-str)
  "Reformat FORM-STR, assuming it is a Lisp fragment."
  (with-temp-buffer
    (lisp-mode-variables)
    (insert form-str)
    (emr-el:reindent-defun)
    (buffer-string)))

(defun emr-el:insert-above-defun (form-str)
  "Insert and indent FORM-STR above the current top level form.
Return the position of the end of FORM-STR."
  (emr-insert-above-defun (emr-el:reindent-string form-str)))

(defun emr-el:symbol-file-name (fn)
  "Find the name of the file that declares function FN."
  (-when-let (file (find-lisp-object-file-name fn (symbol-function fn)))
    (and (stringp file)
         (file-name-nondirectory (file-name-sans-extension file)))))

(defvar emr-el:special-symbols
  '(--cl-rest-- &rest &optional &key &allow-other-keys \,\@ \,)
  "A list of symbols that should be ignored by variable searches.")

(defun* emr-el:bindings-in-lambda ((_lam arglist &rest body))
  "Return all bound variables within a lambda form."
  (let ((bs (-difference arglist emr-el:special-symbols)))
    (-concat bs (emr-el:bound-variables body))))

(defun* emr-el:bindings-in-let ((_let bindings &rest body))
  "Return the list of bound values in the given `let' or `let*' expression."
  (-concat (emr-el:let-binding-list-symbols bindings)
           (emr-el:bound-variables body)))

(defun* emr-el:bindings-in-defalias ((_def (_quote sym) func))
  "Return the bindings in a defalias form, including the named alias."
  (cons sym (emr-el:bound-variables func)))

(defun emr-el:bound-variables (form)
  "Find the list of let- or lambda-bound variables in FORM."
  (-uniq
   (let* (
          ;; Handle errors in expansion. Expansion errors are common with syntax
          ;; quotes, for example.
          (form (or (ignore-errors (macroexpand-all form))
                    (ignore-errors (macroexpand form))
                    form))

          (hd (car-safe form))
          )
     (cond
      ((equal 'lambda hd) (emr-el:bindings-in-lambda form))
      ((equal 'let hd)  (emr-el:bindings-in-let form))
      ((equal 'let* hd) (emr-el:bindings-in-let form))
      ((equal 'defalias hd) (emr-el:bindings-in-defalias form))
      ;; FUNCTION is the quotation form for function objects.
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
         (-mapcat 'emr-el:bound-variables)))))))

(defun emr-el:free-variables (form &optional context)
  "Try to find the symbols in FORM that do not have variable bindings.
CONTEXT is the top level form that encloses FORM."

  ;; Marco-expand FORM and find the list of bound symbols. Diff this with the
  ;; other symbols in FORM. Figure out which ones are not functions, keywords,
  ;; special vars, etc. This should give a pretty good idea of which symbols are
  ;; 'free'.

  (let ((bound-vars (emr-el:bound-variables form))
        (ctx-bound (emr-el:bound-variables context))
        (form-syms (->> form (list) (-flatten) (-filter 'symbolp)))
        )
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
                       (symbol-function it))))))))

;;;; Refactoring Macros

(defmacro* emr-el:extraction-refactor ((&optional binding) description &rest body)
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
         (emr-el:goto-open-round-or-quote)
         (kill-sexp))

       (emr-el:reindent-defun)

       (let
           ;; Define BINDING if supplied.
           ,(when binding `((,binding (s-trim (car kill-ring)))))

         ;; Revert kill-ring pointer.
         (setq kill-ring (cdr kill-ring))
         (save-excursion
           (emr-reporting-buffer-changes ,description
             ,@body))))))

;;;; Definition site tests

(defun emr-el:macro-boundp (symbol)
  "Test whether SYMBOL is bound as a macro."
  (and (fboundp symbol)
       (eq (car (symbol-function symbol)) 'macro)))

(defun emr-el:macro-definition? (form)
  "Return t if FORM expands to a macro definition."
  (ignore-errors
    (let ((exp (macroexpand-all form)))
      ;; yo dawg I herd you like cars
      (and (equal 'defalias (car exp))
           (equal 'macro (cadar (cdaddr exp)))))))

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
  (or (emr-el:definition? (list-at-point))
      (-when-let (def (read (thing-at-point 'defun)))
        (emr-el:find-in-tree (list-at-point) (cl-third def)))))

(defun emr-el:autoload-exists? (function str)
  "Returns true if an autoload for FUNCTION exists in string STR."
  (s-contains? (format "(autoload '%s " function) str))

;;;; Refactoring commands

(defun emr-el:extract-var-values (sexp)
  "Return the name and initializing value of SEXP if it is a variable definition."
  (let ((exp (macroexpand-all sexp)))
    (when (emr-el:variable-definition? exp)
      (cl-destructuring-bind (_def sym &rest forms) exp
          (cons sym (car forms))))))

(defun* emr-el:replace-usages ((sym . value))
  "Replace all instances of SYM with VALUE in the current buffer.
Returns a list of lines where changes were made."
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (let ((match-sym (eval `(rx (not (any "(")) (* space)
                                  (group symbol-start ,(symbol-name sym)
                                         symbol-end))))
            (lines))
        ;; Check for "(" since we don't want to replace function calls.
        (while (and (search-forward-regexp match-sym nil t)
                    (match-data))
          (setq lines (cons (line-number-at-pos) lines))
          ;; Perform replacement.
          (replace-match (emr-el:print value) t nil nil 1)
          (emr-el:reindent-defun))
        (nreverse lines)))))

;;;###autoload
(defun emr-el-inline-variable ()
  "Inline the variable defined at point.
Uses of the variable are replaced with the initvalue in the variable definition."
  (interactive "*")
  (save-excursion
    (emr-el:goto-open-round)
    (-if-let (def (emr-el:extract-var-values (list-at-point)))
      (if (or (consp def) (> (length def) 1))
          (emr-el:extraction-refactor () "Inlining applied at"

            ;; Clean up line spacing.
            (while (s-blank? (buffer-substring-no-properties (line-beginning-position)
                                                             (line-end-position)))
              (kill-line))

            ;; Perform inlining.
            ;; emr-el:extraction-refactor will report the first insertion. If
            ;; there are none or more than one insertion, override this report.
            (-if-let (lines (-map 'int-to-string (emr-el:replace-usages def)))
              (when (> (length lines) 1)
                (message "Inlining applied at lines %s" (s-join ", " lines)))
              (error "No usages found")))

        (error "No value to inline for %s" (car def)))
      (error "Not a variable definition"))))

; ------------------

(defun emr-el:eval-and-print-progn (prog)
  "Eval and print each form in sexp PROG."
  (->> (macroexp-unprogn prog)
    (-map 'eval)
    (-remove 'null)
    (-map 'emr-el:print)))

;;;###autoload
(defun emr-el-eval-and-replace ()
  "Replace the current region or the form at point with its value."
  (interactive "*")
  (emr-el:extraction-refactor (sexp) "Replacement at"
    (insert (->> (read sexp)
              (emr-el:eval-and-print-progn)
              (s-join "\n")))
    (indent-for-tab-command)
    (emr-el:reindent-defun)))

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
        (read (format "(%s)" arglist))
      (error
       ;; Rethrow reader errors as something more informative.
       (error "Malformed arglist")))))

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
      (buffer-substring (region-beginning) (region-end))
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
  "Extract a function, using the current region or form point as the body.
NAME is the name of the new function.
ARGLIST is its argument list."
  (interactive (list (read-string "Name: ")
                     ;; Prompt user with default arglist.
                     (emr-el:read-args (emr-el:form-extent-for-extraction)
                                       (thing-at-point 'defun))))

  (cl-assert (not (s-blank? name)) () "Name must not be blank")

  (emr-el:extraction-refactor (sexp) "Extracted to"
    (let ((name (intern name))
          (defun-form (if (-any? 'listp arglist) 'defun* 'defun))
          (body (emr-el:unprogn sexp)))
      ;; Insert usage at point.
      (insert (emr-el:print (cl-list* name arglist)))
      ;; Insert defun.
      (->> (format "(%s %s %s\n  %s)" defun-form name arglist body)
        (emr-el:format-defun)
        (emr-el:insert-above-defun)))))

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
The function will be called NAME and have the given ARGLIST."
  (interactive (list
                (read (emr-el:read-with-default "Name" (symbol-at-point)))
                ;; Infer arglist from usage.
                (->> (list-at-point)
                  (emr-el:infer-arglist-for-usage)
                  (-map 'symbol-name)
                  (s-join " ")
                  (s-trim)
                  (emr-el:read-with-default "Arglist")
                  (emr-el:format-submitted-arglist))))
  ;; Determine which defun form to use.
  (let ((defun-form (if (-any? 'listp arglist) 'defun* 'defun))
        pos)

    ;; Insert usage and defun, then move to the point to the body of the defun.

    (save-excursion
      ;; Mark whole list at point.
      (beginning-of-thing 'sexp)
      (mark-sexp)

      (emr-el:extraction-refactor ()  "Defined function"

        ;; Insert reference. Quote the symbol if it's not in the funcall
        ;; position.
        (if (thing-at-point-looking-at "(")
            (insert (format "%s" name))
          (insert (format "'%s" name)))

        ;; Insert definition.
        (setq pos (->> (format "(%s %s %s\n  )" defun-form name arglist)
                    (emr-el:format-defun)
                    (emr-el:insert-above-defun)))))

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
The variable will be called NAME."
  (interactive "*sName: ")
  (cl-assert (not (s-blank? name)) () "Name must not be blank")
  (emr-el:extraction-refactor (sexp) "Extracted to"
    ;; Insert usage.
    (insert (s-trim name))
    ;; Insert definition.
    (emr-el:insert-above-defun (format "(defvar %s %s)" name sexp))))

;;;###autoload
(defun emr-el-extract-constant (name)
  "Extract the current region or form at point to a constant special variable.
The variable will be called NAME."
  (interactive "*sName: ")
  (cl-assert (not (s-blank? name)) () "Name must not be blank")
  (emr-el:extraction-refactor (sexp) "Extracted to"
    ;; Insert usage
    (insert (s-trim name))
    ;; Insert definition.
    (emr-el:insert-above-defun (format "(defconst %s %s)" name sexp))))

;;;###autoload
(defun emr-el-extract-autoload (function file)
  "Create an autoload for FUNCTION.
FILE is the file that declares FUNCTION.
See `autoload' for details."
  (interactive
   (let* ((sym  (intern (or (thing-at-point 'symbol) (read-string "Function: "))))
          (file (or (emr-el:symbol-file-name sym)
                    (read-string "File: "))))
     (list sym file)))

  ;; Bail if there is already an autoload for that symbol.
  (if (emr-el:autoload-exists? (symbol-at-point) (buffer-string))
      (error "Autoload already exists")

    (let ((form `(autoload ',function ,file)))
      (save-excursion
        (emr-reporting-buffer-changes "Extracted to"
          ;; Put the extraction next to existing autoloads if any, otherwise
          ;; insert above top-level form.
          (if (emr-el:goto-first-match "^(autoload ")
              (progn (forward-line 1) (end-of-line) (newline)
                     (insert (emr-el:print form)))
            (emr-el:insert-above-defun
             (emr-el:print form))))))))

;;;###autoload
(defun emr-el-comment-form ()
  "Comment out the current region or from at point."
  (interactive "*")
  (if (region-active-p)
      (comment-region (region-beginning)
                      (region-end))
    (emr-el:goto-open-round-or-quote)
    (mark-sexp)
    (comment-region (region-beginning) (region-end))))

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

(defun emr-el:let-binding-list-values (binding-forms)
  "Return the values in the given let BINDING-FORMS."
  (-map 'cdr-safe binding-forms))

(defun* emr-el:let-binding-list ((_let &optional bindings &rest body))
  "Return the bindings list in the given let form."
  bindings)

(defun* emr-el:let-body ((_let &optional _bindings &rest body))
  "Return the body forms in the given let form."
  body)

(defconst emr-el:scope-boundary-forms
  '(lambda defun cl-defun defun* defmacro cl-defmacro defmacro*
    let let* save-excursion unwind-protect
    flet cl-flet cl-flet* cl-labels labels)
  "A list of forms that define some kind of scope or context.
They will bound upward searches when looking for places to insert let forms.")

(defun emr-el:clean-let-form-at-point ()
  "Tidy the let form at point.
If it has no bindings, splice its contents into the surrounding
form or replace with `progn'."
  (save-excursion
    (emr-el:goto-start-of-let-binding)
    ;; Move into list.
    (forward-char 1)
    (let ((bindings (emr-el:let-binding-list (list-at-point)))
          (body     (emr-el:let-body (list-at-point))))
      ;; Move to after bindings list.
      (forward-list 1)
      (cond
       ;; Splice contents in directly if the let body has only a single form.
       ((and (null bindings) (>= 1 (length body)))
        (paredit-splice-sexp-killing-backward))

       ;; Splice contents into surrounding form in if it has an &body
       ;; parameter.
       ((and (null bindings)
             (-contains? (cons 'progn emr-el:scope-boundary-forms)
                         (emr-el:peek-back-upwards)))
        (backward-kill-sexp 2))

       ;; Otherwise replace `let' with `progn'.
       ((null bindings)
        (backward-kill-sexp 2)
        (insert "progn"))))
    (emr-el:reindent-defun)))

;;;###autoload
(defun emr-el-delete-let-binding-form ()
  "Delete the let binding around point."
  (interactive "*")
  (cl-assert (emr-el:looking-at-let-binding-symbol?))
  (let ((kr kill-ring))
    (unwind-protect
        (save-excursion
          ;; Delete binding.
          (emr-el:goto-open-round)
          (kill-sexp)

          ;; Ensure whole form is correctly reindented.
          (mark-defun)
          (indent-region (region-beginning) (region-end)))

      ;; Restore kill-ring.
      (setq kill-ring kr)
      (emr-el:clean-let-form-at-point))))

; ------------------

(defun emr-el:find-upwards (sym)
  "Search upwards from POINT for an enclosing form beginning with SYM."
  (save-excursion
    (cl-loop
     while (ignore-errors (backward-up-list) t)
     when (thing-at-point-looking-at
           (eval `(rx "(" ,(format "%s" sym) symbol-end)))
     do (return (point)))))

(defun emr-el:peek-back-upwards ()
  "Return the car of the enclosing form."
  (save-excursion
    (when (ignore-errors (backward-up-list) t)
      (forward-char 1)
      (sexp-at-point))))

(defun emr-el:goto-containing-body-form ()
  "Search upwards for the first function or macro declaration enclosing point.
Move to that body form that encloses point."
  (cl-loop
   while (ignore-errors (backward-up-list) t)
   do (when (-contains? emr-el:scope-boundary-forms (emr-el:peek-back-upwards))
        (return (point)))))

(defun emr-el:wrap-body-form-at-point-with-let ()
  "Search upward for an enclosing LET statement. If one is not found,
wrap the form with a let statement at a sensible place."
  (emr-el:goto-containing-body-form)
  ;; Wrap with empty let statement.
  (insert "(")
  (save-excursion
    (forward-sexp)
    (insert ")"))
  (insert "let ()\n  ")
  (emr-el:reindent-defun))

;;;###autoload
(defun emr-el-extract-to-let (symbol)
  "Extract the region or expression at point to a let-binding named SYMBOL.

* extracts the list at or around point

* if there is no enclosing let-form, inserts one at the top of
  the current context (e.g. the enclosing `defun' or `lambda' form)."
  (interactive "*SVariable name: ")
  (atomic-change-group
    (save-excursion
      (let (did-wrap-form?)

        ;; Wrap with a let-form if one does not exist.
        ;;
        ;; Redshank provides its own wrapping logic, but it wraps only the
        ;; sexp it's extracting. Instead, we want the let form to be as close to the
        ;; containing defun as possible.
        (save-excursion
          (unless (or (emr-el:find-upwards 'let)
                      (emr-el:find-upwards 'let*))
            (emr-el:wrap-body-form-at-point-with-let)
            (setq did-wrap-form? t)))

        ;; Extract the form.
        ;;
        ;; Redshank extracts by killing forward, so start from the beginning of
        ;; the list or region.
        (if (region-active-p)
            (goto-char (region-beginning))
          (emr-el:goto-open-round))
        (redshank-letify-form-up (symbol-name symbol))

        ;; Tidy let binding after insertion.
        ;;
        ;; Redshank leaves an extra newline when inserting into an empty
        ;; let-form. Find that let-form and remove the extra newline.
        (when did-wrap-form?
          (goto-char (emr-el:find-upwards 'let))
          (end-of-line)
          (forward-char)
          (join-line))))))

; ------------------

(defun emr-el:goto-start-of-let-binding ()
  "Move to the opening paren of the let-expression at point.
  Otherwise move to the previous one in the current top level form."
  (cl-flet ((max-safe (&rest ns) (apply 'max (--map (or it 0) ns))))

    (-when-let (pos (max-safe (emr-el:find-upwards 'let)
                              (emr-el:find-upwards 'let*)))
      (when (< 0 pos)
        (goto-char pos)
        (point)))))

(defun emr-el:find-in-tree (elt tree)
  "Return non-nil if ELT is in TREE."
  (cond ((equal elt tree) elt)
        ((listp tree)
         (--reduce-from (or acc (emr-el:find-in-tree elt it))
                        nil tree))))

(defun emr-el:looking-at-let-binding-symbol? ()
  "Non-nil if point is on a binding symbol in a let-binding form."
  (when (symbol-at-point)
    (ignore-errors
      (let ((maybe-binding-list
             (save-excursion
               (emr-el:goto-open-round)
               (list-at-point))))
        (save-excursion
          ;; Select binding list for the let expression.
          (emr-el:goto-start-of-let-binding)
          (let ((bindings (progn
                            ;; Move inside let form.
                            (forward-char 1)
                            (emr-el:let-binding-list (list-at-point)))))
            (equal maybe-binding-list bindings)))))))

(defun emr-el:let-bindings-recursively-depend? (elt bindings)
  "Non-nil if the given let bindings list has recursive dependency on ELT."
  (-when-let* ((b   (--first (equal elt (emr-el:first-atom it)) bindings))
               (pos (cl-position b bindings :test 'equal)))
    (-> (-split-at (1+ pos) bindings)
      (cl-second)
      (-flatten)
      (-contains? elt))))

(defun* emr-el:let-binding-is-used? (symbol (_let &optional bindings &rest body))
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
  "Inline the let-bound variable at point."
  (interactive "*")
  (cl-assert (emr-el:looking-at-let-binding-symbol?))
  (save-excursion
    ;; Extract binding list.
    ;;
    ;; This will remove it from the let bindings list. We then replace all
    ;; occurences of SYM with VALUE in the scope of the current let form.
    (emr-el:extraction-refactor (form) "Inlined let-bound symbol"
      (destructuring-bind (sym value) (emr-el:split-binding-string form)
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
          ;; Replace occurence os SYM with VALUE.
          (while (search-forward-regexp
                  (eval `(rx symbol-start (group-n 1 ,sym) symbol-end))
                  nil t)
            (unless (or (emr-looking-at-string?)
                        (emr-looking-at-comment?))
              (replace-match value 'fixcase t nil 1)))))))
  ;; Clean up.
  ;;
  ;; The binding has been deleted, leaving a blank line. Join with the
  ;; previous line to clean up.
  (save-excursion
    (forward-char 1)
    (join-line)
    (emr-el:clean-let-form-at-point)
    (emr-el:reindent-defun)))

; ------------------

(defun emr-el:extract-arguments-in-usage-form (usage)
  "Given a function usage form, extract the arguments applied to the function."
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

(defun* emr-el:defun-arglist-symbols ((_def _sym arglist &rest body))
  arglist)

(defun emr-el:defun-body-str (defun-str)
  "Extract the body forms from DEFUN-STR."
  (with-temp-buffer
    (save-excursion (insert defun-str))
    ;; Move past arglist.
    (forward-char)
    (forward-sexp 3)
    (->> (buffer-substring (point) (point-max))
      (s-trim)
      (s-chop-suffix ")"))))

(defun emr-el:transform-function-usage (def usage)
  "Replace the usage of a function with the body from its definition.
Its variables will be let-bound."
  (let* ((params (->> (read def)
                   (emr-el:defun-arglist-symbols)
                   (--remove (-contains? emr-el:special-symbols it))))
         (args (emr-el:extract-arguments-in-usage-form usage))
         ;; Join the function arglist and funcall arguments to create a
         ;; let-bindings list.
         (bindings (or (->> (-zip params args) (-map 'list-utils-make-proper))
                       "()"))
         (body (emr-el:defun-body-str def)))
    (with-temp-buffer
      (save-excursion
        (insert (format "(let %s\n %s)" bindings body)))
      (emr-el:clean-let-form-at-point)
      (buffer-string))))

;;;###autoload
(defun emr-el-inline-function ()
  "Replace usages of a function with its body forms.
Replaces all usages in the current buffer."
  (interactive "*")
  (atomic-change-group
    (save-excursion
      ;; Extract definition.
      (beginning-of-defun)
      (emr-el:extraction-refactor (def) "Inlined function"

        (let ((fname (nth 1 (s-split (rx space) def)))
              (did-perform-insertions?))

          (goto-char (point-min))

          ;; Search the buffer for function usages.
          (while (search-forward-regexp
                  (eval `(rx "("
                             ;; Optional use of apply/funcall.
                             (? (or "apply" "funcall")
                                (+ (any space "\n" "\t"))
                                "'")
                             ;; Usage of name.
                             ,fname symbol-end))
                  nil t)
            ;; Move to start of the usage form.
            (search-backward "(")
            (emr-el:extraction-refactor (usage) "Replace usage"
              (insert (emr-el:transform-function-usage def usage)))
            (setq did-perform-insertions? t))

          ;; Bail if no changes were made to the buffer.
          (unless did-perform-insertions?
            (error "No usages found")))))

    ;; There will now be a blank line where the defun used to be. Join
    ;; lines to fix this.
    (emr-el:collapse-vertical-whitespace)))

; ------------------

;;;; EMR declarations

(emr-declare-action emr-el-implement-function
  :title "implement function"
  :modes emacs-lisp-mode
  :predicate (and (symbol-at-point)
                  (not (emr-looking-at-string?))
                  (not (emr-looking-at-comment?))
                  (not (thing-at-point 'number))
                  (not (emr-el:looking-at-definition?))
                  (not (emr-el:looking-at-let-binding-symbol?))
                  (not (boundp (symbol-at-point)))
                  (not (fboundp (symbol-at-point)))))

(emr-declare-action emr-el-inline-variable
  :title "inline"
  :modes emacs-lisp-mode
  :predicate (emr-el:variable-definition? (list-at-point)))

(emr-declare-action emr-el-inline-function
  :title "inline"
  :modes emacs-lisp-mode
  :predicate (emr-el:looking-at-definition?))

(emr-declare-action emr-el-extract-function
  :title "function"
  :description "defun"
  :modes emacs-lisp-mode
  :predicate (not (or (emr-el:looking-at-definition?)
                      (emr-el:looking-at-let-binding-symbol?))))

(emr-declare-action emr-el-extract-variable
  :title "variable"
  :description "defvar"
  :modes emacs-lisp-mode
  :predicate (and (not (emr-el:looking-at-definition?))
                  (not (emr-el:looking-at-let-binding-symbol?))
                  (thing-at-point 'defun)))

(emr-declare-action emr-el-extract-constant
  :title "constant"
  :description "defconst"
  :modes emacs-lisp-mode
  :predicate (not (or (emr-el:looking-at-definition?)
                      (emr-el:looking-at-let-binding-symbol?))))

(emr-declare-action emr-el-extract-to-let
  :title "let-bind"
  :description "let"
  :modes emacs-lisp-mode
  :predicate (not (or (emr-el:looking-at-definition?)
                      (emr-el:looking-at-decl?)
                      (emr-el:looking-at-let-binding-symbol?))))

(emr-declare-action emr-el-delete-let-binding-form
  :title "delete binding"
  :description "unused"
  :modes emacs-lisp-mode
  :predicate (and (emr-el:looking-at-let-binding-symbol?)
                  (not (emr-el:let-bound-var-at-point-has-usages?))))

(emr-declare-action emr-el-inline-let-variable
    :title "inline binding"
    :modes emacs-lisp-mode
    :predicate (and (emr-el:looking-at-let-binding-symbol?)
                    (emr-el:let-bound-var-at-point-has-usages?)))

(emr-declare-action emr-el-extract-autoload
  :title "autoload"
  :description "autoload"
  :modes emacs-lisp-mode
  :predicate (and (or (functionp (symbol-at-point))
                      (emr-el:macro-boundp (symbol-at-point)))
                  (not (emr-el:variable-definition? (list-at-point)))))

(emr-declare-action emr-el-eval-and-replace
  :title "eval"
  :description "value"
  :modes emacs-lisp-mode
  :predicate (not (or (emr-el:looking-at-definition?)
                      (emr-el:looking-at-let-binding-symbol?))))

(emr-declare-action emr-el-comment-form
  :title "comment"
  :modes emacs-lisp-mode
  :predicate (and (thing-at-point 'defun)
                  (not (emr-looking-at-comment?))))

(provide 'emr-elisp)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; emr-elisp.el ends here
