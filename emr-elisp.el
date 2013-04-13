;;; emr-elisp --- Refactoring commands for Emacs Lisp

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 0.2
;; Keywords: tools elisp convenience refactoring
;; Package-Requires: ((s "20130320.1524") (dash "20130408.2132") (cl-lib "0.2") (popup "20130324.1305"))


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

;;; Refactoring commands for Emacs Lisp. Part of the Emacs Refactoring suite.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'list-utils)
(require 's)
(require 'thingatpt)
(require 'emr)

;;; ----------------------------------------------------------------------------
;;; Read and write lisp forms.
;;;
;;; Because the Elisp reader does not preserve comments or newlines, we embed
;;; them in forms so that they can be reconstructed when printing.

(defun emr--newline? (form)
  "Non-nil if FORM is a newline token."
  (equal form :emr--newline))

(defun emr--format-comments (line)
  "Wrap any comments at the end of LINE in a comment form.  Otherwise return LINE unchanged."
  (if-let (pos (s-index-of ";" line))
    (let ((code    (substring line 0 (1- pos)))
          (comment (substring line pos)))
      (concat
       code
       (if (s-blank? comment) "" (format " (%s %S)" :emr--comment comment))))
    line))

(defun emr--read (str)
  "Read the given string STR as a Lisp expression, inserting tokens to represent whitespace."
  (let ((print-quoted t)
        (print-level nil)
        (print-length nil)
        (print-escape-newlines t)
        )
    (->> (s-lines str)
      (-map 'emr--format-comments)
      ;; Insert newline tokens.
      (s-join (format " %s " :emr--newline))
      (read))))

(defun emr--reconstruct-comments (str)
  "Unpack any eol comments in STR, otherwise leave STR unchanged."
  (let ((prefix (format "(%s" :emr--comment)))
    (if (s-contains? prefix str)
        (let* ((split   (s-split (s-trim prefix) str))
               (code    (or (car split) ""))
               (comment (format "%s" (read (cdr split)))))
          (concat code comment))
      str)))

(defun emr--unescape-question-marks (str)
  "Unescape question marks in string STR that were escaped by the reader."
  (with-temp-buffer
    (lisp-mode-variables)
    (insert str)
    (goto-char (point-min))
    (save-match-data
      (while (search-forward "\\?" nil t)
        (replace-match "?")))
    (buffer-string)))

(defun emr--print (form)
  "Print FORM as a Lisp expression, replacing whitespace tokens with newlines."
  (let ((nl (format "%s" :emr--newline))
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
      (-map 'emr--reconstruct-comments)
      (s-join "\n  ")
      (emr--unescape-question-marks))))

;;; ----------------------------------------------------------------------------
;;; Navigation commands

(defun emr--goto-first-match (regex)
  "Move point to the first match in the buffer for REGEX."
  (save-match-data
    (when (string-match regex (buffer-string) 0)
      (goto-char (match-beginning 0)))))

(defun emr--looking-at-string? ()
  "Return non-nil if point is inside a string."
  (save-excursion
    (let ((point (point)))
      (beginning-of-defun)
      (nth 3 (parse-partial-sexp (point) point)))))

(defun emr--looking-at-comment? ()
  "Non-nil if point is on a comment."
  (when-let (comment (save-excursion
                       (beginning-of-line)
                       (comment-search-forward (point-at-eol) t)))
    ;; Test if there is a comment-start before point.
    (<= comment (point))))

(defun emr--looking-at-decl? ()
  (-contains? '(interactive declare) (car-safe (emr--list-at-point))))

(defun emr--goto-open-round ()
  "Move to the opening paren for the Lisp list at point."
  (interactive)
  (when (or (not (equal "(" (thing-at-point 'char)))
            (emr--looking-at-string?))
    (beginning-of-sexp)
    (unless (equal "(" (thing-at-point 'char))
      (search-backward "("))))

(defun emr--goto-open-round-or-quote ()
  "Move to the opening paren or quote for the Lisp list at point."
  (interactive)
  (emr--goto-open-round)
  (when (or (thing-at-point-looking-at "'")
            (thing-at-point-looking-at "`")
            (emr--looking-at-string?))
    (search-backward-regexp (rx (or "'" "`")))))

;;; ----------------------------------------------------------------------------
;;; Formatting commands

(defun emr--reindent-defun ()
  "Reindent the current top-level form."
  (save-excursion (end-of-defun) (beginning-of-defun) (indent-sexp)))

(defun emr--reindent-string (form-str)
  "Reformat FORM-STRING, assuming it is a Lisp fragment."
  (with-temp-buffer
    (lisp-mode-variables)
    (insert form-str)
    (emr--reindent-defun)
    (buffer-string)))

(defun emr--insert-above (form-str)
  "Insert and indent FORM-STR above the current top level form.
Return the position of the end of FORM-STR."
  (save-excursion
    (let ((mark-ring nil))
      ;; Move to position above top-level form.
      (beginning-of-line)
      (beginning-of-defun)
      (newline)
      (forward-line -1)
      (back-to-indentation)
      ;; Perform insertion.
      (insert (emr--reindent-string form-str))
      (prog1 (point)
        (newline-and-indent)))))

(defun emr--symbol-file-name (fn)
  "Find the name of the file that declares function FN."
  (when-let (file (find-lisp-object-file-name fn (symbol-function fn)))
    (and (stringp file)
         (file-name-nondirectory (file-name-sans-extension file)))))

(defun emr--list-at-point ()
  "Return the Lisp list at point or enclosing point."
  (interactive)
  (save-excursion
    (emr--goto-open-round)
    (mark-sexp 1 nil)
    (emr--read (buffer-substring-no-properties (region-beginning)
                                               (region-end)))))

(defvar emr--special-symbols
  '(--cl-rest-- &rest &optional &key &allow-other-keys \,\@ \,)
  "A list of symbols that should be ignored by variable searches.")

(cl-defun emr--bindings-in-lambda ((_lam arglist &rest body))
  "Return all bound variables within a lambda form."
  (let ((bs (-difference arglist emr--special-symbols)))
    (-concat bs (emr--bound-variables body))))

(cl-defun emr--bindings-in-let ((_let bindings &rest body))
  "Return the list of bound values in the given `let' or `let*' expression."
  (-concat (emr--let-binding-list-symbols bindings)
           (emr--bound-variables body)))

(cl-defun emr--bindings-in-defalias ((_def (_quote sym) func))
  "Return the bindings in a defalias form, including the named alias."
  (cons sym (emr--bound-variables func)))

(defun emr--bound-variables (form)
  "Find the list of let- or lambda-bound variables in form."
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
      ((equal 'lambda hd) (emr--bindings-in-lambda form))
      ((equal 'let hd)  (emr--bindings-in-let form))
      ((equal 'let* hd) (emr--bindings-in-let form))
      ((equal 'defalias hd) (emr--bindings-in-defalias form))
      ;; FUNCTION is the quotation form for function objects.
      ;; Do not bail if the next item is not a lambda.
      ((equal 'function hd) (condition-case _err
                                (-mapcat 'emr--bindings-in-lambda (cdr form))
                              (error
                               (-mapcat 'emr--bound-variables (cdr form)))))
      ;; FORM is probably a value if we're not looking at a list, and can be
      ;; ignored.
      ((listp form)
       (->> form
         ;; Handle improper lists.
         (list-utils-make-proper-copy)
         (-remove 'emr--nl-or-comment?)
         (-mapcat 'emr--bound-variables)))))))

(defun emr--free-variables (form &optional context)
  "Try to find the symbols in FORM that do not have variable bindings.
CONTEXT is the top-level form that encloses FORM."

  ;; Marco-expand FORM and find the list of bound symbols. Diff this with the
  ;; other symbols in FORM. Figure out which ones are not functions, keywords,
  ;; special vars, etc. This should give a pretty good idea of which symbols are
  ;; 'free'.

  (let ((bound-vars (emr--bound-variables form))
        (ctx-bound (emr--bound-variables context))
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
                    (-contains? emr--special-symbols it)
                    (booleanp it)
                    (keywordp it)
                    ;; Remove special vars and function names, unless they've
                    ;; been bound in the enclosing form.
                    (unless (-contains? ctx-bound it)
                      (or
                       (special-variable-p it)
                       (symbol-function it))))))))

;;; ----------------------------------------------------------------------------
;;; Refactoring Macros

(defmacro emr--reporting-buffer-changes (description &rest body)
  "Execute forms producing an effect described by DESCRIPTION.
Report the changes made to the buffer at a result of executing BODY forms."
  (declare (indent 1))
  `(let ((before-changes (buffer-string)))
     ,@body
     ;; Report changes.
     (when-let (diff (and emr-report-actions
                          (car (emr--diff-lines before-changes (buffer-string)))))
       (destructuring-bind (_ . (line . text)) diff
         (unless (emr--line-visible? line)
           (emr--report-action ,description line text))))))

(defun emr--remove-trailing-newlines (form)
  "Remove newlines from the end of FORM."
  (if (listp form)
      (->> form (reverse) (-drop-while 'emr--newline?) (reverse))
    form))

(defun emr--collapse-leading-newlines (form)
  "Find the first instance of newlines in FORM and collapse any newlines in sequence."
  (if-let (pos (and (listp form) (cl-position :emr--newline form)))
    ;; Find the first newline, split FORM and drop newlines before splicing the
    ;; parts back together with a newline separator.
    (cl-destructuring-bind (hd tl) (-split-at pos form)
      `(,@hd :emr--newline ,@(-drop-while 'emr--newline? tl)))
    form))

(defun emr--wrapping-read (str)
  "Try to read string STR, wrapping in a PROGN if necessary."
  (let* (
         ;; Wrap the last kill in a progn.
         (form (emr--read (format "(progn \n %s)" str)))
         ;; Point to the first non-newline item in the PROGN.
         (beg (--drop-while (or (equal 'progn it) (emr--newline? it)) form))
         )
    ;; Remove PROGN form if it is unneccesary and tidy newlines.
    (->> (cond ((atom beg) beg)
               ;; Strip the PROGN if it only contains a single sexpr.
               ((= 1 (length (-remove 'emr--nl-or-comment? beg))) (car beg))
               (t form))
      (emr--collapse-leading-newlines)
      (emr--remove-trailing-newlines))))

(cl-defmacro emr--extraction-refactor ((&optional binding) description &rest body)
  "Kill the sexp near point then execute forms.
BINDING is the name to bind to the extracted form.
DESCRIPTION is used to report the result of the refactoring.
BODY is a list of forms to execute after extracting the sexp near point.
The extracted expression is bound to the symbol 'extracted-sexp'."
  (declare (indent 2))
  `(save-excursion

     ;; Either extract the active region or the sexp near point.
     (if (region-active-p)
         (kill-region (region-beginning) (region-end))
       (emr--goto-open-round-or-quote)
       (kill-sexp))

     (emr--reindent-defun)

     (let
         ;; Define BINDING if supplied.
         ,(when binding `((,binding (emr--wrapping-read (car kill-ring)))))

       ;; Revert kill-ring pointer.
       (setq kill-ring (cdr kill-ring))
       (save-excursion
         (emr--reporting-buffer-changes ,description
           ,@body)))))

(defun emr--line-visible? (line)
  "Return true if LINE is within the visible bounds of the current window."
  (let* ((min (line-number-at-pos (window-start)))
         (max (line-number-at-pos (window-end))))
    (and (>= line min) (<= line max))))

;;; ----------------------------------------------------------------------------
;;; Definition site tests.

(defun emr--macro-definition? (form)
  "Return t if FORM expands to a macro definition."
  (ignore-errors
    (let ((exp (macroexpand-all form)))
      ;; yo dawg I herd you like cars
      (and (equal 'defalias (car exp))
           (equal 'macro (cadar (cdaddr exp)))))))

(defun emr--function-definition? (form)
  "Return t if FORM expands to a function definition."
  (ignore-errors
    (let ((exp (macroexpand-all form)))
      (and (equal 'defalias (car exp))
           (equal 'function (caaddr exp))))))

(defun emr--variable-definition? (form)
  (ignore-errors
    (-contains? '(defconst defvar defcustom)
                (car (macroexpand-all form)))))

(defun emr--definition? (form)
  "Return non-nil if FORM is a definition."
  (or (emr--variable-definition? form)
      (emr--macro-definition? form)
      (emr--function-definition? form)))

(defun emr--looking-at-definition? ()
  "Return non-nil if point is at a definition form."
  (emr--definition? (emr--list-at-point)))

(defun emr--autoload-exists? (function str)
  "Returns true if an autoload for FUNCTION exists in string STR."
  (s-contains? (format "(autoload '%s " function) str))

;;; ----------------------------------------------------------------------------
;;; Define refactoring commands.

(defun emr--extract-var-values (sexp)
  "Return the name and initializing value of SEXP if it is a variable definition."
  (let ((exp (macroexpand-all sexp)))
    (when (emr--variable-definition? exp)
      (cl-destructuring-bind (_def sym &rest forms) exp
          (cons sym (car forms))))))

(cl-defun emr--replace-usages ((sym . value))
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
          (replace-match (emr--print value) t nil nil 1)
          (emr--reindent-defun))
        (nreverse lines)))))

;;;###autoload
(defun emr-inline-variable ()
  "Inline the variable defined at point.
Uses of the variable are replaced with the initvalue in the variable definition."
  (interactive)
  (save-excursion
    (emr--goto-open-round)
    (if-let (vals (emr--extract-var-values (emr--list-at-point)))
      (if (> (length vals) 1)
          (emr--extraction-refactor () "Inlining applied at"

            ;; Clean up line spacing.
            (while (s-blank? (buffer-substring-no-properties (line-beginning-position)
                                                             (line-end-position)))
              (kill-line))

            ;; Perform inlining.
            ;; emr--extraction-refactor will report the first insertion. If
            ;; there are none or more than one insertion, override this report.
            (if-let (lines (-map 'int-to-string (emr--replace-usages vals)))
              (when (> (length lines) 1)
                (message "Inlining applied at lines %s" (s-join ", " lines)))
              (message "No usages found")))

        (error "No value to inline for %s" (car vals)))
      (error "Not a variable definition"))))

(defun emr--eval-and-print-progn (prog)
  "Eval and print each form in sexp PROG."
  (->> prog
    (emr--unprogn)
    (-map 'eval)
    (-remove 'null)
    (-map 'emr--print)))

(defun emr--multiline-region? ()
  (when (region-active-p)
    (/= (line-number-at-pos (region-beginning))
        (line-number-at-pos (region-end)))))

;;; TODO: insert above should skip comments.

;;;###autoload
(defun emr-eval-and-replace ()
  "Replace the current region or the form at point with its value."
  (interactive)
  (let ((multline? (emr--multiline-region?)))
    (emr--extraction-refactor (sexp) "Replacement at"
      (--each (emr--eval-and-print-progn sexp)
        (insert it)
        (indent-for-tab-command))
      ;; the extraction internally trims newlines. If this was an eval'ed
      ;; region, we want a single new line afterwards.
      (when multline? (newline-and-indent))
      (emr--reindent-defun))))

(defun emr--read-with-default (prompt value)
  "Prompt for user input, showing PROMPT with an inline default VALUE."
  (let ((val (s-trim (format "%s" value))))
    (s-trim
     (if (s-blank? val)
         (read-string (format "%s:  " prompt))
       (read-string (format "%s (default: %s): "  prompt val) nil nil val)))))

(defun emr--format-submitted-arglist (input)
  "Format a user-submitted arglist, raising an error if it is malformed."
  (unless (or (s-blank? input)
              (s-matches? (rx (or "()" "nil")) input))
    (condition-case _err
        (read (format "(%s)" input))
      (error
       ;; Rethrow reader errors as something more informative.
       (error "Malformed arglist")))))

(defun emr--read-args (form context)
  "Read an arglist from the user, using FORM to generate a suggestion.
CONTEXT is the top-level form that encloses FORM."
  (let ((input
         ;; Generate suggested arglist for prompt.
         (->> (emr--free-variables form context)
           (-map 'symbol-name)
           (s-join " ")
           (s-trim)
           ;; Read user input, supplying default arglist.
           (emr--read-with-default "Arglist" )))
        )
    (emr--format-submitted-arglist input)))

(defun emr--format-defun (defun-str)
  "Format DEFUN-STR to a prettier defun representation."
  (replace-regexp-in-string
   (rx bol "(" (* nonl) "def" (* nonl) (group "nil" (* space)) eol)
   "()"
   defun-str t nil 1))

(defun emr--unprogn (body)
  "Remove a `progn' if it is the first non-whitespace symbol in BODY.
Ensures the result is in a list, regardless of whether a progn was found."
  (->> (cl-list* body)
    (-drop-while 'emr--newline?)
    (macroexp-unprogn)
    (-drop-while 'emr--newline?)))

;;;###autoload
(defun emr-extract-function (name arglist)
  "Extract a function, using the current region or form point as the body.
NAME is the name of the new function.
ARGLIST is its argument list."
  (interactive (list (read-string "Name: ")
                     (emr--read-args (list-at-point) (thing-at-point 'defun))))
  (cl-assert (not (s-blank? name)) () "Name must not be blank")
  (emr--extraction-refactor (sexp) "Extracted to"
    (let ((name (intern name)))
      ;; Insert usage.
      (insert (emr--print (cl-list* name arglist)))
      ;; Insert defun.
      (emr--insert-above
       (emr--format-defun
        (emr--print
         `(defun ,name ,arglist
            :emr--newline
            ,@(emr--unprogn sexp))))))))

;;;###autoload
(defun emr-extract-variable (name)
  "Extract the current region or form at point to a special variable.
The variable will be called NAME."
  (interactive "sName: ")
  (cl-assert (not (s-blank? name)) () "Name must not be blank")
  (emr--extraction-refactor (sexp) "Extracted to"
    ;; Insert usage.
    (insert (s-trim name))
    ;; Insert definition.
    (emr--insert-above
     (emr--print
      (list 'defvar (intern name) sexp)))))

;;;###autoload
(defun emr-extract-constant (name)
  "Extract the current region or form at point to a constant special variable.
The variable will be called NAME."
  (interactive "sName: ")
  (cl-assert (not (s-blank? name)) () "Name must not be blank")
  (emr--extraction-refactor (sexp) "Extracted to"
    ;; Insert usage
    (insert (s-trim name))
    ;; Insert definition.
    (emr--insert-above
     (emr--print
      (list 'defconst (intern name) sexp)))))

;;;###autoload
(defun emr-extract-autoload (function file)
  "Create an autoload for FUNCTION.
FILE is the file that declares FUNCTION.
See `autoload' for details."
  (interactive
   (let* ((sym  (intern (or (thing-at-point 'symbol) (read-string "Function: "))))
          (file (or (emr--symbol-file-name sym)
                    (read-string "File: "))))
     (list sym file)))
  (let ((form `(autoload ',function ,file)))
    (save-excursion
      (emr--reporting-buffer-changes "Extracted to"
        ;; Put the extraction next to existing autoloads if any, otherwise
        ;; insert above top-level form.
        (if (emr--goto-first-match "^(autoload ")
            (progn (forward-line 1) (end-of-line) (newline)
                   (insert (emr--print form)))
          (emr--insert-above
           (emr--print form)))))))

;;;###autoload
(defun emr-comment-form ()
  "Comment out the current region or from at point."
  (interactive)
  (if (region-active-p)
      (comment-region (region-beginning)
                      (region-end))
    (emr--goto-open-round-or-quote)
    (mark-sexp)
    (comment-region (region-beginning) (region-end))))

(defun emr--infer-arglist-for-usage (form)
  "Suggest a suitable arglist for the given function application FORM."
  (->> form
    ;; Anything that isn't a symbol becomes 'argn'.
    (--map-indexed (if (symbolp it) it (intern (format "arg%s" it-index))))
    ;; Drop function name.
    (-drop 1)))

;;;###autoload
(defun emr-implement-function (name arglist)
  "Create a function definition for the symbol at point.
The function will be called NAME and have the given ARGLIST. "
  (interactive (list
                (emr--read (emr--read-with-default "Name" (symbol-at-point)))
                ;; Infer arglist from usage.
                (->> (list-at-point)
                  (emr--infer-arglist-for-usage)
                  (-map 'symbol-name)
                  (s-join " ")
                  (s-trim)
                  (emr--read-with-default "Arglist")
                  (emr--format-submitted-arglist))))

  ;; Save position after insertion so we can move to the defun's body.
  (let (pos)
    (save-excursion

      ;; Mark whole sexp at point.
      (beginning-of-thing 'sexp)
      (mark-sexp)

      (emr--extraction-refactor ()  "Defined function"

        ;; Insert reference.
        (insert (format "%s" name))

        ;; Insert definition.
        (->> (list 'defun name arglist :emr--newline)
          (emr--print)
          (emr--format-defun)
          (emr--insert-above)
          (setq pos))))

    ;; Move to body position.
    (goto-char pos)
    (beginning-of-defun)
    (forward-line 1)
    (indent-for-tab-command)))

;;; ----------------------------------------------------------------------------
;;; Let expressions.

(defun emr--let-form? (form)
  "Non-nil if FORM is a let or let* form."
  (-contains? '(let let*) (car-safe form)))

(defun emr--defun-form? (form)
  "Non-nil if FORM is a function or macro definition form."
  (-contains? '(defun cl-defun defun* defmacro cl-defmacro defmacro*)
              (car-safe form)))

;;; Binding membership tests.

(defun emr--first-atom (form)
  "Returns the first atom the car of FORM, at any level of nesting."
  (if (listp form)
      (car-safe (-flatten form))
    form))

(defun emr--bindings-after (sym binding-forms)
  "Return the bindings after the first instance of SYM in BINDING-FORMS."
  (->> binding-forms
    (--split-with (equal sym (emr--first-atom it)))
    (cdr)))

(defun emr--duplicates? (ls)
  "Return non-nil if any elements in LS are duplicated."
  (/= (length ls) (length (-distinct ls))))

(defun emr--let-binding-list-symbols (binding-forms)
  "Return the symbols defined in a let BINDING FORM."
  (->> binding-forms
    (--map (or (car-safe it) it))
    (-remove 'emr--nl-or-comment?)
    (-remove 'null)))

(defun emr--let-binding-list-values (binding-forms)
  "Return the values in a let BINDING FORM."
  (->> binding-forms (-map 'cdr-safe) (-remove 'emr--nl-or-comment?)))

(defun emr--recursive-bindings? (binding-forms)
  "Test whether let BINDING-FORMS are dependent on one-another."
  (and
   ;; Cannot be recursive if bindings are empty or singleton.
   (< 1 (length binding-forms))

   ;; Find references to declared syms in values.
   (let ((syms (emr--let-binding-list-symbols binding-forms))
         (vals (-flatten (emr--let-binding-list-values binding-forms))))
     (or (emr--duplicates? syms)
         (-intersection syms vals)))))

;;; Variable insertion.

(defun emr--let-wrap (form &optional splice?)
  "Ensure FORM is wrapped with a `let' form. No change if FORM is already a let form.
SPLICE? determines whether FORM should be directly spliced into the let BODY."
  (let ((nonl (if (listp form) (-drop-while 'emr--newline? form) form)))
    (cond
     ;; No need to wrap existing let-forms.
     ((emr--let-form? nonl)
      nonl)
     ;; Return car if FORM is a singleton list containing a let expr.
     ((and (listp form)
           (equal 1 (length form))
           (emr--let-form? (car form)))
      (car nonl))
     ;; It's not a let form, and the caller has opted to splice form into the
     ;; let body.
     (splice?
      (cl-list* 'let nil :emr--newline nonl))
     ;; In all other cases, wrap with a let expression.
     (t (list 'let nil :emr--newline nonl)))))

(defun emr--pad-top (form)
  "Ensure FORM begins with a newline."
  (if (equal :emr--newline (car-safe form))
      form
    (cons :emr--newline form)))

(defun emr--add-to-bindings (symbol value bindings-list)
  "Append SYMBOL and VALUE to BINDINGS-LIST.
If BINDINGS-LIST is nil, just return the new bindings."
  (let ((new `((,symbol ,value))))
    (if bindings-list
        `(,@bindings-list :emr--newline ,@new)
      new)))

(cl-defun emr--insert-let-var (symbol value-form (let bindings &rest body))
  "Insert a binding into the given let expression."
  (cl-assert (emr--let-form? (list let)))
  (let ((updated (emr--add-to-bindings symbol value-form bindings)))
    ;; Combine updated forms. If the updated bindings are recursive, use let*.
    `(,(if (emr--recursive-bindings? updated) 'let* 'let)
      ,updated
      ,@(emr--pad-top body))))

(defun emr--maybe-skip-docstring (form)
  "Skip docstring if it is at the head of FORM.
Do not skip if there are no forms afterwards. "
  (if (and (stringp (car-safe form))
           (-remove 'emr--newline? (cdr form)))
      (cdr form)
    form))

(defun emr--decl-form? (form)
  "Non-nil if form is an `interactive' spec, assertion, or `declare' form."
  (-contains? '(interactive declare assert cl-assert)
               (or (car-safe form) form)))

(defun emr--nl-or-comment? (form)
  (or (equal :emr--newline form)
      (equal :emr--comment (car-safe form))))

(defun emr--split-defun (form)
  "Split a defun FORM into a list of (header body).
The body is the part of FORM that can be safely transformed without breaking the definition."
  (cl-assert (emr--defun-form? form) () "Not a recognised definition form")

  (-> ;; Inspect the structure of the form. A definition contains an optional
      ;; docstring and interactive/declare specs which should not be changed
      ;; by operations to the body, so we skip those.
      (->> form
        ;; Newlines and comments not semantically useful here.
        (-remove 'emr--nl-or-comment?)
        ;; Skip defun, symbol and arglist.
        (-drop 3)
        (emr--maybe-skip-docstring)
        ;; Skip comments, INTERACTIVE, DECLARE and assertion forms.
        (--drop-while (or (emr--nl-or-comment? it) (emr--decl-form? it))))
    ;; We should now be pointed at the first body form.
    (car)
    ;; Get the position of the body in FORM and split at that point.
    (cl-position form)
    (-split-at form)))

(cl-defun emr--split-defvar (form)
  "Split FORM into a list of (decl sym & docstring)"
  (cl-destructuring-bind (def sym &optional value docstring)
      (-remove 'emr--nl-or-comment? form)
    `((,def ,sym :emr--newline) ,value (,docstring))))

(defun emr--partition-body (form)
  "Splits the given form into a 2-item list at its body forms, if any."
  (cond ((emr--defun-form? form)          (emr--split-defun form))
        ((emr--variable-definition? form) (emr--split-defvar form))
        (t
         ;; Supply a null item to signify an empty header.
         (list nil form))))

(cl-defun emr--recombine-forms ((header body &optional rem))
  (cond ((emr--defun-form? header) `(,@header ,body))
        ((emr--variable-definition? header)
         ;; If REM args exist, put them on a new line.
         `(,@header ,body ,@(when rem (cons :emr--newline rem))))
        (t
         body)))

(cl-defun emr--let-format-body (symbol value (header body &optional rest))
  "Wrap BODY in a let expression."
  ;; Defun forms should have their body spliced into the let form.
  (list header
        (->> (emr--defun-form? header)
          (emr--let-wrap body)
          (emr--insert-let-var symbol value))
        rest))

(defun emr--add-let-binding (symbol value form)
  "Insert a let-binding for SYMBOL with VALUE into FORM.
Wraps FORM with a let form if necessary."
  (->> form
    ;; Determine which part of FORM is the body, and apply let-insertion to that
    ;; form.
    (emr--partition-body)
    (emr--let-format-body symbol value)
    (emr--recombine-forms)
    ;; Drop trailing newlines and nil forms.
    (reverse)
    (--drop-while (or (emr--newline? it) (null it)))
    (reverse)))

;; (nthcdr 9 (emr--add-let-binding
;;            'x 'y

;;            '(defun fn (args) :emr--newline
;;               "docstring" :emr--newline
;;               (:emr--comment ";; hello!")
;;               (interactive) :emr--newline
;;               (let () :emr--newline
;;                    (body)))))

;;;###autoload
(defun emr-extract-to-let (symbol)
  "Extract the current region or form at point into a let-bound variable.
A let form will be created if one does not exist.
The expression will be bound to SYMBOL."
  (interactive "SSymbol: ")
  (emr--extraction-refactor (sexp) "Extracted to let expression"

    ;; Insert usage.
    (insert (emr--print symbol))

    ;; Replace the top-level form.
    (beginning-of-defun)
    (kill-sexp)

    ;; Insert updated let-binding.
    (->> (emr--read (car kill-ring))
      (emr--add-let-binding symbol sexp)
      ;; Pretty-format for insertion.
      (emr--print)
      (emr--format-defun)
      (emr--reindent-string)
      (insert)))

  ;; Move to inserted variable.
  (save-match-data
    (search-forward-regexp
     (eval `(rx word-start ,(emr--print symbol) word-end))
     (save-excursion (end-of-defun))
     'no-error)))

;;; TODO: Use body form index instead of crazy parsing.
;; (defun emr--fn-body-index (fn)
;;   "Return the minimum position of the body or &rest args for function fn."
;;   (when (fboundp fn)
;;     (->> (help-function-arglist fn)
;;       ;; &optional forms precede &rest forms.
;;       (--remove (equal '&optional it))
;;       (cl-position '&rest))))

;;; ----------------------------------------------------------------------------
;;; Declare commands with EMR.

;;; Inline variable
(emr-declare-action emr-inline-variable emacs-lisp-mode "inline"
  :predicate (emr--variable-definition? (emr--list-at-point)))

;;; Extract function
(emr-declare-action emr-extract-function emacs-lisp-mode "function"
  :predicate (not (emr--looking-at-definition?))
  :description "defun")

;;; Let-bind variable
(emr-declare-action emr-extract-to-let emacs-lisp-mode "let-bind"
  :predicate (not (or(emr--looking-at-definition?)
                     (emr--looking-at-decl?)))
  :description "let")

;;; Extract variable
(emr-declare-action emr-extract-variable emacs-lisp-mode "variable"
  :predicate (and (not (emr--looking-at-definition?))
                  (thing-at-point 'defun))
  :description "defvar")

;;; Extract constant
(emr-declare-action emr-extract-constant emacs-lisp-mode "constant"
  :predicate (not (emr--looking-at-definition?))
  :description "defconst")

;;; Implement function
(emr-declare-action emr-implement-function emacs-lisp-mode "implement function"
  :predicate (and (not (emr--looking-at-string?))
                  (not (thing-at-point 'comment))
                  (not (thing-at-point 'number))
                  (symbol-at-point)
                  (not (boundp (symbol-at-point)))
                  (not (fboundp (symbol-at-point)))))

;;; Eval and replace expression
(emr-declare-action emr-eval-and-replace emacs-lisp-mode "eval"
  :predicate (not (emr--looking-at-definition?))
  :description "value")

;;; Extract autoload
(emr-declare-action emr-extract-autoload emacs-lisp-mode "autoload"
  :description "autoload"
  :predicate (and (functionp (symbol-at-point))
                  (not (emr--variable-definition? (emr--list-at-point)))
                  (not (emr--autoload-exists? (symbol-at-point) (buffer-string)))))

;;; Comment-out form
;;; Should be looking at a lisp list.
(emr-declare-action emr-comment-form emacs-lisp-mode "comment"
  :predicate (and (thing-at-point 'defun)
                  (not (emr--looking-at-comment?))))

(provide 'emr-elisp)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; emr-elisp.el ends here
