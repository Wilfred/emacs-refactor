;;; emr-elisp --- Refactoring commands for Emacs Lisp

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

(defun emr:newline? (form)
  "Non-nil if FORM is a newline token."
  (equal form :emr:newline))

(defun emr:format-comments (line)
  "Wrap any comments at the end of LINE in a comment form.  Otherwise return LINE unchanged."

  ;; Comment extraction, the Cthulhu way. We search for the start of a comment
  ;; in LINE, then wrap everything from that index in a list form beginning with
  ;; the :emr:comment token.
  (save-match-data
    (with-temp-buffer
      (lisp-mode-variables t)
      (insert line)

      (if (comment-beginning)
          (progn
            ;; POINT is now inside the comment.
            ;; Move to the start of the comment chars.
            (goto-char (search-backward-regexp
                        (eval `(rx (not (any ,comment-start)) ,comment-start))))
            (forward-char 1)
            ;; Split at the location of the comment. Format the line so that the
            ;; comment is inside a lisp list form.
            (let ((code (buffer-substring (line-beginning-position) (point)))
                  (cmt  (buffer-substring (point) (line-end-position))))
              (format "%s (%s \"%s\")" code :emr:comment cmt)))
        ;; No processing necessary - return the argument unchanged.
        line))))

(defun emr:read (str)
  "Read the given string STR as a Lisp expression, inserting tokens to represent whitespace."
  (let ((print-quoted t)
        (print-level nil)
        (print-length nil)
        (print-escape-newlines t)
        )
    (->> (s-lines str)
      (-map 'emr:format-comments)
      ;; Insert newline tokens.
      (s-join (format " %s " :emr:newline))
      (read))))

(defun emr:reconstruct-comments (str)
  "Unpack any eol comments in STR, otherwise leave STR unchanged."
  (let ((prefix (format "(%s" :emr:comment)))
    (if (s-contains? prefix str)
        (let* ((split   (s-split (s-trim prefix) str))
               (code    (or (car split) ""))
               (comment (format "%s" (read (cdr split)))))
          (concat code comment))
      str)))

(defun emr:unescape-question-marks (str)
  "Unescape questionmarks in string STR that were escaped by the reader."
  (with-temp-buffer
    (lisp-mode-variables)
    (insert str)
    (goto-char (point-min))
    (save-match-data
      (while (search-forward "\\?" nil t)
        (replace-match "?")))
    (buffer-string)))

(defun emr:print (form)
  "Print FORM as a Lisp expression, replacing whitespace tokens with newlines."
  (let ((nl (format "%s" :emr:newline))
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
      (-map 'emr:reconstruct-comments)
      (s-join "\n  ")
      (emr:unescape-question-marks))))

;;; ----------------------------------------------------------------------------
;;; Navigation commands

(defun emr:goto-first-match (regex)
  "Move point to the first match in the buffer for REGEX."
  (save-match-data
    (when (string-match regex (buffer-string) 0)
      (goto-char (match-beginning 0)))))

(defun emr:looking-at-string? ()
  "Return non-nil if point is inside a string."
  (save-excursion
    (let ((point (point)))
      (beginning-of-defun)
      (nth 3 (parse-partial-sexp (point) point)))))

(defun emr:looking-at-comment? ()
  "Non-nil if point is on a comment."
  (-when-let (comment (save-excursion
                        (beginning-of-line)
                        (comment-search-forward (point-at-eol) t)))
    ;; Test if there is a comment-start before point.
    (<= comment (point))))

(defun emr:looking-at-decl? ()
  (-contains? '(interactive declare) (car-safe (emr:list-at-point))))

(defun emr:goto-open-round ()
  "Move to the opening paren for the Lisp list at point."
  (interactive)
  (when (or (not (equal "(" (thing-at-point 'char)))
            (emr:looking-at-string?))
    (beginning-of-sexp)
    (unless (equal "(" (thing-at-point 'char))
      (search-backward "("))))

(defun emr:goto-open-round-or-quote ()
  "Move to the opening paren or quote for the Lisp list at point."
  (interactive)
  (emr:goto-open-round)
  (when (or (thing-at-point-looking-at "'")
            (thing-at-point-looking-at "`")
            (emr:looking-at-string?))
    (search-backward-regexp (rx (or "'" "`")))))

;;; ----------------------------------------------------------------------------
;;; Formatting commands

(defun emr:reindent-defun ()
  "Reindent the current top level form."
  (save-excursion (end-of-defun) (beginning-of-defun) (indent-sexp)))

(defun emr:reindent-string (form-str)
  "Reformat FORM-STR, assuming it is a Lisp fragment."
  (with-temp-buffer
    (lisp-mode-variables)
    (insert form-str)
    (emr:reindent-defun)
    (buffer-string)))

(defun emr:insert-above (form-str)
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
      (insert (emr:reindent-string form-str))
      (prog1 (point)
        (newline-and-indent)))))

(defun emr:symbol-file-name (fn)
  "Find the name of the file that declares function FN."
  (-when-let (file (find-lisp-object-file-name fn (symbol-function fn)))
    (and (stringp file)
         (file-name-nondirectory (file-name-sans-extension file)))))

(defun emr:list-at-point ()
  "Return the Lisp list at point or enclosing point."
  (interactive)
  (save-excursion
    (emr:goto-open-round)
    (mark-sexp 1 nil)
    (emr:read (buffer-substring-no-properties (region-beginning)
                                               (region-end)))))

(defvar emr:special-symbols
  '(--cl-rest-- &rest &optional &key &allow-other-keys \,\@ \,)
  "A list of symbols that should be ignored by variable searches.")

(defun* emr:bindings-in-lambda ((_lam arglist &rest body))
  "Return all bound variables within a lambda form."
  (let ((bs (-difference arglist emr:special-symbols)))
    (-concat bs (emr:bound-variables body))))

(defun* emr:bindings-in-let ((_let bindings &rest body))
  "Return the list of bound values in the given `let' or `let*' expression."
  (-concat (emr:let-binding-list-symbols bindings)
           (emr:bound-variables body)))

(defun* emr:bindings-in-defalias ((_def (_quote sym) func))
  "Return the bindings in a defalias form, including the named alias."
  (cons sym (emr:bound-variables func)))

(defun emr:bound-variables (form)
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
      ((equal 'lambda hd) (emr:bindings-in-lambda form))
      ((equal 'let hd)  (emr:bindings-in-let form))
      ((equal 'let* hd) (emr:bindings-in-let form))
      ((equal 'defalias hd) (emr:bindings-in-defalias form))
      ;; FUNCTION is the quotation form for function objects.
      ;; Do not bail if the next item is not a lambda.
      ((equal 'function hd) (condition-case _err
                                (-mapcat 'emr:bindings-in-lambda (cdr form))
                              (error
                               (-mapcat 'emr:bound-variables (cdr form)))))
      ;; FORM is probably a value if we're not looking at a list, and can be
      ;; ignored.
      ((listp form)
       (->> form
         ;; Handle improper lists.
         (list-utils-make-proper-copy)
         (-remove 'emr:nl-or-comment?)
         (-mapcat 'emr:bound-variables)))))))

(defun emr:free-variables (form &optional context)
  "Try to find the symbols in FORM that do not have variable bindings.
CONTEXT is the top level form that encloses FORM."

  ;; Marco-expand FORM and find the list of bound symbols. Diff this with the
  ;; other symbols in FORM. Figure out which ones are not functions, keywords,
  ;; special vars, etc. This should give a pretty good idea of which symbols are
  ;; 'free'.

  (let ((bound-vars (emr:bound-variables form))
        (ctx-bound (emr:bound-variables context))
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
                    (-contains? emr:special-symbols it)
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

(defun emr:remove-trailing-newlines (form)
  "Remove newlines from the end of FORM."
  (if (listp form)
      (->> form (reverse) (-drop-while 'emr:newline?) (reverse))
    form))

(defun emr:collapse-leading-newlines (form)
  "Find the first instance of newlines in FORM and collapse any newlines in sequence."
  (-if-let (pos (and (listp form) (cl-position :emr:newline form)))
    ;; Find the first newline, split FORM and drop newlines before splicing the
    ;; parts back together with a newline separator.
    (cl-destructuring-bind (hd tl) (-split-at pos form)
      `(,@hd :emr:newline ,@(-drop-while 'emr:newline? tl)))
    form))

(defun emr:wrapping-read (str)
  "Try to read string STR, wrapping in a PROGN if necessary."
  (let* (
         ;; Wrap the last kill in a progn.
         (form (emr:read (format "(progn \n %s)" str)))
         ;; Point to the first non-newline item in the PROGN.
         (beg (--drop-while (or (equal 'progn it) (emr:newline? it)) form))
         )
    ;; Remove PROGN form if it is unneccesary and tidy newlines.
    (->> (cond ((atom beg) beg)
               ;; Strip the PROGN if it only contains a single sexpr.
               ((= 1 (length (-remove 'emr:nl-or-comment? beg))) (car beg))
               (t form))
      (emr:collapse-leading-newlines)
      (emr:remove-trailing-newlines))))

(cl-defmacro emr:extraction-refactor ((&optional binding) description &rest body)
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
       (emr:goto-open-round-or-quote)
       (kill-sexp))

     (emr:reindent-defun)

     (let
         ;; Define BINDING if supplied.
         ,(when binding `((,binding (emr:wrapping-read (car kill-ring)))))

       ;; Revert kill-ring pointer.
       (setq kill-ring (cdr kill-ring))
       (save-excursion
         (emr-reporting-buffer-changes ,description
           ,@body)))))

;;; ----------------------------------------------------------------------------
;;; Definition site tests.

(defun emr:macro-boundp (symbol)
  "Test whether SYMBOL is bound as a macro."
  (and (fboundp symbol)
       (eq (car (symbol-function symbol)) 'macro)))

(defun emr:macro-definition? (form)
  "Return t if FORM expands to a macro definition."
  (ignore-errors
    (let ((exp (macroexpand-all form)))
      ;; yo dawg I herd you like cars
      (and (equal 'defalias (car exp))
           (equal 'macro (cadar (cdaddr exp)))))))

(defun emr:function-definition? (form)
  "Return t if FORM expands to a function definition."
  (ignore-errors
    (let ((exp (macroexpand-all form)))
      (and (equal 'defalias (car exp))
           (equal 'function (caaddr exp))))))

(defun emr:variable-definition? (form)
  (ignore-errors
    (-contains? '(defconst defvar defcustom)
                (car (macroexpand-all form)))))

(defun emr:definition? (form)
  "Return non-nil if FORM is a definition."
  (or (emr:variable-definition? form)
      (emr:macro-definition? form)
      (emr:function-definition? form)))

(defun emr:looking-at-definition? ()
  "return non-nil if point is at a definition form."
  (or (emr:definition? (list-at-point))
      (-when-let (def (read (thing-at-point 'defun)))
        (emr:find-in-tree (list-at-point) (cl-third def)))))

(defun emr:autoload-exists? (function str)
  "Returns true if an autoload for FUNCTION exists in string STR."
  (s-contains? (format "(autoload '%s " function) str))

;;; ----------------------------------------------------------------------------
;;; Define refactoring commands.

(defun emr:extract-var-values (sexp)
  "Return the name and initializing value of SEXP if it is a variable definition."
  (let ((exp (macroexpand-all sexp)))
    (when (emr:variable-definition? exp)
      (cl-destructuring-bind (_def sym &rest forms) exp
          (cons sym (car forms))))))

(defun* emr:replace-usages ((sym . value))
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
          (replace-match (emr:print value) t nil nil 1)
          (emr:reindent-defun))
        (nreverse lines)))))

;;;###autoload
(defun emr-inline-variable ()
  "Inline the variable defined at point.
Uses of the variable are replaced with the initvalue in the variable definition."
  (interactive)
  (save-excursion
    (emr:goto-open-round)
    (-if-let (vals (emr:extract-var-values (emr:list-at-point)))
      (if (> (length vals) 1)
          (emr:extraction-refactor () "Inlining applied at"

            ;; Clean up line spacing.
            (while (s-blank? (buffer-substring-no-properties (line-beginning-position)
                                                             (line-end-position)))
              (kill-line))

            ;; Perform inlining.
            ;; emr:extraction-refactor will report the first insertion. If
            ;; there are none or more than one insertion, override this report.
            (-if-let (lines (-map 'int-to-string (emr:replace-usages vals)))
              (when (> (length lines) 1)
                (message "Inlining applied at lines %s" (s-join ", " lines)))
              (message "No usages found")))

        (error "No value to inline for %s" (car vals)))
      (error "Not a variable definition"))))

(defun emr:eval-and-print-progn (prog)
  "Eval and print each form in sexp PROG."
  (->> prog
    (emr:unprogn)
    (-map 'eval)
    (-remove 'null)
    (-map 'emr:print)))

(defun emr:multiline-region? ()
  (when (region-active-p)
    (/= (line-number-at-pos (region-beginning))
        (line-number-at-pos (region-end)))))

;;; TODO: insert above should skip comments.

;;;###autoload
(defun emr-eval-and-replace ()
  "Replace the current region or the form at point with its value."
  (interactive)
  (let ((multline? (emr:multiline-region?)))
    (emr:extraction-refactor (sexp) "Replacement at"
      (--each (emr:eval-and-print-progn sexp)
        (insert it)
        (indent-for-tab-command))
      ;; the extraction internally trims newlines. If this was an eval'ed
      ;; region, we want a single new line afterwards.
      (when multline? (newline-and-indent))
      (emr:reindent-defun))))

(defun emr:read-with-default (prompt value)
  "Prompt for user input, showing PROMPT with an inline default VALUE."
  (let ((val (s-trim (format "%s" value))))
    (s-trim
     (if (s-blank? val)
         (read-string (format "%s:  " prompt))
       (read-string (format "%s (default: %s): "  prompt val) nil nil val)))))

(defun emr:format-submitted-arglist (arglist)
  "Format a user-submitted ARGLIST, raising an error if it is malformed."
  (unless (or (s-blank? arglist)
              (s-matches? (rx (or "()" "nil")) arglist))
    (condition-case _err
        (read (format "(%s)" arglist))
      (error
       ;; Rethrow reader errors as something more informative.
       (error "Malformed arglist")))))

(defun emr:read-args (form context)
  "Read an arglist from the user, using FORM to generate a suggestion.
CONTEXT is the top level form that encloses FORM."
  (let ((input
         ;; Generate suggested arglist for prompt.
         (->> (emr:free-variables form context)
           (-map 'symbol-name)
           (s-join " ")
           (s-trim)
           ;; Read user input, supplying default arglist.
           (emr:read-with-default "Arglist" )))
        )
    (emr:format-submitted-arglist input)))

(defun emr:format-defun (defun-str)
  "Format DEFUN-STR to a prettier defun representation."
  (replace-regexp-in-string
   (rx bol "(" (* nonl) "def" (* nonl) (group "nil" (* space)) eol)
   "()"
   defun-str t nil 1))

(defun emr:unprogn (body)
  "Remove a `progn' if it is the first non-whitespace symbol in BODY.
Ensures the result is in a list, regardless of whether a progn was found."
  (->> body
    (-drop-while 'emr:newline?)
    (macroexp-unprogn)
    (-drop-while 'emr:newline?)))

(defun emr:form-extent-for-extraction ()
  "Return either the current region or the list at point."
  (or
   ;; Find symbols within the marked region.
   (when (region-active-p)
     (ignore-errors
       (emr:wrapping-read (buffer-substring (region-beginning) (region-end)))))
   ;; Find symbols within the form around point.
   (list-at-point)))

;;;###autoload
(defun emr-extract-function (name arglist)
  "Extract a function, using the current region or form point as the body.
NAME is the name of the new function.
ARGLIST is its argument list."
  (interactive (list (read-string "Name: ")
                     ;; Prompt user with default arglist.
                     (emr:read-args (emr:form-extent-for-extraction)
                                     (thing-at-point 'defun))))

  (cl-assert (not (s-blank? name)) () "Name must not be blank")

  (emr:extraction-refactor (sexp) "Extracted to"
    (let ((name (intern name))
          (defun-form (if (-any? 'listp arglist) 'defun* 'defun))
          (body (if (listp sexp) (emr:unprogn sexp) (list sexp))))
      ;; Insert usage.
      (insert (emr:print (cl-list* name arglist)))
      ;; Insert defun.
      (->> `(,defun-form ,name ,arglist :emr:newline ,@body)
        (emr:print)
        (emr:format-defun)
        (emr:insert-above)))))

;;;###autoload
(defun emr-extract-variable (name)
  "Extract the current region or form at point to a special variable.
The variable will be called NAME."
  (interactive "sName: ")
  (cl-assert (not (s-blank? name)) () "Name must not be blank")
  (emr:extraction-refactor (sexp) "Extracted to"
    ;; Insert usage.
    (insert (s-trim name))
    ;; Insert definition.
    (emr:insert-above
     (emr:print
      (list 'defvar (intern name) sexp)))))

;;;###autoload
(defun emr-extract-constant (name)
  "Extract the current region or form at point to a constant special variable.
The variable will be called NAME."
  (interactive "sName: ")
  (cl-assert (not (s-blank? name)) () "Name must not be blank")
  (emr:extraction-refactor (sexp) "Extracted to"
    ;; Insert usage
    (insert (s-trim name))
    ;; Insert definition.
    (emr:insert-above
     (emr:print
      (list 'defconst (intern name) sexp)))))

;;;###autoload
(defun emr-extract-autoload (function file)
  "Create an autoload for FUNCTION.
FILE is the file that declares FUNCTION.
See `autoload' for details."
  (interactive
   (let* ((sym  (intern (or (thing-at-point 'symbol) (read-string "Function: "))))
          (file (or (emr:symbol-file-name sym)
                    (read-string "File: "))))
     (list sym file)))

  ;; Bail if there is already an autoload for that symbol.
  (if (emr:autoload-exists? (symbol-at-point) (buffer-string))
      (error "Autoload already exists")

    (let ((form `(autoload ',function ,file)))
      (save-excursion
        (emr-reporting-buffer-changes "Extracted to"
          ;; Put the extraction next to existing autoloads if any, otherwise
          ;; insert above top-level form.
          (if (emr:goto-first-match "^(autoload ")
              (progn (forward-line 1) (end-of-line) (newline)
                     (insert (emr:print form)))
            (emr:insert-above
             (emr:print form))))))))

;;;###autoload
(defun emr-comment-form ()
  "Comment out the current region or from at point."
  (interactive)
  (if (region-active-p)
      (comment-region (region-beginning)
                      (region-end))
    (emr:goto-open-round-or-quote)
    (mark-sexp)
    (comment-region (region-beginning) (region-end))))

(defun emr:infer-arglist-for-usage (form)
  "Suggest a suitable arglist for the given function application FORM."
  (->> form
    ;; Anything that isn't a symbol becomes 'argn'.
    (--map-indexed (if (symbolp it) it (intern (format "arg%s" it-index))))
    ;; Drop function name.
    (-drop 1)))

;;;###autoload
(defun emr-implement-function (name arglist)
  "Create a function definition for the symbol at point.
The function will be called NAME and have the given ARGLIST."
  (interactive (list
                (emr:read (emr:read-with-default "Name" (symbol-at-point)))
                ;; Infer arglist from usage.
                (->> (list-at-point)
                  (emr:infer-arglist-for-usage)
                  (-map 'symbol-name)
                  (s-join " ")
                  (s-trim)
                  (emr:read-with-default "Arglist")
                  (emr:format-submitted-arglist))))
  ;; Determine which defun form to use.
  (let ((defun-form (if (-any? 'listp arglist) 'cl-defun 'defun))
        pos)

    ;; Insert usage and defun, then move to the point to the body of the defun.

    (save-excursion
      ;; Mark whole list at point.
      (beginning-of-thing 'sexp)
      (mark-sexp)

      (emr:extraction-refactor ()  "Defined function"
        ;; Insert reference.
        (insert (format "%s" name))
        ;; Insert definition.
        (setq pos (->> `(,defun-form ,name ,arglist :emr:newline)
                    (emr:print)
                    (emr:format-defun)
                    (emr:insert-above)))))

    ;; Move to end of inserted form.
    (goto-char pos)
    ;; Move to body position.
    (beginning-of-defun)
    (forward-line 1)
    (indent-for-tab-command)))

;;; ----------------------------------------------------------------------------
;;; Let expressions.

(defun emr:let-form? (form)
  "Non-nil if FORM is a let or let* form."
  (-contains? '(let let*) (car-safe form)))

(defun emr:defun-form? (form)
  "Non-nil if FORM is a function or macro definition form."
  (-contains? '(defun cl-defun defun* defmacro cl-defmacro defmacro*)
              (car-safe form)))

;;; Binding membership tests.

(defun emr:first-atom (form)
  "Return the first atom the car of FORM, at any level of nesting."
  (if (listp form)
      (car-safe (-flatten form))
    form))

(defun emr:bindings-after (sym binding-forms)
  "Return the bindings after the first instance of SYM in BINDING-FORMS."
  (->> binding-forms
    (--split-with (equal sym (emr:first-atom it)))
    (cdr)))

(defun emr:duplicates? (ls)
  "Return non-nil if any elements in LS are duplicated."
  (/= (length ls) (length (-distinct ls))))

(defun emr:let-binding-list-symbols (binding-forms)
  "Return the symbols defined in the given let BINDING-FORMS."
  (->> binding-forms
    (--map (or (car-safe it) it))
    (-remove 'emr:nl-or-comment?)
    (-remove 'null)))

(defun emr:let-binding-list-values (binding-forms)
  "Return the values in the given let BINDING-FORMS."
  (->> binding-forms (-map 'cdr-safe) (-remove 'emr:nl-or-comment?)))

(defun* emr:let-binding-list ((_let &optional bindings &rest body))
  "Return the bindings list in the given let form."
  bindings)

(defun emr:recursive-bindings? (binding-forms)
  "Test whether let BINDING-FORMS are dependent on one-another."
  (and
   ;; Cannot be recursive if bindings are empty or singleton.
   (< 1 (length binding-forms))

   ;; Find references to declared syms in values.
   (let ((syms (emr:let-binding-list-symbols binding-forms))
         (vals (-flatten (emr:let-binding-list-values binding-forms))))
     (or (emr:duplicates? syms)
         (-intersection syms vals)))))

;;; Let extraction.

(defun emr:let-wrap (form &optional splice?)
  "Ensure FORM is wrapped with a `let' form.
No changes if FORM is already a let form.
SPLICE? determines whether FORM should be directly spliced into the let BODY."
  (let ((nonl (if (listp form) (-drop-while 'emr:newline? form) form)))
    (cond
     ;; No need to wrap existing let-forms.
     ((emr:let-form? nonl)
      nonl)
     ;; Return car if FORM is a singleton list containing a let expr.
     ((and (listp form)
           (equal 1 (length form))
           (emr:let-form? (car form)))
      (car nonl))
     ;; It's not a let form, and the caller has opted to splice form into the
     ;; let body.
     (splice?
      (cl-list* 'let nil :emr:newline nonl))
     ;; In all other cases, wrap with a let expression.
     (t (list 'let nil :emr:newline nonl)))))

(defun emr:pad-top (form)
  "Ensure FORM begins with a newline."
  (if (equal :emr:newline (car-safe form))
      form
    (cons :emr:newline form)))

(defun emr:add-to-bindings (symbol value bindings-list)
  "Append SYMBOL and VALUE to BINDINGS-LIST.
If BINDINGS-LIST is nil, just return the new bindings."
  (let ((new `((,symbol ,value))))
    (if bindings-list
        `(,@bindings-list :emr:newline ,@new)
      new)))

(defun* emr:insert-let-var (symbol value-form (let bindings &rest body))
  "Insert a binding into the given let expression."
  (cl-assert (emr:let-form? (list let)))
  (let ((updated (emr:add-to-bindings symbol value-form bindings)))
    ;; Combine updated forms. If the updated bindings are recursive, use let*.
    `(,(if (emr:recursive-bindings? updated) 'let* 'let)
      ,updated
      ,@(emr:pad-top body))))

(defun emr:maybe-skip-docstring (form)
  "Skip docstring if it is at the head of FORM.
Do not skip if there are no forms afterwards."
  (if (and (stringp (car-safe form))
           (-remove 'emr:newline? (cdr form)))
      (cdr form)
    form))

(defun emr:decl-form? (form)
  "Non-nil if form is an `interactive' spec, assertion, or `declare' form."
  (-contains? '(interactive declare assert cl-assert)
               (or (car-safe form) form)))

(defun emr:nl-or-comment? (form)
  "Return non-nil if FORM is an emr newline or comment keyword."
  (or (equal :emr:newline form)
      (equal :emr:comment (car-safe form))))

(defun emr:split-defun (form)
  "Split a defun FORM into a list of (header body).
The body is the part of FORM that can be safely transformed without breaking the definition."
  (cl-assert (emr:defun-form? form) () "Not a recognised definition form")

  (-> ;; Inspect the structure of the form. A definition contains an optional
      ;; docstring and interactive/declare specs which should not be changed
      ;; by operations to the body, so we skip those.
      (->> form
        ;; Newlines and comments not semantically useful here.
        (-remove 'emr:nl-or-comment?)
        ;; Skip defun, symbol and arglist.
        (-drop 3)
        (emr:maybe-skip-docstring)
        ;; Skip comments, INTERACTIVE, DECLARE and assertion forms.
        (--drop-while (or (emr:nl-or-comment? it) (emr:decl-form? it))))
    ;; We should now be pointed at the first body form.
    (car)
    ;; Get the position of the body in FORM and split at that point.
    (cl-position form)
    (-split-at form)))

(defun* emr:split-defvar (form)
  "Split FORM into a list of (decl sym & docstring)"
  (cl-destructuring-bind (def sym &optional value docstring)
      (-remove 'emr:nl-or-comment? form)
    `((,def ,sym :emr:newline) ,value (,docstring))))

(defun emr:partition-body (form)
  "Split FORM into a 2-item list at its body forms, if any."
  (cond ((emr:defun-form? form)          (emr:split-defun form))
        ((emr:variable-definition? form) (emr:split-defvar form))
        (t
         ;; Supply a null item to signify an empty header.
         (list nil form))))

(defun* emr:recombine-forms ((header body &optional rem))
  (cond ((emr:defun-form? header) `(,@header ,body))
        ((emr:variable-definition? header)
         ;; If REM args exist, put them on a new line.
         `(,@header ,body ,@(when rem (cons :emr:newline rem))))
        (t
         body)))

(defun* emr:let-format-body (symbol value (header body &optional rest))
  "Wrap BODY in a let expression."
  ;; Defun forms should have their body spliced into the let form.
  (list header
        (->> (emr:defun-form? header)
          (emr:let-wrap body)
          (emr:insert-let-var symbol value))
        rest))

(defun emr:add-let-binding (symbol value form)
  "Insert a let-binding for SYMBOL with VALUE into FORM.
Wraps FORM with a let form if necessary."
  (->> form
    ;; Determine which part of FORM is the body, and apply let-insertion to that
    ;; form.
    (emr:partition-body)
    (emr:let-format-body symbol value)
    (emr:recombine-forms)
    ;; Drop trailing newlines and nil forms.
    (reverse)
    (--drop-while (or (emr:newline? it) (null it)))
    (reverse)))

;;;###autoload
(defun emr-extract-to-let (symbol)
  "Extract the current region or form at point into a let-bound variable.
A let form will be created if one does not exist.
The expression will be bound to SYMBOL."
  (interactive "SSymbol: ")
  (emr:extraction-refactor (sexp) "Extracted to let expression"

    ;; Insert usage.
    (insert (emr:print symbol))

    ;; Replace the top-level form.
    (beginning-of-defun)
    (kill-sexp)

    ;; Insert updated let-binding.
    (->> (emr:read (car kill-ring))
      (emr:add-let-binding symbol sexp)
      ;; Pretty-format for insertion.
      (emr:print)
      (emr:format-defun)
      (emr:reindent-string)
      (insert)))

  ;; Move to inserted variable.
  (save-match-data
    (search-forward-regexp
     (eval `(rx word-start ,(emr:print symbol) word-end))
     (save-excursion (end-of-defun) (point))
     'no-error)))

;;; Let inlining

(defun emr:goto-start-of-let-binding ()
  "Move to the opening paren of the let-expression at point.
Otherwise move to the previous one in the current top level form."
  (save-match-data
    ;; If we're on a let-form, move fowards so the subsequent regex motion works.
    (when (or (equal (symbol-at-point) 'let)
              (equal (symbol-at-point) 'let*))
      (forward-whitespace 1))
    ;; Find start of let-expression, bounded to the current top-level form.
    (search-backward-regexp (rx "(" (or "let" "let*") (or "\n" " " "("))
                            (save-excursion (beginning-of-defun) (point))
                            t)))

(defun emr:find-in-tree (elt tree)
  "Return non-nil if ELT is in TREE."
  (cond ((equal elt tree) elt)
        ((listp tree)
         (--reduce-from (or acc (emr:find-in-tree elt it))
                        nil tree))))

(defun emr:looking-at-let-binding-symbol? ()
  "Non-nil if point is on the binding symbol in a let-binding form."
  (ignore-errors
    (let* ((form (save-excursion (emr:goto-open-round) (emr:list-at-point)))
           (sym (car-safe form)))
      (save-excursion
        ;; Select binding list for the let expression.
        (emr:goto-start-of-let-binding)
        (let ((bindings (emr:let-binding-list (emr:list-at-point))))
          (and
           ;; List at point is part of the bindings list?
           (emr:find-in-tree form bindings)
           ;; Head of the list is a symbol in the binding list?
           (-contains? (emr:let-binding-list-symbols bindings) sym)))))))

(defun emr:let-bindings-recursively-depend? (elt bindings)
  "Non-nil if the given let bindings list has recursive dependency on ELT."
  (-when-let* ((b   (--first (equal elt (emr:first-atom it)) bindings))
               (pos (cl-position b bindings :test 'equal)))
    (-> (-split-at (1+ pos) bindings)
      (cl-second)
      (-flatten)
      (-contains? elt))))

(defun* emr:let-binding-is-used? (symbol (_let &optional bindings &rest body))
  "Non-nil if SYMBOL is used in the body or other bindings of the given let expression."
  (or
   ;; Subsequent references in bindings list?
   (emr:let-bindings-recursively-depend? symbol bindings)
   ;; Body contains usage?
   (-contains? (-flatten body) symbol)))

(defun emr:replace-in-tree (symbol value form)
  "Replace usages of SYMBOL with VALUE in FORM."
  (cond
   ((listp form) (--map (emr:replace-in-tree symbol value it) form))
   ((equal symbol form) value)
   (t
    form)))

(defun* emr:remove-let-binding (symbol (&optional let bindings &rest body))
  "Remove the binding for SYMBOL from the given binding form.
Return a list where the car is the value binding that was removed
and the cdr is the updated input form."
  (let* (;; Remove SYMBOL from BINDINGS, then reformat.
         (updated (->> bindings (--remove (equal symbol (car-safe it)))))

         ;; Get the bindings form.
         (binding (->> bindings (-remove 'emr:nl-or-comment?) (assoc symbol)))
         )
    (list binding `(,let ,updated ,@body))))

(defun* emr:update-let-body (binding-elt (_let &optional bindings &rest body))
  "Replace usages of a binding in BODY forms.
BINDING-ELT is a list of the form (symbol &optional value)"
  (let* (;; Replace `let*' with `let' if possible.
         (let-form (if (emr:recursive-bindings? bindings) 'let* 'let))
         ;; Perform inlining if BINDING-ELT can be destructured.
         (symbol (cl-first binding-elt))
         (value  (cl-second binding-elt))
         (body   (if binding-elt (emr:replace-in-tree symbol value body) body)))
    `(,let-form ,bindings ,@body)))

(defun* emr:simplify-let-form ((let &optional bindings &rest body))
  "Simplifies a `let' or `let*' form if there are no bindings.
When there are no bindings:
* Returns the body form if it is a single value.
* Changes to a `progn' if is more than one value."
  (let ((nonl (-remove 'emr:nl-or-comment? body)))
    (cond
     ;; Return form unchanged if there are let bindings after inlining.
     ((-remove 'emr:nl-or-comment? bindings) `(,let ,bindings ,@body))
     ;; Return body form if it is a singleton list.
     ((= 1 (length nonl)) (car nonl))
     ;; Use progn if no bindings and multiple body forms.
     ((null nonl) nil)
     (t `(progn ,@body)))))

(defun emr:inline-let-binding (symbol form)
  "Replace usages of SYMBOL with VALUE in FORM and update the bindings list."
  (cl-destructuring-bind (b-elt updated) (emr:remove-let-binding symbol form)
    (-> (emr:update-let-body b-elt updated)
      (emr:reformat-let-binding-list)
      (emr:simplify-let-form))))

(defun* emr:reformat-let-binding-list ((let &optional bindings &rest body))
  "Ensure the bindings list in the given let expression is well-formatted."
  `(,let ,(->> bindings
            ;; Trim whitespace at beginning and end of list.
            (-drop-while 'emr:newline?)
            (reverse)
            (-drop-while 'emr:newline?)
            (reverse))
     ,@body))

;;;###autoload
(defun emr-inline-let-variable (symbol)
  "Inline the let-bound variable named SYMBOL at point."
  (interactive (list (symbol-at-point)))
  (cl-assert (emr:looking-at-let-binding-symbol?))

  (save-excursion
    (emr:goto-start-of-let-binding)
    (emr:extraction-refactor (form) "Inlined let-bound symbol"
      ;; Insert updated let-binding.
      (->> (emr:inline-let-binding symbol form)
        (emr:print)
        (emr:format-defun)
        (emr:reindent-string)
        (insert)))

    ;; Ensure whole form is correctly reindented.
    (mark-defun)
    (indent-region (region-beginning) (region-end)))

  ;; Move back into bindings or body.
  (forward-symbol 2))

;;; Let deletion

(defun emr:let-bound-var-at-point-has-usages? ()
  "Non-nil if the let-bound symbol at point is referred to in the
bindings or body of the enclosing let expression."
  (and (emr:looking-at-let-binding-symbol?)
       (save-excursion
         (let ((sym (symbol-at-point)))
           (emr:goto-start-of-let-binding)
           (forward-symbol 1)
           (emr:let-binding-is-used? sym (list-at-point))))))

;;;###autoload
(defun emr-delete-let-binding-form ()
  "Delete the let binding around point."
  (interactive)
  (cl-assert (emr:looking-at-let-binding-symbol?))

  ;; HACK: extraction-refactor macro doesn't work for this one. Manually
  ;; manipulate the kill ring data so it can be restored.
  (let ((kr kill-ring))
    (unwind-protect
        (save-excursion
          ;; Delete binding.
          (emr:goto-open-round)
          (kill-sexp)

          ;; Simplify and tidy let expression.
          (emr:goto-start-of-let-binding)
          (kill-sexp)
          (->> (emr:read (car kill-ring))
            (emr:reformat-let-binding-list)
            (emr:simplify-let-form)
            (emr:print)
            (emr:format-defun)
            (emr:reindent-string)
            (insert))

          ;; Ensure whole form is correctly reindented.
          (mark-defun)
          (indent-region (region-beginning) (region-end)))

      ;; Restore kill-ring.
      (setq kill-ring kr))))

;;; TODO: Use body form index instead of crazy parsing.
;; (defun emr:fn-body-index (fn)
;;   "Return the minimum position of the body or &rest args for function fn."
;;   (when (fboundp fn)
;;     (->> (help-function-arglist fn)
;;       ;; &optional forms precede &rest forms.
;;       (--remove (equal '&optional it))
;;       (cl-position '&rest))))

;;; ----------------------------------------------------------------------------
;;; Declare commands with EMR.

;;; Implement function
(emr-declare-action emr-implement-function emacs-lisp-mode "implement function"
  :predicate (and (symbol-at-point)
                  (not (emr:looking-at-string?))
                  (not (thing-at-point 'comment))
                  (not (thing-at-point 'number))
                  (not (emr:looking-at-definition?))
                  (not (emr:looking-at-let-binding-symbol?))
                  (not (boundp (symbol-at-point)))
                  (not (fboundp (symbol-at-point)))))

;;; Inline variable
(emr-declare-action emr-inline-variable emacs-lisp-mode "inline"
  :predicate (emr:variable-definition? (emr:list-at-point)))

;;; Extract function
(emr-declare-action emr-extract-function emacs-lisp-mode "function"
  :predicate (not (or (emr:looking-at-definition?)
                      (emr:looking-at-let-binding-symbol?)))
  :description "defun")

;;; Let-bind variable
(emr-declare-action emr-extract-to-let emacs-lisp-mode "let-bind"
  :predicate (not (or (emr:looking-at-definition?)
                      (emr:looking-at-decl?)
                      (emr:looking-at-let-binding-symbol?)))
  :description "let")

;;; Inline let-binding
(emr-declare-action emr-inline-let-variable emacs-lisp-mode "inline binding"
  :predicate (and (emr:looking-at-let-binding-symbol?)
                  (emr:let-bound-var-at-point-has-usages?)))

;;; Delete unused let-binding
(emr-declare-action emr-delete-let-binding-form emacs-lisp-mode "delete binding"
  :description "unused"
  :predicate (and (emr:looking-at-let-binding-symbol?)
                  (not (emr:let-bound-var-at-point-has-usages?))))

;;; Extract variable
(emr-declare-action emr-extract-variable emacs-lisp-mode "variable"
  :predicate (and (not (emr:looking-at-definition?))
                  (not (emr:looking-at-let-binding-symbol?))
                  (thing-at-point 'defun))
  :description "defvar")

;;; Extract constant
(emr-declare-action emr-extract-constant emacs-lisp-mode "constant"
  :predicate (not (or (emr:looking-at-definition?)
                      (emr:looking-at-let-binding-symbol?)))
  :description "defconst")

;;; Eval and replace expression
(emr-declare-action emr-eval-and-replace emacs-lisp-mode "eval"
  :predicate (not (or (emr:looking-at-definition?)
                      (emr:looking-at-let-binding-symbol?)))
  :description "value")

;;; Extract autoload
(emr-declare-action emr-extract-autoload emacs-lisp-mode "autoload"
  :description "autoload"
  :predicate (and (or (functionp (symbol-at-point))
                      (emr:macro-boundp (symbol-at-point)))
                  (not (emr:variable-definition? (emr:list-at-point)))))

;;; Comment-out form
;;; Should be looking at a lisp list.
(emr-declare-action emr-comment-form emacs-lisp-mode "comment"
  :predicate (and (thing-at-point 'defun)
                  (not (emr:looking-at-comment?))))

(provide 'emr-elisp)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; emr-elisp.el ends here
