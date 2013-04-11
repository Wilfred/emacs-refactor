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

;;; Refactoring commands for Emacs Lisp. Part of the ELR suite.

;;; Code:

(require 'cl-lib)

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
  "Unescape question marks that were escaped by the reader."
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

(defun emr--reindent-string (form-str)
  "Reformat FORM-STRING, assuming it is a Lisp fragment."
  (with-temp-buffer
    (lisp-mode-variables)
    (insert form-str)
    ;; Indent each line.
    (mark-whole-buffer)
    (indent-for-tab-command)
    (buffer-string)))

(defun emr--insert-above (form-str)
  "Insert and indent FORM-STR above the current top level form.
Return the position of the end of FORM-STR."
  (save-excursion
    ;; Move to position above top-level form.
    (beginning-of-defun)
    (beginning-of-line)
    (newline)
    (forward-line -1)
    (back-to-indentation)
    (insert (emr--reindent-string form-str))
    (prog1 (point)
      (newline-and-indent))))

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

(defun emr--global-var? (sym)
  (let ((s (symbol-name sym)))
    (or (s-contains? "--" s)
        (s-contains? "/" s))))

(defvar emr--special-symbols '(&rest &optional &key &allow-other-keys \,\@ \,)
  "A list of symbols that should be ignored by variable searches.")

;;; FIXME:
;;; This needs more work to make it more accurate.
(defun emr--unbound-symbols (form)
  "Try to find the symbols in FORM that do not have variable bindings."
  (->> (cl-list* form)
    (-flatten)
    (-filter 'symbolp)
    (--remove
     (or (symbol-function it)
         (booleanp it)
         (keywordp it)
         (emr--global-var? it)
         (-contains? emr--special-symbols it)))
    (-uniq)))

(defun emr--unbound-symbols-string ()
  "Format a string of the unbound symbols in the list at point."
  (->> (emr--list-at-point)
    (emr--unbound-symbols)
    (-map 'symbol-name)
    (s-join " ")
    (s-trim)))

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
         (kill-region (region-beginning)
                      (region-end))

       (emr--goto-open-round-or-quote)
       (kill-sexp))


     (let
         ;; Define BINDING if supplied.
         ,(when binding `((,binding (emr--read (car kill-ring)))))

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
          ;; Try to pretty-format.
          (save-excursion (end-of-defun) (beginning-of-defun) (indent-sexp)))
        (nreverse lines)))))

(defun emr-inline-variable ()
  "Inline the variable at defined at point.
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

(defun emr-eval-and-replace ()
  "Replace the form at point with its value."
  (interactive)
  (emr--extraction-refactor (sexp) "Replacement at"
    (let ((str (prin1-to-string (eval sexp))))
      (insert str)
      (indent-for-tab-command))))

(defun emr--read-with-default (prompt value)
  "Prompt for user input, showing PROMPT with an inline default VALUE."
  (let ((val (s-trim (format "%s" value))))
    (s-trim
     (if (s-blank? val)
         (read-string (format "%s:  " prompt))
       (read-string (format "%s (default: %s): "  prompt val) nil nil val)))))

(defun emr--read-args ()
  "Read an arglist from the user."
  (let* ((syms (emr--unbound-symbols-string))
         (input (emr--read-with-default "Arglist" syms)))
    (unless (or (s-blank? input)
                (s-matches? (rx (or "()" "nil")) input))
      (read (format "(%s)" input)))))

(defun emr--format-defun (defun-str)
  "Format DEFUN-STR to a prettier defun representation."
  (replace-regexp-in-string
   (rx bol "(" (* nonl) "def" (* nonl) (group "nil" (* space)) eol)
   "()"
   defun-str t nil 1))

(defun emr-extract-function (name arglist)
  "Extract a function from the sexp beginning at point.
NAME is the name of the new function.
ARGLIST is its argument list."
  (interactive (list (read-string "Name: ")
                     (emr--read-args)))
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
            ,(-drop-while 'emr--newline? sexp))))))))

(defun emr-extract-variable (name)
  "Extract a form as the argument to a defvar named NAME."
  (interactive "sName: ")
  (cl-assert (not (s-blank? name)) () "Name must not be blank")
  (emr--extraction-refactor (sexp) "Extracted to"
    ;; Insert usage.
    (insert (s-trim name))
    ;; Insert definition.
    (emr--insert-above
     (emr--print
      (list 'defvar (intern name) sexp)))))

(defun emr-extract-constant (name)
  "Extract a form as the argument to a defconst named NAME."
  (interactive "sName: ")
  (cl-assert (not (s-blank? name)) () "Name must not be blank")
  (emr--extraction-refactor (sexp) "Extracted to"
    ;; Insert usage
    (insert (s-trim name))
    ;; Insert definition.
    (emr--insert-above
     (emr--print
      (list 'defconst (intern name) sexp)))))

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

(defun emr-comment-form ()
  "Comment out the list at point."
  (interactive)
  (if (region-active-p)
      (comment-region (region-beginning)
                      (region-end))
    (emr--goto-open-round-or-quote)
    (mark-sexp)
    (comment-region (region-beginning) (region-end))))

(defun emr-implement-function (name arglist)
  "Insert a function definition for NAME with ARGLIST."
  (interactive (list
                (emr--read (emr--read-with-default "Name" (symbol-at-point)))
                (emr--read-args)))

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

(defun emr--find-binding-references (current-form binding-forms)
  (let* ((sym (emr--first-atom current-form))
         (rest (emr--bindings-after sym binding-forms)))
    (-contains? rest sym)))

(defun emr--duplicates? (xs)
  "Return non-nil if any elements in XS are duplicated."
  (/= (length xs) (length (-distinct xs))))

(defun emr--recursive-bindings? (binding-forms)
  "Test whether let BINDING-FORMS are dependent on one-another."
  (let ((syms (->> binding-forms
                (--map (or (car-safe it) it))
                (-remove 'emr--nl-or-comment?)))
        (vals (->> binding-forms
                (-map 'cdr-safe)
                (-remove 'emr--nl-or-comment?) )))
    (or (emr--duplicates? syms)
        (-intersection syms (-flatten vals)))))

;;; Variable insertion.

(defun emr--let-wrap (form &optional splice?)
  "Ensure FORM is wrapped with a `let' form. No change if FORM is already a let form."
  ;; Trim leading newlines.
  (let ((ls (if (listp form) (-drop-while 'emr--newline? form) form)))
    (cond
     ((emr--let-form? ls)
      ls)

     (splice?
      (cl-list* 'let nil :emr--newline ls))

     (t
      (list     'let nil :emr--newline ls)))))

(cl-defun emr--insert-let-var (symbol value-form (let bindings &rest body))
  "Insert a binding into the given let expression."
  (cl-assert (emr--let-form? (list let)))
  (let* ((new `((,symbol ,value-form)))
         ;; Add to existing bindings if possible.
         (updated (if bindings (-concat bindings (list :emr--newline) new) new))
         )
    ;; Combine updated forms. If the updated bindings are recursive, use let*.
    `(,(if (emr--recursive-bindings? updated) 'let* 'let)
      ,updated
      ,@body)))

(defun emr--maybe-skip-docstring (xs)
  "Skip docstring if it is at the head of XS.
If there are forms afterwards, do not skip."
  (if (and (stringp (car-safe xs))
           (-remove 'emr--newline? (cdr xs)))
      (cdr xs)
    xs))

(defun emr--decl-form? (form)
  "Non-nil if form is an `interactive' spec, assertion, or `declare' form."
  (-contains? '(interactive declare assert cl-assert)
               (or (car-safe form) form)))

(defun emr--index-of (elt coll)
  "Find the index of ELT in COLL, or nil if not found."
  (->> coll
    (--map-indexed (cons it-index it))
    (--first (equal elt (cdr it)))
    (car)))

(defun emr--nl-or-comment? (form)
  (or (equal :emr--newline form)
      (equal :emr--comment form)))

(defun emr--split-defun (form)
  "Split a defun FORM into a list of (header body).
The body is the part of FORM that can be safely transformed without breaking the definition."
  (cl-assert (emr--defun-form? form) () "Not a recognised definition form.")
  (let* (
         ;; Inspect the structure of the form. A definition contains an optional
         ;; docstring and interactive/declare specs which should not be changed
         ;; by operations to the body, so we skip those.

         (split-point

          (->> form
            ;; Newlines and comments not semantically useful here.
            (-remove 'emr--nl-or-comment?)
            ;; Skip defun, symbol and arglist.
            (-drop 3)
            (emr--maybe-skip-docstring)
            ;; Skip comments, INTERACTIVE, DECLARE and assertion forms.
            (--drop-while (or (emr--nl-or-comment? it) (emr--decl-form? it)))
            ;; We should now be pointed at the first body form.
            (car)))

         ;; Now that we know which form is probably the body, get its position
         ;; in FORM and split FORM at that point.
         (pos (emr--index-of split-point form))
         )
    (cl-assert (integerp pos) () "Unable find body in `%s`" form)
    (-split-at pos form)))

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

(cl-defun emr--recombine-forms (header body (&rest docstring))
  (cond ((emr--defun-form? header) `(,@header ,body))

        ((emr--variable-definition? header)
         `(,@header ,body ; Put non-nil docstring on a new line.
                    ,@(when docstring (cons :emr--newline docstring))))
        (t
         body)))

(defun emr--add-let-binding (symbol value form)
  "Insert (SYMBOL VALUE & DOC) into FORM, encapsulating FORM in a `let' expression if necessary."
  (destructuring-bind (header body &optional docstring)
      (emr--partition-body form)

    (emr--recombine-forms
     header

     ;; Wrap BODY in a let expression.
     ;; Defun forms should have their body spliced into the let form.
     (->> (emr--defun-form? header)
       (emr--let-wrap body)
       (emr--insert-let-var symbol value ))

     docstring)))

(defun emr-extract-to-let (symbol)
  "Extract the form at point into a let expression.
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
;;; Declare commands with ELR.

;;; Inline variable
(emr-declare-action emr-inline-variable emacs-lisp-mode "inline"
  :predicate (emr--variable-definition? (emr--list-at-point)))

;;; Extract function
(emr-declare-action emr-extract-function emacs-lisp-mode "function"
  :predicate (not (emr--looking-at-definition?))
  :description "defun")

;;; Let-bind variable.
(emr-declare-action emr-extract-to-let emacs-lisp-mode "let-bind"
  :predicate (or (not (emr--looking-at-definition?))
                 (region-active-p))
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

;;; Implement function.
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

;;; Uncomment form.
(defun emr--looking-at-comment? ()
  "Non-nil if point is on a comment."
  (when-let (comment (save-excursion
                       (beginning-of-line)
                       (comment-search-forward (point-at-eol) t)))
    ;; Test if there is a comment-start before point.
    (<= comment (point))))

(defun emr--find-comment-block-start ()
  (let (pos)
    (save-excursion
      (while (search-backward-regexp (eval `(rx bol (* space) ,comment-start))
                                     (line-beginning-position) t)
        (setq pos (point))))
    pos))

(defun emr-uncomment-form ()
  (interactive)
  (let nil

    (emr--find-comment-block-start)))

(emr-declare-action emr-uncomment-form emacs-lisp-mode "uncomment"
  :predicate (emr--looking-at-comment?))

(provide 'emr-elisp)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; emr-elisp.el ends here
