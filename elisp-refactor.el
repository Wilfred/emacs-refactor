;;; elisp-refactor --- Refactoring commands for Emacs Lisp.

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Keywords: tools, elisp, convenience
;; Dependencies: s, dash, cl-lib, popup

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

;; Add this package to your load path and add an autoload for `elr-show-refactor-menu`.
;; Bind the `elr-show-refactor-menu` command to something convenient.
;;
;; (autoload 'elr-show-refactor-menu "elisp-refactor")
;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda () (local-set-key (kbd "M-RET") 'elr-show-refactor-menu)))
;;
;; See README.md for more information.

;;; Code:

(require 'dash)
(require 's)
(require 'cl-lib)
(require 'thingatpt)
(require 'popup)

(defgroup elisp-refactor nil
  "Provides refactoring tools for Emacs Lisp."
  :group 'tools
  :prefix "elr-")

(defcustom elr-report-actions t
  "Non-nil means display an indication when a refactoring results in an insertion."
  :type 'checkbox
  :group 'elisp-refactor)

;;; Macros

(defun elr--macro-boundp (symbol)
  "Test whether SYMBOL is bound as a macro."
  (and (functionp symbol)
       ;; This may throw an error if it's a normal function.
       (ignore-errors
         (eq (car (symbol-function symbol)) 'macro))))

(defmacro elr--defmacro-safe (symbol arglist &rest body)
  "Define the given macro only if it is not already defined.
SYMBOL is the name of the macro.
ARGLIST is a cl-style argument list.
BODY is the body of the macro.
See `cl-defmacro'."
  (declare (doc-string 3) (indent defun))
  (cl-assert (symbolp symbol))
  (cl-assert (listp arglist))
  `(unless (elr--macro-boundp ',symbol)
     (cl-defmacro ,symbol ,arglist ,@body)))

(elr--defmacro-safe when-let ((var form) &rest body)
  "Execute BODY forms with bindings only if FORM evaluates to a non-nil value."
  (declare (indent 1))
  `(let ((,var ,form))
     (when ,var
       ,@body)))

(elr--defmacro-safe if-let ((var form) then &rest else)
  "Execute THEN form with bindings if FORM evaluates to a non-nil value,
otherwise execute ELSE forms without bindings."
  (declare (indent 1))
  `(let ((,var ,form))
     (if ,var ,then ,@else)))

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
;;; Reporting

(cl-defun elr--ellipsize (str &optional (maxlen (window-width (minibuffer-window))))
  (if (> (length str) maxlen)
      (concat (substring-no-properties str 0 (1- maxlen)) "…")
    str))

(defun elr--indexed-lines (str)
  "Split string STR into a list of conses.  The index is the car and the line is the cdr."
  (--map-indexed (cons it-index it) (s-lines str)))

(defun elr--diff-lines (str1 str2)
  "Get the lines that differ between strings STR1 and STR2."
  (--remove (equal (car it) (cdr it))
            (-zip (elr--indexed-lines str1) (elr--indexed-lines str2))))

(cl-defun elr--report-action (description line text)
  "Report the action that occured at the point of difference."
  (message
   (elr--ellipsize
    (format "%s line %s: %s"
            description
            line
            (if (s-blank? text) "nil"
              (replace-regexp-in-string "[ \n\r\t]+" " " text))))))

;;; ----------------------------------------------------------------------------
;;; Refactoring commands

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

            ;; Remove the line where the variable defininition used to be.
            (kill-line)

            ;; Perform inlining.
            ;; elr--extraction-refactor will report the first insertion. If
            ;; there are none or more than one insertion, override this report.
            (if-let (lines (-map 'int-to-string (elr--replace-usages vals)))
              (when (> (length lines) 1)
                (message "Inlining applied at lines %s" (s-join ", " lines)))
              (message "No usages found")))

        (error "No value to inline for %s" (car vals)))
      (error "Not a variable definition"))))

;;; ----------------------------------------------------------------------------
;;; UI commands

;;; The refactor menu is context-sensitive. Popup items are created with
;;; constructor functions which return nil when given refactoring is not
;;; available at POINT.

(defun elr--inline-variable-popup ()
  (when (elr--variable-definition? (elr--list-at-point))
    (popup-make-item "inline" :value 'elr-inline-variable)))

(defun elr--extract-function-popup ()
  (unless (elr--looking-at-definition?)
    (popup-make-item "function" :value 'elr-extract-function :summary "defun")))

(defun elr--extract-variable-popup ()
  (unless (elr--looking-at-definition?)
    (popup-make-item "variable" :value 'elr-extract-variable :summary "defvar")))

(defun elr--extract-constant-popup ()
  (unless (elr--looking-at-definition?)
    (popup-make-item "constant" :value 'elr-extract-constant :summary "defconst")))

(defun elr--autoload-exists? (function str)
  "Returns true if an autoload for FUNCTION exists in string STR."
  (s-contains? (format "(autoload '%s " function) str))

(defun elr--extract-autoload-popup ()
  (when (and (functionp (symbol-at-point))
             (not (elr--variable-definition? (elr--list-at-point)))
             (not (elr--autoload-exists? (symbol-at-point) (buffer-string))))
    (popup-make-item "autoload" :value 'elr-extract-autoload :summary "autoload")))

(defun elr--eval-and-replace-popup ()
  (unless (elr--looking-at-definition?)
    (popup-make-item "eval" :value 'elr-eval-and-replace :summary "value")))

(defvar elr--refactor-options
  (list 'elr--inline-variable-popup
        'elr--extract-function-popup
        'elr--extract-variable-popup
        'elr--extract-constant-popup
        'elr--extract-autoload-popup
        'elr--eval-and-replace-popup)
  "Contains possible refactorings and determines their listing order.")

(defun elr-show-refactor-menu ()
  "Show the extraction menu at point."
  (interactive)
  (if-let (actions (->> elr--refactor-options
                     (-map 'funcall)
                     (--filter (not (null it)))))
    (atomic-change-group
      (call-interactively (popup-menu* actions :isearch t)))
    (error "No refactorings available")))

(provide 'elisp-refactor)

;;; NB: callargs warnings disabled to prevent format warnings caused by
;;; `cl-assert', as of Emacs 24.3.50 darwin.

;; Local Variables:
;; lexical-binding: t
;; byte-compile-warnings: (not callargs)
;; End:

;;; elisp-refactor.el ends here
