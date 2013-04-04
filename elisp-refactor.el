;;; elisp-refactor --- Refactoring commands for Emacs Lisp.

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

;; Add this package to your load path and add an autoload for `elr/show-refactor-menu`.
;; Bind the `elr/show-refactor-menu` command to something convenient.

;; (autoload 'elr/show-refactor-menu "elisp-refactor")
;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda () (local-set-key (kbd "M-RET") 'elr/show-refactor-menu)))

;; See README.md for more information.

;;; Dependencies:
;; s dash cl-lib popup

;;; Code:

(require 'dash)
(require 's)
(require 'cl-lib)
(require 'thingatpt)
(require 'popup)

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
  (newline))

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

(defun elr--format-defun (name arglist body)
  (with-temp-buffer
    (lisp-mode-variables)
    (insert (format "(defun %s (%s) \n%s)" name arglist body))
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
;;; Refactoring commands

(defmacro elr--extracting-list (&rest body)
  "Kill the sexp near point then execute BODY forms.
The kill ring is reverted at the end of the body."
  (declare (indent 0))
  `(save-excursion
     (elr--goto-open-round-or-quote)
     (kill-sexp)
     (unwind-protect
         (progn ,@body)
       ;; Revert kill-ring pointer.
       (setq kill-ring (cdr kill-ring)))))

(defun elr/eval-and-replace ()
  "Replace the form at point with its value."
  (interactive)
  (elr--extracting-list
    (let ((str (prin1-to-string (eval (read (car kill-ring))))))
      (insert str)
      (indent-for-tab-command))))

(defun elr--read-args ()
  (let ((syms (elr--unbound-symbols-string)))
    (if (s-blank? syms)
        (read-string "Arglist: ")
      (read-string (format "Arglist (default: %s): " syms) nil nil syms))))

(defun elr/extract-function (name arglist)
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
    (elr--extracting-list
      (insert (apply 'elr--format-list name (s-split-words args)))
      (beginning-of-defun)
      (elr--insert-above (elr--format-defun name args (car kill-ring))))))

(defun elr/extract-variable (name)
  "Extract a form as the argument to a defvar named NAME."
  (interactive "sName: ")
  (cl-assert (not (s-blank? name)) t "Name must not be blank")
  (let ((name (s-trim name)))
    (elr--extracting-list
      (insert name)
      (beginning-of-defun)
      (elr--insert-above (elr--format-list "defvar" name (car kill-ring))))))

(defun elr/extract-constant (name)
  "Extract a form as the argument to a defconst named NAME."
  (interactive "sName: ")
  (cl-assert (not (s-blank? name)) t "Name must not be blank")
  (let ((name (s-trim name)))
    (elr--extracting-list
      (insert name)
      (beginning-of-defun)
      (elr--insert-above (elr--format-list "defconst" name (car kill-ring))))))

(defun elr/extract-autoload (function file)
  "Create an autoload for FUNCTION.
FILE is the file that declares FUNCTION.
See `autoload' for details."
  (interactive
   (let* ((sym  (intern (or (thing-at-point 'symbol) (read-string "Function: "))))
          (file (or (elr--symbol-file-name sym)
                    (read-string "File: "))))
     (list sym file)))
  (save-excursion
    (let ((form  (format "(autoload '%s \"%s\")" function file)))
      (if (elr--goto-first-match "^(autoload ")
          (progn (forward-line 1) (end-of-line) (newline) (insert form))
        (beginning-of-defun)
        (elr--insert-above form)))))

;;; ----------------------------------------------------------------------------
;;; UI commands

(defun elr--extract-function-popup ()
  (popup-make-item "function" :value 'elr/extract-function :summary "defun"))

(defun elr--extract-variable-popup ()
  (popup-make-item "variable" :value 'elr/extract-variable :summary "defvar"))

(defun elr--extract-constant-popup ()
  (popup-make-item "constant" :value 'elr/extract-constant :summary "defconst"))

(defun elr--extract-autoload-popup ()
  (when (functionp (symbol-at-point))
    (popup-make-item "autoload" :value 'elr/extract-autoload :summary "autoload")))

(defun elr--eval-and-replace-popup ()
  (popup-make-item "eval" :value 'elr/eval-and-replace :summary "value"))

(defvar elr--refactor-options
  (list 'elr--extract-function-popup
        'elr--extract-variable-popup
        'elr--extract-constant-popup
        'elr--extract-autoload-popup
        'elr--eval-and-replace-popup))

(defun elr/show-refactor-menu ()
  "Show the extraction menu at point."
  (interactive)
  (if-let (actions (->> elr--refactor-options
                     (-map 'funcall)
                     (--filter (not (null it)))))
    (call-interactively (popup-menu* actions :isearch t))
    (error "No refactorings available")))

(provide 'elisp-refactor)

;;; NB: callargs warnings disabled to prevent format warnings caused by
;;; `cl-assert', as of Emacs 24.3.50 darwin.

;; Local Variables:
;; lexical-binding: t
;; byte-compile-warnings: (not callargs)
;; End:

;;; elisp-refactor.el ends here
