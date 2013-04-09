;;; elr --- Emacs refactoring system.

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
;; (autoload 'elr-show-refactor-menu "elr")
;; (add-hook 'prog-mode-hook
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

;;; ----------------------------------------------------------------------------
;;; Macros
;;;
;;; Convenience macros.

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
;;; Reporting
;;;
;;; These commands may be used to describe the changes made to buffers. See
;;; example in elr-elisp.

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
;;; Popup menu
;;; Items to be displayed in the refactoring popup menu are added using the
;;; `elr-declare-action' macro.

(defvar elr--refactor-commands '()
  "A list of refactoring commands used to build menu items.")

(cl-defmacro elr-declare-action (function mode title &key (predicate t) description)
  "Define a refactoring command.
FUNCTION is the refactoring command to perform.
MODE is the major mode in which this
TITLE is the name of the command that will be displayed in the popup menu.
PREDICATE is a condition that must be satisfied to display this item.
If PREDICATE is not supplied, the item will always be visible for this mode.
DESCRIPTION is shown to the left of the titile in the popup menu."
  (declare (indent 3))
  (let ((fname (intern (format "elr--gen--%s--%s" mode title))))
    `(progn
       ;; Define a function to encapsulate the predicate. Also ensures each
       ;; refactoring command is only added once.
       (defun ,fname nil
         (when (and (derived-mode-p major-mode ',mode)
                    (eval ,predicate))
           (popup-make-item ,title :value ',function :summary ,description)))
       ;; Make this refactoring available in the popup menu.
       (add-to-list 'elr--refactor-commands ',fname t))))

(defun elr-show-refactor-menu ()
  "Show the extraction menu at point."
  (interactive)
  (if-let (actions (->> elr--refactor-commands
                     (-map 'funcall)
                     (-remove 'null)))
    (atomic-change-group
      (when-let (action (popup-menu* actions :isearch t))
        (call-interactively action)))
    (error "No refactorings available")))

(require 'elr-elisp)

(provide 'elr)

;;; NB: callargs warnings disabled to prevent format warnings caused by
;;; `cl-assert', as of Emacs 24.3.50 darwin.

;; Local Variables:
;; lexical-binding: t
;; byte-compile-warnings: (not callargs)
;; End:

;;; elisp-refactor.el ends here
