;;; emr --- Emacs refactoring system.

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 0.2
;; Keywords: tools convenience refactoring
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

;; Add this package to your load path and add an autoload for `emr-show-refactor-menu`.
;; Bind the `emr-show-refactor-menu` command to something convenient.
;;
;; (autoload 'emr-show-refactor-menu "emr")
;; (add-hook 'prog-mode-hook
;;           (lambda () (local-set-key (kbd "M-RET") 'emr-show-refactor-menu)))
;;
;; See README.md for more information.

;;; Code:

(require 'dash)
(require 's)
(require 'cl-lib)
(require 'thingatpt)
(require 'popup)

(defgroup emacs-refactor nil
  "Provides refactoring tools for Emacs."
  :group 'tools
  :prefix "emr-")

(defcustom emr-report-actions t
  "Non-nil means display an indication when a refactoring results in an insertion."
  :type 'checkbox
  :group 'emacs-refactor)

;;; ----------------------------------------------------------------------------
;;; Macros
;;;
;;; Convenience macros.

(defun emr--macro-boundp (symbol)
  "Test whether SYMBOL is bound as a macro."
  (and (functionp symbol)
       ;; This may throw an error if it's a normal function.
       (ignore-errors
         (eq (car (symbol-function symbol)) 'macro))))

(defmacro emr--defmacro-safe (symbol arglist &rest body)
  "Define the given macro only if it is not already defined.
SYMBOL is the name of the macro.
ARGLIST is a cl-style argument list.
BODY is the body of the macro.
See `cl-defmacro'."
  (declare (doc-string 3) (indent defun))
  (cl-assert (symbolp symbol))
  (cl-assert (listp arglist))
  `(unless (emr--macro-boundp ',symbol)
     (cl-defmacro ,symbol ,arglist ,@body)))

(emr--defmacro-safe when-let ((var form) &rest body)
  "Execute BODY forms with bindings only if FORM evaluates to a non-nil value."
  (declare (indent 1))
  `(let ((,var ,form))
     (when ,var
       ,@body)))

(emr--defmacro-safe if-let ((var form) then &rest else)
  "Execute THEN form with bindings if FORM evaluates to a non-nil value,
otherwise execute ELSE forms without bindings."
  (declare (indent 1))
  `(let ((,var ,form))
     (if ,var ,then ,@else)))

;;; ----------------------------------------------------------------------------
;;; Reporting
;;;
;;; These commands may be used to describe the changes made to buffers. See
;;; example in emr-elisp.

(cl-defun emr--ellipsize (str &optional (maxlen (window-width (minibuffer-window))))
  (if (> (length str) maxlen)
      (concat (substring-no-properties str 0 (1- maxlen)) "…")
    str))

(defun emr--indexed-lines (str)
  "Split string STR into a list of conses.
The index is the car and the line is the cdr."
  (--map-indexed (cons it-index it) (s-lines str)))

(defun emr--diff-lines (str1 str2)
  "Get the lines that differ between strings STR1 and STR2."
  (--remove (equal (car it) (cdr it))
            (-zip (emr--indexed-lines str1) (emr--indexed-lines str2))))

(cl-defun emr--report-action (description line text)
  "Report the action that occured at the point of difference."
  (message
   (emr--ellipsize
    (format "%s line %s: %s"
            description
            line
            (if (s-blank? text) "nil"
              (replace-regexp-in-string "[ \n\r\t]+" " " text))))))

;;; ----------------------------------------------------------------------------
;;; Popup menu
;;; Items to be displayed in the refactoring popup menu are added using the
;;; `emr-declare-action' macro.

(defvar emr--refactor-commands '()
  "A list of refactoring commands used to build menu items.")

;;;###autoload
(cl-defmacro emr-declare-action (function mode title &key (predicate t) description)
  "Define a refactoring command.
FUNCTION is the refactoring command to perform.
MODE is the major mode in which this
TITLE is the name of the command that will be displayed in the popup menu.
PREDICATE is a condition that must be satisfied to display this item.
If PREDICATE is not supplied, the item will always be visible for this mode.
DESCRIPTION is shown to the left of the titile in the popup menu."
  (declare (indent 3))
  (let ((fname (intern (format "emr--gen--%s--%s" mode title))))
    `(progn
       ;; Define a function to encapsulate the predicate. Also ensures each
       ;; refactoring command is only added once.
       (defun ,fname nil
         (when (and (derived-mode-p ',mode)
                    (ignore-errors
                      (eval ,predicate)))
           (popup-make-item ,title :value ',function :summary ,description)))
       ;; Make this refactoring available in the popup menu.
       (add-to-list 'emr--refactor-commands ',fname t))))



;;;###autoload
(defun emr-show-refactor-menu ()
  "Show the extraction menu at point."
  (interactive)
  (if-let (actions (->> emr--refactor-commands
                     (-map 'funcall)
                     (-remove 'null)))
    (atomic-change-group
      (when-let (action (popup-menu* actions :isearch t))
        (call-interactively action)))
    (error "No refactorings available")))

(provide 'emr)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; emacs-refactor.el ends here
