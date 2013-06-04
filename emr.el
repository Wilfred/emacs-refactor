;;; emr.el --- Emacs refactoring system.

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 0.3.5
;; Keywords: tools convenience refactoring
;; Package-Requires: ((s "1.3.1") (dash "1.2.0") (cl-lib "0.2") (popup "0.5.0") (emacs "24.1") (list-utils "0.3.0") (redshank "1.0.0") (paredit "24.0.0") (projectile "0.9.0"))
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
;; (define-key prog-mode-map (kbd "M-RET") 'emr-show-refactor-menu)
;; (eval-after-load "emr" '(emr-initialize))
;;
;; See README.md for more information.

;;; Code:

(require 'dash)
(require 's)
(require 'cl-lib)
(require 'popup)
(autoload 'in-string-p "thingatpt")
(autoload 'beginning-of-thing "thingatpt")

(defgroup emacs-refactor nil
  "Provides refactoring tools for Emacs."
  :group 'tools
  :prefix "emr-")

(defcustom emr-report-actions t
  "Non-nil means display an indication when a refactoring results in an insertion."
  :type 'checkbox
  :group 'emacs-refactor)

(defcustom emr-lines-between-toplevel-forms 1
  "The number of lines to try to preserve between toplevel forms when refactoring Lisps."
  :group 'emr)

(defcustom emr-popup-help-delay 1
  "The time to wait before showing documentation in the refactor menu."
  :group 'emr)

; ------------------

;;;; Utility functions

;;; Functions that could be useful to extensions.

;;;###autoload
(defun emr-move-above-defun ()
  "Move to the start of the current defun.
If the defun is preceded by comments, move above them."
  (interactive)
  ;; If we're at a defun already, prevent `beginning-of-defun' from moving
  ;; back to the preceding defun.
  (beginning-of-thing 'defun)
  ;; If there is a comment attached to this defun, skip over it.
  (while (save-excursion
           (forward-line -1)
           (and (emr-looking-at-comment?)
                (not (bobp))))
    (forward-line -1)))

;;;###autoload
(defun emr-looking-at-string? ()
  "Return non-nil if point is inside a string."
  (or (ignore-errors (and (in-string-p) t))
      (equal 'font-lock-string-face (face-at-point))))

;;;###autoload
(defun emr-looking-at-comment? ()
  "Non-nil if point is on a comment."
  (-contains? '(font-lock-comment-face font-lock-comment-delimiter-face)
              (face-at-point)))

;;;###autoload
(defun emr-blank? (str)
  "Non-nil if STR is null, empty or whitespace-only."
  (s-blank? (s-trim str)))

;;;###autoload
(defun emr-line-str ()
  "Return the contents of the current line."
  (buffer-substring (line-beginning-position)
                    (line-end-position)))

;;;###autoload
(defun* emr-blank-line? (&optional (point (point)))
  "Non-nil if POINT is on a blank line."
  (save-excursion
    (goto-char point)
    (emr-blank? (emr-line-str))))

;;;###autoload
(defun* emr-line-matches? (regex &optional (point (point)))
  "Non-nil if POINT is on a line that matches REGEX."
  (save-excursion
    (goto-char point)
    (s-matches? regex (emr-line-str))))

;;;###autoload
(defun emr-insert-above-defun (str)
  "Insert and indent STR above the current top level form.
Return the position of the end of STR."
  (save-excursion
    (let ((mark-ring nil))
      ;; Move to position above top-level form.
      (beginning-of-line)
      (emr-move-above-defun)
      (open-line 2)
      ;; Perform insertion.
      (insert str)
      ;; Ensure there is leading blank line.
      (save-excursion
        (emr-move-above-defun)
        (unless (save-excursion
                  (forward-line -1)
                  (emr-blank-line?))
          (open-line 1)))
      (point))))

;;;###autoload
(defun emr-collapse-vertical-whitespace ()
  "Collapse blank lines around point.
Ensure there are at most `emr-lines-between-toplevel-forms' blanks."
  (when (emr-blank-line?)
    (save-excursion
      ;; Delete blank lines.
      (search-backward-regexp (rx (not (any space "\n"))) nil t)
      (forward-line 1)
      (while (emr-blank-line?)
        (forward-line)
        (join-line))
      ;; Open a user-specified number of blanks.
      (open-line emr-lines-between-toplevel-forms))))

; ------------------

;;;; Reporting

;;; These commands may be used to describe the changes made to buffers. See
;;; example in emr-elisp.

(defun* emr:ellipsize (str &optional (maxlen (window-width (minibuffer-window))))
  "Chop STR and add ellipses if it exceeds MAXLEN in length."
  (if (> (length str) maxlen)
      (concat (substring-no-properties str 0 (1- maxlen)) "…")
    str))

(defun emr:indexed-lines (str)
  "Split string STR into a list of conses.
The index is the car and the line is the cdr."
  (--map-indexed (cons it-index it) (s-lines str)))

(defun emr:diff-lines (str1 str2)
  "Get the lines that differ between strings STR1 and STR2."
  (--remove (equal (car it) (cdr it))
            (-zip (emr:indexed-lines str1) (emr:indexed-lines str2))))

(defun* emr:report-action (description line text)
  "Report the action that occured at the point of difference.

Displays a short summary containing the line number, a
description of the change, and a snippet of text from the
buffer."
  (when emr-report-actions

    (->> (if (s-blank? text)
             "nil"
           (replace-regexp-in-string "[ \n\r\t]+" " " text))

      (format "%s line %s: %s" description line)
      (emr:ellipsize)
      (message))))

(defun emr:line-visible? (line)
  "Return true if LINE is within the visible bounds of the current window."
  (let* ((min (line-number-at-pos (window-start)))
         (max (line-number-at-pos (window-end))))
    (and (>= line min) (<= line max))))

;;;###autoload
(defmacro emr-reporting-buffer-changes (description &rest body)
  "Perform a refactoring action and show a brief diff.
* DESCRIPTION describes the overall action, and is shown to the user.
* BODY forms perform the refactor action."
  (declare (indent 1))
  `(let ((before-changes (buffer-string)))
     ,@body
     ;; Report changes.
     (-when-let (diff (and emr-report-actions
                           (car (emr:diff-lines before-changes (buffer-string)))))
       (cl-destructuring-bind (_ . (line . text)) diff
         (unless (emr:line-visible? line)
           (emr:report-action ,description line text))))))

; ------------------

;;;; Popup menu

;;; Items to be displayed in the refactoring popup menu are declared using
;;; the `emr-declare-action' macro. This macro transforms a specification
;;; into an executable function and adds it to the list of available
;;; refactoring commands.
;;;
;;; The `emr:refactor-commands' table stores those functions. Each function
;;; will dynamically create the popups to populate the menu.
;;;
;;; Whenever the user invokes the menu, we loop through the function in the
;;; hash table. Each function checks the current mode and runs a predicate,
;;; returning a popup item to display in the list if these tests succeed.

(defvar emr:refactor-commands (make-hash-table :test 'equal)
  "A hashtable of refactoring commands used to build menu items.")

(defun emr:documentation (sym)
  "Get the docstring for SYM. Does not display the arglist for functions."
  (ignore-errors
    (->> (documentation sym)
      (s-lines)
      ;; Remove the function arglist.
      (nreverse)
      (--drop-while (s-matches? (rx bol (* space) "(") it))
      (nreverse)
      (s-join "\n")
      (s-trim))))

;;;###autoload
(defmacro* emr-declare-action (function &key modes title (predicate t) description)
  "Define a refactoring command.

* FUNCTION is the refactoring command to perform.

* MODE is the major mode in which this command will be
  available. Includes derived modes.

* TITLE is the name of the command that will be displayed in the popup menu.

* PREDICATE is a condition that must be satisfied to display this item.
If PREDICATE is not supplied, the item will always be visible for this mode.

* DESCRIPTION is shown to the left of the title in the popup menu."
  (declare (indent 1))
  (cl-assert modes)
  (cl-assert title)
  ;; Allow both lists and symbols for the MODES argument.
  (let ((modes (if (symbolp modes) (list modes) modes)))
    `(let ((fname ',(intern (format "%s--%s" function title)))

           ;; Create factory function.
           ;;
           ;; This function will be run whenever the user invokes the popup
           ;; menu. It checks whether the given refactoring is available,
           ;; and returns either a new popup menu item or nil.
           (fn (lambda ()
                 (when (and
                        ;; 1. Test whether this command is available in the
                        ;; current buffer's major mode.
                        (apply 'derived-mode-p ',modes)
                        ;; 2. Run the declared predicate to test whether
                        ;; the refactoring command is available in the
                        ;; current context.
                        (ignore-errors
                          (eval ,predicate)))
                   ;; If the above tests succeed, create a popup for the
                   ;; refactor menu.
                   (popup-make-item ,title
                                    :value ',function
                                    :summary ,description
                                    :document (emr:documentation ',function))))))

       ;; Add the created function into the global table of refactoring
       ;; commands.
       (puthash fname fn emr:refactor-commands)
       ',function)))

(defun emr:hash-values (ht)
  "Return the hash values in hash table HT."
  (cl-loop for v being the hash-values in ht collect v))

;;;###autoload
(defun emr-show-refactor-menu ()
  "Show the refactor menu at point."
  (interactive)
  ;; Run each factory function and collect the menu items representing
  ;; available commands.
  (-if-let (actions (->> emr:refactor-commands
                      (emr:hash-values)
                      (-map 'funcall)
                      (-remove 'null)))
    ;; Display the menu.
    (atomic-change-group
      (-when-let (action (popup-menu*
                          actions
                          :isearch t
                          :help-delay emr-popup-help-delay))
        (call-interactively action)))

    ;; Having no items to show implies that no refactoring commands are
    ;; available.
    (message "No refactorings available")))

; ------------------

;;;###autoload
(defun emr-initialize ()
  "Activate language support for EMR."

  (require 'emr-prog)

  ;; Lazily load support for individual languages.

  (eval-after-load "lisp-mode"
    '(progn
       (require 'emr-lisp)
       (require 'emr-elisp)))

  (eval-after-load "cc-mode"
    '(progn
       (require 'emr-c)
       (emr-c-initialize))))

(provide 'emr)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; emr.el ends here
