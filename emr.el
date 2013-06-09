;;; emr.el --- Emacs refactoring system.

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 0.3.6
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
  (ignore-errors
    (beginning-of-thing 'defun))
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
;;; the `emr-declare-command' macro. This macro adds builds a struct to
;;; represent the command and adds it to a table for later retrieval.
;;;
;;; When the user invokes the popup menu, each struct is transformed into a
;;; function that will return a popup if that command is available.

(cl-defstruct emr-refactor-spec
  function title description modes predicate)

(defvar emr:refactor-commands (make-hash-table :test 'equal)
  "A table of refactoring specs used to build menu items.")

(defun emr:documentation (sym)
  "Get the docstring for SYM. Does not display the arglist for functions."
  (ignore-errors
    (->> (documentation sym)
      (s-lines)
      ;; Remove the function arglist.
      (nreverse)
      (-drop 1)
      (nreverse)
      (s-join "\n")
      (s-trim))))

;;;###autoload
(defmacro* emr-declare-command
    (function &key modes title description predicate)
  "Define a refactoring command.

* FUNCTION is the refactoring command to perform. It should be
  either the name of a refactoring command or a
  lambda-expression.

* MODES is a symbol or list of symbols of the modes in which this
  command will be available. This will also enable the command
  for derived modes.

* TITLE is the name of the command that will be displayed in the
  popup menu.

* PREDICATE is a condition that must be satisfied to display this
  item. It should be a lambda-expression or function name.

* DESCRIPTION is shown to the left of the title in the popup
  menu."
  (declare (indent 1))
  (cl-assert (functionp function))
  (cl-assert title)
  (cl-assert modes)
  (cl-assert (or (functionp predicate)
                 (symbolp predicate)))
  ;; Add the created function into the global table of refactoring commands.
  `(puthash ',function
            (make-emr-refactor-spec
             :function ',function
             :title ,title
             :modes ',(if (symbolp modes) (list modes) modes)
             :predicate ,predicate
             :description ,description
             )
            emr:refactor-commands))

;;;###autoload
(defmacro* emr-extend-command
    (function &optional &key modes (predicate nil))
  "Extend an existing refactoring command to other major modes.
See the documentation for `emr-declare-command'."
  (declare (indent 1))
  (cl-assert (functionp function))
  (cl-assert modes)
  (cl-assert (or (null predicate)
                 (functionp predicate)
                 (symbolp predicate)))
  (let ((ms (if (symbolp modes) (list modes) modes)))
    `(let ((struct (gethash ',function emr:refactor-commands)))
       (setf (emr-refactor-spec-modes struct)
             (->> (emr-refactor-spec-modes struct) (-concat ',ms) (-uniq)))

       ;; Override the original predicate if one is supplied.
       (unless (null ',predicate)
         (let* ((cur (emr-refactor-spec-predicate struct))
                (fn (lambda () (funcall (if (derived-mode-p ',ms) ,predicate cur)))))
           (setf (emr-refactor-spec-predicate struct) fn))))))

(defun emr:hash-values (ht)
  "Return the hash values in hash table HT."
  (cl-loop for v being the hash-values in ht collect v))

(defun emr:make-popup (struct)
  "Test whether the refactoring specified by STRUCT is available.
Return a popup item for the refactoring menu if so."
  (when (and
         ;; 1. Test whether this command is available in the current
         ;; buffer's major mode.
         (apply 'derived-mode-p (emr-refactor-spec-modes struct))
         ;; 2. Run the declared predicate to test whether the refactoring
         ;; command is available in the current context.
         (ignore-errors
           (funcall (emr-refactor-spec-predicate struct))))
    ;; If the above tests succeed, create a popup for the
    ;; refactor menu.
    (popup-make-item (emr-refactor-spec-title struct)
                     :value (emr-refactor-spec-function struct)
                     :summary (emr-refactor-spec-description struct)
                     :document (emr:documentation (emr-refactor-spec-function struct)))))

;;;###autoload
(defun emr-show-refactor-menu ()
  "Show the refactor menu at point."
  (interactive)
  ;; Run each factory function and collect the menu items representing
  ;; available commands.
  (-if-let (actions (->> emr:refactor-commands
                      (emr:hash-values)
                      (-map 'emr:make-popup)
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

(defmacro emr:after-load (feature &rest forms)
  (declare (indent 1))
  `(eval-after-load ,feature
     '(progn ,@forms)))

;;;###autoload
(defun emr-initialize ()
  "Activate language support for EMR."

  (require 'emr-prog)

  ;; Lazily load support for individual languages.

  (emr:after-load "lisp-mode"
    (require 'emr-lisp)
    (require 'emr-elisp))
  (emr:after-load "cc-mode"
    (require 'emr-c)
    (emr-c-initialize))
  (emr:after-load "scheme"
    (require 'emr-scheme)))

(provide 'emr)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; emr.el ends here
