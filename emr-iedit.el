;;; emr-iedit.el -- Brief introduction here.
;; Copyright (C) 2015 EMR Project
;; Author: YangYingchao <yangyingchao@gmail.com>

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
;; Rename variables/functions/macros with iedit.

;;; Code:

(require 'emr)
(require 's)
(require 'dash)
(require 'thingatpt)
(require 'which-func)

(require 'iedit)

(defconst emr-iedit:rx-iterator
  (rx (+ (or alnum "-" "_"))))

(defun emr-iedit:looking-at-iterator? ()
  (thing-at-point-looking-at emr-iedit:rx-iterator))

(defun emr-iedit-global ()
  "Rename a variable in this buffer.."
  (interactive)
  (iedit-mode t))

(defun emr-iedit-in-function (arg)
  "Rename variable in this function"
  (interactive "P")
  (iedit-barf-if-lib-active)
  (let (occurrence
        complete-symbol
        (beg (if (eq major-mode 'occur-edit-mode) ; skip the first occurrence
                 (next-single-char-property-change 1 'read-only)
               (point-min)))
        (end (point-max)))
    (and iedit-current-symbol-default
         (setq occurrence (funcall iedit-current-symbol)))
    (when iedit-only-at-symbol-boundaries
      (setq complete-symbol t))
    (save-excursion
      (mark-defun)
      (setq beg (region-beginning))
      (setq end (region-end)))
    (setq iedit-only-complete-symbol-local complete-symbol)
    (setq mark-active nil)
    (run-hooks 'deactivate-mark-hook)
    (setq iedit-initial-string-local occurrence)
    (iedit-start (iedit-regexp-quote occurrence) beg end)))


(emr-declare-command 'emr-iedit-in-function
  :title "rename (in function)"
  :description "in function"
  :modes '(prog-mode)
  :predicate (lambda ()
               (and (not (iedit-region-active))
                    (emr-iedit:looking-at-iterator?)
                    (which-function))))

(emr-declare-command 'emr-iedit-global
  :title "rename"
  :description "globally"
  :modes '(prog-mode)
  :predicate (lambda ()
               (and (not (iedit-region-active))
                    (emr-iedit:looking-at-iterator?))))

(define-key iedit-mode-keymap (kbd "C-c C-c") 'iedit-mode)

(provide 'emr-iedit)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; emr-iedit.el ends here
