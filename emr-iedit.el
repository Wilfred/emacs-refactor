;;; emr-iedit.el -- Brief introduction here.

;; Author: YangYingchao <yangyingchao@gmail.com>

;;; Commentary:

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

(defun emr-iedit-in-region (arg)
  "Rename variable in this function"
  (interactive "P")
  (iedit-barf-if-lib-active)
  (let (occurrence
        complete-symbol
        (beg (region-beginning))
        (end (region-end)))
    (setq occurrence  (buffer-substring-no-properties
                       (mark) (point)))
    (setq iedit-only-complete-symbol-local complete-symbol)
    (setq mark-active nil)
    (run-hooks 'deactivate-mark-hook)
    (setq iedit-initial-string-local occurrence)
    (iedit-start (iedit-regexp-quote occurrence) beg end)))


(emr-declare-command 'emr-iedit-in-function
  :title "rename (in function)"
  :description ""
  :modes '(prog-mode)
  :predicate (lambda ()
               (and (not (iedit-region-active))
                    (emr-iedit:looking-at-iterator?)
                    (which-function))))

(emr-declare-command 'emr-iedit-in-region
  :title "rename (in region)"
  :description ""
  :modes '(prog-mode)
  :predicate (lambda ()
               (iedit-region-active)))

(emr-declare-command 'emr-iedit-global
  :title "rename"
  :description "rename all.."
  :modes '(prog-mode)
  :predicate (lambda ()
               (and (not (iedit-region-active))
                    (emr-iedit:looking-at-iterator?))))

(provide 'emr-iedit)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; emr-iedit.el ends here
