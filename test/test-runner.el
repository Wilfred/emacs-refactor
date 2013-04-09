;;; test-runner --- Runs elr tests.

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

;; Runs elr tests.

;;; Code:

(require 'ert)

(defconst dependencies '(popup s dash))

;;; ----------------------------------------------------------------------------
;;; Configuration

(defun load-packages ()
  (init-melpa)
  (mapc 'require-package dependencies))

(defun init-melpa ()
  (setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")))
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents)))

(defun require-package (pkg)
  (unless (package-installed-p pkg)
    (package-install pkg))
  (require pkg))

;;; ----------------------------------------------------------------------------

(defun run-tests ()
  (init-melpa)
  (add-to-list 'load-path (expand-file-name ".."))
  (add-to-list 'load-path (expand-file-name "."))
  (add-to-list 'load-path (expand-file-name "./test"))

  (load-packages)
  (message "%s" load-path)
  (require 'elr)
  (require 'elr-elisp)
  (require 'elr-elisp-tests)
  (ert-run-tests-batch-and-exit nil))

(provide 'test-runner)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; test-runner.el ends here
