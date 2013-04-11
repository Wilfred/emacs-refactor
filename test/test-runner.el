;;; test-runner --- Runs emr tests.

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

;; Runs emr tests.

;;; Code:

(require 'package)

(defconst test-dependencies '(ert popup s dash cl-lib))

;;; ----------------------------------------------------------------------------
;;; Configuration

(defun load-packages ()
  "Install package dependencies."
  (init-melpa)
  (mapc 'require-package test-dependencies))

(defun init-melpa ()
  "Configure package.el to use MELPA and initialize."
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents)))

(defun require-package (pkg)
  "Install package PKG and require it."
  (unless (package-installed-p pkg)
    (package-install pkg))
  (require pkg))

;;; ----------------------------------------------------------------------------

(defun run-tests ()
  "Set up the environment and run unit tests."
  (init-melpa)
  (add-to-list 'load-path (expand-file-name ".."))
  (add-to-list 'load-path (expand-file-name "."))
  (add-to-list 'load-path (expand-file-name "./test"))
  (load-packages)

  (require 'ert)
  (require 'emr)
  (require 'emr-elisp)
  (require 'emr-elisp-tests)
  (ert-run-tests-batch-and-exit nil))

(provide 'test-runner)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; test-runner.el ends here
