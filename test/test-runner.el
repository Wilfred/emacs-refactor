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

(message "Running EMR tests")

;; Set up the environment and run unit tests.
(message "--> Configuring load path...")
(add-to-list 'load-path (expand-file-name "./"))
(add-to-list 'load-path (expand-file-name "test"))

(message "--> Requiring features...")
(require 'ert)
(require 'emr)
(require 'emr-elisp)
(require 'emr-c)

(message "--> Preparing emr...")
(emr-initialize)

(message "--> Loading tests...")
(require 'test-utils)
(require 'emr-elisp-tests)
(require 'emr-c-tests)

(message "--> Running tests...")
(ert-run-tests-batch-and-exit nil)

;; Local Variables:
;; lexical-binding: t
;; no-byte-compile: t
;; End:

;;; test-runner.el ends here
