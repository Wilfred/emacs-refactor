;;; emr-c.el --- Refactoring commands for C  -*- lexical-binding: t; -*-

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

;; Refactoring commands for C and C-Based modes.

;;; Code:

(require 'emr)
(require 's)
(require 'dash)
(require 'thingatpt)
(autoload 'c-mode-map "cc-mode")
(autoload 'projectile-dir-files "projectile")
(autoload 'projectile-project-p "projectile")

(defvar emr-c:standard-headers
  '("aio.h" "arpa/inet.h" "assert.h" "complex.h" "cpio.h" "ctype.h"
    "curses.h" "dirent.h" "dlfcn.h" "errno.h" "fcntl.h" "fenv.h" "float.h"
    "fmtmsg.h" "fnmatch.h" "ftw.h" "glob.h" "grp.h" "iconv.h" "inttypes.h"
    "iso646.h" "langinfo.h" "libgen.h" "limits.h" "locale.h" "math.h"
    "monetary.h" "mqueue.h" "ndbm.h" "net/if.h" "netdb.h" "netinet/in.h"
    "netinet/tcp.h" "nl_types.h" "poll.h" "pthread.h" "pwd.h" "regex.h"
    "sched.h" "search.h" "semaphore.h" "setjmp.h" "signal.h" "spawn.h"
    "stdalign.h" "stdarg.h" "stdatomic.h" "stdbool.h" "stddef.h" "stdint.h"
    "stdio.h" "stdlib.h" "stdnoreturn.h" "string.h" "strings.h" "stropts.h"
    "sys/ipc.h" "sys/mman.h" "sys/msg.h" "sys/resource.h" "sys/select.h"
    "sys/sem.h" "sys/shm.h" "sys/socket.h" "sys/stat.h" "sys/statvfs.h"
    "sys/time.h" "sys/times.h" "sys/types.h" "sys/uio.h" "sys/un.h"
    "sys/utsname.h" "sys/wait.h" "syslog.h" "tar.h" "term.h" "termios.h"
    "tgmath.h" "threads.h" "time.h" "trace.h" "uchar.h" "ulimit.h"
    "uncntrl.h" "unistd.h" "utime.h" "utmpx.h" "wchar.h" "wctype.h"
    "wordexp.h"))


(defcustom emr-clang-format-style 'Google
  "Style used to format codes with clang.
Refer to http://clang.llvm.org/docs/ClangFormatStyleOptions.html for more
detailed descriptions."
  :type '(radio (const :tag "Format with style suggested by Google." Google)
                (const :tag "Format used by LLVM project." LLVM)
                (const :tag "Format used by Chromium project." Chromium)
                (const :tag "Format used by Mozilla project." Mozilla)
                (const :tag "Format used by Webkit project." WebKit)
                (const :tag "Load style configuration from file." file)
                (repeat :tag "Customized alist." (cons (regexp :tag "Tag")
                                                       (directory :tag "Format"))))

  :group 'emr)

(defvar emr-c-format-fallback-func 'indent-region
  "Function to indent a buffer region.
Will be passed start and end positions of region to be formatted.")

; ------------------

(defconst emr-c:rx-include
  (rx "#include" (+ space)
      (group-n 1
               (or "\"" "<") (* (not space)) (or "\"" ">"))))

(defun emr-c:looking-at-include? ()
  (thing-at-point-looking-at emr-c:rx-include))

(defun emr-c:bob-after-comments ()
  "Move to the first non-comment character in the buffer."
  (goto-char (point-min))
  (while (emr-looking-at-comment?)
    (forward-line 1))
  (point))

(defun emr-c:goto-includes-or-buf-start ()
  (goto-char (point-min))
  (or (search-forward-regexp emr-c:rx-include nil t)
      (emr-c:bob-after-comments))
  (beginning-of-line)
  (point))

;;;###autoload
(defun emr-c-tidy-includes ()
  "Collate and reorder include directives in the current buffer.
Library and project includes are kept separate."
  (interactive "*")
  (let (includes)
    (save-excursion
      (emr-c:goto-includes-or-buf-start)

      ;; Collect include statements in buffer.
      (save-excursion
        (goto-char (point-min))
        (while (search-forward-regexp emr-c:rx-include nil t)
          (push (match-string 1) includes)
          (replace-match "")
          (when (emr-blank-line?)
            (ignore-errors
              (kill-line)))))
      ;; Partition includes by type, subsort alphabetically and insert into
      ;; buffer.
      (->> includes
           (--separate (s-starts-with? "<" it))
           (--map (sort it 'string<))
           (-flatten)
           (--map (concat "#include " it))
           (s-join "\n")
           (s-append "\n")
           (insert)))))

; ------------------

(defun emr-c:headers-in-project ()
  "Return a list of available C header files.

Find header files in the current project.  If this is not a valid
project, return all header files in the current directory."
  (->> (-if-let (proj (projectile-project-p))
         (--map (concat proj it) (projectile-dir-files proj))
         (-> (buffer-file-name) (file-name-directory) (directory-files t)))
    (--filter (-contains? '("h" "hpp") (file-name-extension it)))
    (-map 'file-relative-name)))

;;;###autoload
(defun emr-c-insert-include (header)
  "Insert an include for HEADER and tidy the includes in the buffer."
  (interactive
   (list
    (if (yes-or-no-p "Library header?")
        (format "<%s>" (completing-read "Header: " emr-c:standard-headers))
      (format "\"%s\"" (completing-read "Header: " (emr-c:headers-in-project))))))

  (let ((str (concat "#include " header)))
    (when (s-contains? str (buffer-string))
      (user-error "%s is already included" header))
    (save-excursion
      (atomic-change-group
        (emr-reporting-buffer-changes "Inserted header"
          ;; Insert header.
          (emr-c:goto-includes-or-buf-start)
          (insert str)
          (newline)
          (emr-c-tidy-includes))))))



;;; EMR Declarations

(autoload 'clang-format-region "clang-format" ""  t)
(autoload 'clang-format-buffer "clang-format" ""  t)

(defun emr-clang-available? ()
  "Return whether clang-format is available."
  (and (featurep 'clang-format)
       (executable-find "clang-format")))

(defun emr-cc-get-style ()
  "Return style as a string."
  (cond
   ((stringp emr-clang-format-style) emr-clang-format-style)
   ((listp emr-clang-format-style)
    (concat "{"(mapconcat (lambda (x)
                            (format "%s: %s" (car x) (cdr x)))
                          emr-clang-format-style ", ") "}"))
   ((symbolp emr-clang-format-style) (symbol-name emr-clang-format-style))
   (t nil)))

(defun emr-cc-format-region (start end)
  "Format region (START/END).
Uses either clang-format, if available, or `emr-c-format-fallback-func'."
  (interactive "rp")
  (if (emr-clang-available?)
      (clang-format-region start end (emr-cc-get-style))
    (funcall emr-c-format-fallback-func start end)))

(defun emr-cc-format-buffer ()
  "Format region (START/END).
Uses either clang-format, if available, or `emr-c-format-fallback-func.'"
  (interactive)
  (if (emr-clang-available?)
      (clang-format-buffer (emr-cc-get-style))
    (funcall emr-c-format-fallback-func (point-min) (point-max))))

(defalias 'emr-cc-tidy-includes 'emr-c-tidy-includes)

(defvar emr-cc-surround-var-hist nil
  "A collection of variables used by if-defs..")

(defun emr-cc-surround-if-end (start end)
  "Surround region between START & END with if-def."
  (interactive "rp")
  (let ((content (buffer-substring-no-properties start end))
        (var (completing-read "Variable Name: " emr-cc-surround-var-hist
                              nil nil nil 'emr-cc-surround-var-hist)))
    (save-excursion
      (delete-region start end)
      (insert (format "#ifdef %s\n" var))
      (insert content)
      (insert (format "\n#endif /*%s*/" var))
      (emr-cc-format-region start (point)))))

(defun emr-cpp-try-catch (start end)
  "Surround region between START & END with try-catch."
      (interactive "rp")
    (let ((content (buffer-substring-no-properties start end)))
      (save-excursion
        (delete-region start end)
        (insert "try {\n")
        (insert content)
        (insert
         "}\ncatch (exception& e) {\n")
        (insert "throw ;\n}\n")
        (emr-cc-format-region start (point)))))

(defun emr-region-active? ()
  "Return t if a valid region is active."
  (and mark-active (not (equal (mark) (point)))))
(defun emr-region-inactive? ()
  "Return nil if a valid region is active."
  (not (emr-region-active?)))

; ------------------

;;; EMR Declarations

(emr-declare-command 'emr-cc-tidy-includes
  :title "tidy"
  :description "includes"
  :modes '(c++-mode c-mode)
  :predicate (lambda ()
               (emr-c:looking-at-include?)))

(emr-declare-command 'emr-cc-format-region
  :title "format region"
  :description (if (emr-clang-available?)
                   "with clang"
                 "with the value of emr-c-format-fallback-func")
  :modes '(c-mode c++-mode)
  :predicate 'emr-region-active?)

(emr-declare-command 'emr-cc-format-buffer
  :title "format buffer"
  :description (if (emr-clang-available?)
                   "with clang"
                 "with the value of emr-c-format-fallback-func")
  :modes '(c-mode c++-mode)
  :predicate 'emr-region-inactive?)

(emr-declare-command 'emr-cc-surround-if-end
  :title "surround"
  :description "with if-endif"
  :modes '(c++-mode c-mode)
  :predicate 'emr-region-active?)

(emr-declare-command 'emr-cpp-try-catch
  :title "surround"
  :description "with try-catch"
  :modes '(c++-mode)
  :predicate 'emr-region-active?)

(emr-declare-command 'emr-c-insert-include
  :title "insert header"
  :description "#include"
  :modes '(c-mode)
  :predicate (lambda () t))

;; ------------------

(defun emr-c:show-menu ()
  (when (boundp 'c-mode-map)
    (easy-menu-add-item
     nil
     '("EMR")
     ["Insert #include" emr-c-insert-include])))

;;;###autoload
(defun emr-c-initialize ()
  "Initialize EMR in C buffers and enable the EMR menu."
  (add-hook 'c-mode-hook 'emr-c:show-menu)
  (--each (buffer-list)
    (with-current-buffer it
      (when (derived-mode-p 'c-mode)
        (emr-c:show-menu)))))

(provide 'emr-c)

;;; emr-c.el ends here
