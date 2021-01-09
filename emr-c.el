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

;;; include guards

(defun emr-cc-basename-include-guard ()
  "Derive an include guard from the buffer's basename.
All non-identifier characters of either the buffer's filename if
available or the buffer's name are replaced by underscores, and
the result is upcased.

E.g. foo.h -> FOO_H."
  (let ((name (or (-some-> (buffer-file-name) file-name-nondirectory)
                  (buffer-name))))
    (upcase (replace-regexp-in-string
             (rx (or (not (any alpha "_")) (: bos digit)))
             "_" name t t))))

(defun emr-cc-basename-included-guard ()
  "Like `emr-cc-basename-include-guard', but ends in _INCLUDED."
  (concat (emr-cc-basename-include-guard) "_INCLUDED"))

(defcustom emr-cc-include-guard-style #'emr-cc-include-guard
  "Function used to derive the include guard symbol.
It will be called with no arguments in the context of the buffer
where `emr-cc-add-include-guard' is called and must return a
string to be used as include guard."
  :type '(choice
          (const emr-cc-basename-include-guard)
          (const emr-cc-basename-included-guard)
          (function :tag "custom function"))
  :group 'emr-cc)

(defcustom emr-cc-include-guard-value nil
  "What include guards are #defined to, or nil.
If this is non-nil, `emr-cc-add-include-guard' will insert this
string after \"#define <symbol> \" (note the space)."
  :type '(choice string (const nil))
  :group 'emr-cc)

(defcustom emr-cc-include-guard-space nil
  "Whether there should be a space after #.
If this is t, `emr-cc-add-include-guard' will insert a space
after the # of #define, #ifndef and #endif. This variable may
also be a string, in which case that is inserted instead."
  :type '(choice
          (const :tag "Insert a space after #" t)
          (const :tag "Don't insert space after #" nil)
          (string :tag "Insert after #:"))
  :group 'emr-cc)

(defun emr-cc-include-guard-suffix-c89 (guard)
  "Insert the include GUARD wrapped in a c89-style comment."
  (format "/* %s */" guard))

(defun emr-cc-include-guard-suffix-comment (guard)
  "Insert the include GUARD wrapped in a // comment."
  (format "// %s" guard))

(defcustom emr-cc-include-guard-suffix nil
  "Function to determine the text after #endif.
If this is non-nil, `emr-cc-add-include-guard' will insert the
result of calling this function with the include guard (see
`emr-cc-include-guard-style') as only argument and insert its
result, if non-nil, after \"#endif \" (note the space). If this
variable is a string, it will be `format'ted with the include
guard as second argument and inserted the same way as if it were
a function."
  :type '(choice (const :tag "/* GUARD */" emr-cc-include-guard-suffix-c89)
                 (const :tag "// GUARD" emr-cc-include-guard-suffix-comment)
                 (const :tag "No #endif suffix" nil)
                 (function :tag "custom function")
                 (string :tag "Format string (one argument)")))

(defun emr-cc--include-guard-space (variable)
  "Resolve a # spacing VARIABLE.
VARIABLE is the value of the # spacing variable, for example the
value of `emr-cc-include-guard-space'."
  (pcase variable
    ((pred stringp) variable)
    (`nil "")
    (_ " ")))

(defun emr-cc--beginning-of-header ()
  "Go to the start of the source file, skipping comments."
  (goto-char (point-min))
  ;; Skip comment(s) and whitespace (e.g. license header)
  (let ((parse-sexp-ignore-comments t))
    (with-syntax-table (copy-syntax-table (syntax-table))
      (modify-syntax-entry ?\\ " ")
      (forward-sexp)))
  (beginning-of-line))

(defun emr-cc--end-of-header ()
  "Go to bol at the end of the source file, skipping comments."
  (goto-char (point-max))
  (let ((parse-sexp-ignore-comments t))
    (backward-sexp)
    (forward-sexp))
  (beginning-of-line))

(defun emr-cc-add-include-guard ()
  "Add an include guard to the current buffer."
  (interactive)
  (let ((guard (funcall emr-cc-include-guard-style)))
    (save-excursion
      (emr-cc--beginning-of-header)
      (insert
       (format
        "#%3$sifndef %1$s
#%3$sdefine %1$s%2$s

"
        guard (or emr-cc-include-guard-value "")
        (emr-cc--include-guard-space emr-cc-include-guard-space)))
      (emr-cc--end-of-header)
      (forward-line)
      (unless (= (char-before) ?\n)
        (insert ?\n))
      (insert
       (format
        "\n#%sendif%s"
        (emr-cc--include-guard-space emr-cc-include-guard-space)
        (or (-some->>
                (-some-> emr-cc-include-guard-suffix
                  (cl-etypecase
                      (string (format emr-cc-include-guard-suffix guard))
                    (function (funcall emr-cc-include-guard-suffix guard))))
              (concat " "))
            ""))))))

(defun emr-cc--looking-at-include-guard ()
  "`looking-at' the include guard of the buffer, if it has one."
  (save-excursion
    (emr-cc--beginning-of-header)
    (looking-at
     (rx bol (* space) "#" (* space) "ifndef" (* space) (group (+ any) symbol-end) (* any) "\n"
         bol (* space) "#" (* space) "define" (* space) (backref 1) symbol-end (* any) "\n"
         (? "\n")))))

(defun emr-cc-delete-include-guard ()
  "Remove the current buffer's include guard.
Return non-nil if an include guard was actually removed."
  (interactive)
  (save-match-data
    (when (emr-cc--looking-at-include-guard)
      (replace-match "")

      (save-excursion
        (emr-cc--end-of-header)
        (when (looking-at (rx bol "#" (* space) "endif" symbol-end (* any) eol))
          (replace-match "")
          ;; We can't know if the file should end in a newline, so don't delete
          ;; yet another newline (even if it was inserted by
          ;; `emr-cc-add-include-guard'). User's own whitespace management
          ;; solutions (e.g. `ws-butler') can fix this.
          (when (eq (char-before) ?\n)
            (delete-char -1))))
      ;; Success, even if there was no #endif.
      t)))

(defcustom emr-cc-pragma-once-space nil
  "`emr-cc-include-guard-space', but for #pragma once."
  :type '(choice
          (const :tag "Insert a space after #" t)
          (const :tag "Don't insert space after #" nil)
          (string :tag "Insert after #:"))
  :group 'emr-cc)

(defun emr-cc-add-pragma-once ()
  "Add #pragma once."
  (interactive)
  (save-excursion
    (emr-cc--beginning-of-header)
    (insert (format "#%spragma once\n\n"
                    (emr-cc--include-guard-space emr-cc-pragma-once-space)))))

(defun emr-cc--looking-at-pragma-once ()
  (save-excursion
    (emr-cc--beginning-of-header)
    (looking-at (rx bol (* space) "#" (* space) "pragma" (* space) "once" (* any) (** 0 2 ?\n)))))

(defun emr-cc-delete-pragma-once ()
  "Remove #pragma once.
Return non-nil if a #pragma once was removed."
  (interactive)
  (save-match-data
    (when (emr-cc--looking-at-pragma-once)
      (replace-match "")
      t)))

(defun emr-cc-toggle-include-guard ()
  "Toggle between #pragma once and include guards."
  (interactive)
  (if (emr-cc-delete-pragma-once)
      (emr-cc-add-include-guard)
    (unless (emr-cc-delete-include-guard)
      (user-error "Current buffer contains neither an include guard nor #pragma once"))
    (emr-cc-add-pragma-once)))


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


(defun emr-cc--has-include-guard? ()
  "Check if there is an include guard or #pragma once."
  (save-match-data
    (or (emr-cc--looking-at-include-guard)
        (emr-cc--looking-at-pragma-once))))

(defun emr-cc--need-include-guard? ()
  "Check if the buffer has no include guard or #pragma once."
  (not (emr-cc--has-include-guard?)))

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

(emr-declare-command 'emr-cc-add-include-guard
  :title "add include guard"
  :description "#ifndef X #define X... #endif"
  :modes '(c-mode c++-mode)
  :predicate #'emr-cc--need-include-guard?)

(emr-declare-command 'emr-cc-delete-include-guard
  :title "remove include guard"
  :description "remove #ifndef X #define X... #endif"
  :modes '(c-mode c++-mode)
  :predicate (lambda ()
               (save-match-data
                 (emr-cc--looking-at-include-guard))))

(emr-declare-command 'emr-cc-add-pragma-once
  :title "add #pragma once"
  :description "#pragma once"
  :modes '(c-mode c++-mode)
  :predicate #'emr-cc--need-include-guard?)

(emr-declare-command 'emr-cc-delete-pragma-once
  :title "remove #pragma once"
  :description "remove #pragma once"
  :modes '(c-mode c++-mode)
  :predicate (lambda ()
               (save-match-data
                 (emr-cc--looking-at-pragma-once))))

(emr-declare-command 'emr-cc-toggle-include-guard
  :title "toggle include guard"
  :description "toggle between #pragma once and an include guard"
  :modes '(c-mode c++-mode)
  :predicate #'emr-cc--has-include-guard?)

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
