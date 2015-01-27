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

;; Refactoring commands for C.

;;; Code:

(require 'emr)
(require 's)
(require 'dash)
(require 'thingatpt)
(autoload 'ido-completing-read "ido")
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

; ------------------

(defconst emr-c:rx-include
  (rx "#include" (+ space)
      (group-n 1
               (or "\"" "<") (* (not space)) (or "\"" ">"))))

(defcustom emr-c-implementation-extensions
  '("c" "cpp")
  "Specify extensions that indicate current file is an implementation files."
  :group 'emr)

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


(defvar emr-c:--semantic-tag-list '()
  "[INTERNAL] List of semantic tags in current buffer.")

(defun emr-c:--semantic-fetch-candidates ()
  (setq emr-c:--semantic-tag-list nil)
  (emr-c:--semantic-fetch-candidates-helper (semantic-fetch-tags) 0 nil))

(defun emr-c:--semantic-fetch-candidates-helper (tags depth &optional class)
  "Return a list of pairs '(DISPLAY . REAL), where DISPLAY is the string to be
presented to user, while REAL is a semantic tag.

TAGS are collection of Semantic tags in current buffer.
DEPTH is current recursion depth.
CLASS is the parent class."
  (let ((spaces (make-string (* depth 2) ?\s))
        (class class) cur-type display)
    (cl-dolist (tag tags)
      (when (listp tag)
        (cl-case (setq cur-type (semantic-tag-class tag))
          ((function variable type)
           (let ((type-p (eq cur-type 'type)))
             (unless (and (> depth 0) (not type-p))
               (setq class nil))
             (setq display (concat (if (null class)
                                       spaces
                                     (format "%s(%s) " spaces
                                             (propertize (semantic-format-tag-name (semantic-tag-calculate-parent tag) nil t)
                                                         'semantic-tag
                                                         (semantic-tag-calculate-parent tag))))
                                   (propertize (semantic-format-tag-summarize tag nil t)
                                               'semantic-tag tag)))
             (message "display is %s" display)
             (and type-p
                  (setq class (car tag)))
             (push (cons display tag)
                   emr-c:--semantic-tag-list)
             ;; Recurse to children
             (emr-c:--semantic-fetch-candidates-helper (semantic-tag-components tag)
                                                       (1+ depth)
                                                       class)))
          ;; Don't do anything with packages or includes for now
          ((package include)
           (push (cons (propertize (semantic-format-tag-summarize tag nil t)
                                   'semantic-tag tag)
                       tag)
                 emr-c:--semantic-tag-list))
          ;; Catch-all
          (t))))))

;;;###autoload
(defun emr-c-insert-include (header)
  "Insert an include for HEADER and tidy the includes in the buffer."
  (interactive
   (list
    (if (yes-or-no-p "Library header?")
        (format "<%s>" (ido-completing-read "Header: " emr-c:standard-headers))
      (format "\"%s\"" (ido-completing-read "Header: " (emr-c:headers-in-project))))))

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

;;;###autoload
(defun emr-c:semantic-insert-function-prototype-or-implementation ()
  "Insert function at point as prototype or implementation to
other file (files with same names but different extensions),
depends on file extension. If the file extension is in
emr-c-implementation-extensions, insert \";\"; otherwise, insert
{}. If there is more than one file, prompt for a file. If there's
no file, prompt for the entire projectile project files."
  (interactive)
  (let (file other-files l)
    (if (featurep 'projectile)
        (progn
          (setf other-files
                (projectile-get-other-files (buffer-file-name)
                                            (projectile-current-project-files)
                                            nil))
          (setf l (length other-files))
          (setf file (concat (projectile-project-root)
                             (cond ((> l 1)
                                    (completing-read "Select a file to insert: "
                                                     other-files))
                                   ((= l 1)
                                    (car other-files))
                                   (t (projectile-find-file)))))))
    (senator-copy-tag)
    (with-current-buffer (if (featurep 'projectile)
                             (find-file file)
                           (ff-find-other-file))
      (emr-c:--semantic-fetch-candidates)

      (if emr-c:--semantic-tag-list
          (progn
            (setq emr-c:--semantic-tag-list (nreverse emr-c:--semantic-tag-list))
            (emr-c:--semantic-insert-prototype (cdr (assoc (completing-read "Select a place to insert: "
                                                                            emr-c:--semantic-tag-list)
                                                           emr-c:--semantic-tag-list))))
        (emr-c:--semantic-insert-prototype nil)))))

(defun emr-c:--semantic-insert-prototype (tag)
  "[INTERNAL] Insert a Semantic TAG to current buffer.
If the file extension is in emr-c-implementation-extensions,
insert \";\"; otherwise, insert {}."
  (when tag
    (semantic-go-to-tag tag)
    (goto-char (semantic-tag-end tag))
    (newline 2))

  (senator-yank-tag)
  (indent-according-to-mode)

  (if (member (file-name-extension (buffer-file-name))
              emr-c-implementation-extensions)
      (progn
        (insert "{}")
        (forward-char -1)
        (open-line 1)
        (newline 1)
        (indent-according-to-mode))
    (insert ";")
    (save-excursion
      (forward-line 1)
      (unless (c-guess-empty-line-p)
        (newline 1)))))

;; ------------------

;;; EMR Declarations

(emr-declare-command 'emr-c-tidy-includes
  :title "tidy"
  :description "includes"
  :modes 'c-mode
  :predicate (lambda ()
               (emr-c:looking-at-include?)))

; ------------------

;;;; Minor Mode

;;;###autoload
(defvar emr-c-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "C-c i") 'emr-c-insert-include)
    km)
  "Key map for `emr-c-mode'.")

;;;###autoload
(define-minor-mode emr-c-mode
  "A minor-mode for C that makes extra key bidings available."
  nil " emr" emr-c-mode-map)

(defun emr-c:show-menu ()
  (when (boundp 'c-mode-map)
    (easy-menu-add-item
     nil
     '("EMR")
     ["Insert #include" emr-c-insert-include])))

;;;###autoload
(defun emr-c-initialize ()
  "Initialize EMR in C buffers and enable the EMR menu."
  (add-hook 'c-mode-hook 'emr-c-mode)
  (add-hook 'c-mode-hook 'emr-c:show-menu)
  (--each (buffer-list)
    (with-current-buffer it
      (when (derived-mode-p 'c-mode)
        (emr-c:show-menu)
        (emr-c-mode +1)))))

(provide 'emr-c)

;;; emr-c.el ends here
